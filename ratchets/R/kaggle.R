#' Connect to the Kaggle SQLite database.
#'
#' @param sqlite_location Location of Kaggle sqlite database. Download this file
#'   from \url{https://www.kaggle.com/kaggle/meta-kaggle/downloads/database.sqlite.zip},
#'   unzip it, and put it wherever you'd like. By default, the file is expected
#'   to be found in the "evoteams" package at "inst/extdata/kaggle.sqlite".
#'
#' @examples
#' kaggle_db <- connect_kaggle("path/to/kaggle.sqlite")
#' kaggle_db <- connect_kaggle()  # assumes db included in evoteams pkg
#'
#' @export
connect_kaggle <- function(sqlite_location) {
  if (missing(sqlite_location)) {
    sqlite_location <- system.file("extdata/kaggle.sqlite", package = "evoteams")
  }
  if (!file.exists(sqlite_location)) stop("kaggle.sqlite not found")

  dplyr::src_sqlite(sqlite_location)
}


#' Get all submissions to all Kaggle competitions.
#'
#' Simplifies the Submissions table of the Kaggle db.
#'
#' @param team_competitions Used to label competition ids in submissions.
#'    If not provided, team competitions will be generated from the provided
#'    Kaggle db.
#' @param with_predicted_place Set to true and/or provide leaderboards to
#'    augment submissions with a PredictedPlace column based on final
#'    standings.
#'
#' @import dplyr
#' @import magrittr
#' @export
get_submissions <- function(kaggle_db, team_competitions,
                            with_predicted_place = TRUE) {
  if (missing(kaggle_db)) stop("must provide connection to kaggle db")

  if (missing(team_competitions)) {
    team_competitions <- get_team_competitions(kaggle_db)
  }

  submissions <- kaggle_db %>%
    tbl("Submissions") %>%
    select(TeamId, DateSubmitted, Score = PublicScore) %>%
    # Break DBI to allow normal data frame manipulation
    as_data_frame() %>%
    mutate(DateSubmitted = lubridate::ymd_hms(DateSubmitted)) %>%
    group_by(TeamId) %>%
    arrange(DateSubmitted) %>%
    mutate(SubmissionNum = 1:n()) %>%
    ungroup() %>%
    # Label competition id
    left_join(team_competitions)

  if (with_predicted_place) {
    leaderboards <- make_leaderboards(kaggle_db, submissions,
                                      with_submission_intervals = FALSE,
                                      with_relative_submissions = FALSE,
                                      with_competition_intervals = FALSE,
                                      label_team_types = FALSE)
    submissions %<>% predict_place(leaderboards)
  }

  submissions
}


#' Create Kaggle leaderboards from submissions.
#'
#' Provide either submissions or a connection to the Kaggle db.
#'
#' @examples
#' submissions <- get_submissions(kaggle_db)
#' leaderboards <- make_leaderboards(submissions)
#' leaderboards <- make_leaderboards(kaggle_db = kaggle_db)
#'
#' @param kaggle_db sqlite connector. Required unless submissions and team sizes
#'    are both provided.
#' @param submissions data frame of Kaggle submissions. If submissions aren't
#'    provided, they will be created.
#' @param team_sizes data frame map of Kaggle team ids to team sizes. Required
#'    if connection to Kaggle db is not provided.
#' @param with_submission_intervals Logical. Should submission intervals
#'    be calculated? Defaults to true. Calls
#'    \code{\link{calculate_submission_intervals}} to create the intervals.
#' @param label_team_types Should submissions on leaderboards be assigned
#'    team types in one of the four quadrants of the number submissions by
#'    total time grid. See \code{\link{determine_team_types}}.
#'
#' @import dplyr
#' @import magrittr
#' @export
make_leaderboards <- function(kaggle_db, submissions, team_sizes,
                              with_submission_intervals = TRUE,
                              with_competition_intervals = TRUE,
                              with_relative_submissions = TRUE,
                              label_team_types = TRUE) {
  if (missing(kaggle_db) & any(missing(submissions), missing(team_sizes))) {
    stop("must provide kaggle db or all required data frames")
  }

  # Only create the data frames that weren't provided
  if (missing(submissions)) submissions <- get_submissions(kaggle_db)
  if (missing(team_sizes)) team_sizes <- get_team_sizes(kaggle_db)

  leaderboards <- submissions %>%
    # Select the final submission for each team
    group_by(TeamId) %>%
    filter(SubmissionNum == max(SubmissionNum)) %>%
    rename(TotalSubmissions = SubmissionNum) %>%
    ungroup() %>%
    # Assign places for each competition
    group_by(CompetitionId) %>%
    arrange(desc(Score)) %>%  # assumes bigger scores are better
    mutate(Place = 1:n()) %>%
    ungroup() %>%
    left_join(team_sizes) %>%
    as_data_frame()

  if (with_submission_intervals) {
    submission_intervals <- calculate_submission_intervals(submissions)
    leaderboards %<>% left_join(submission_intervals)
  }

  if (with_competition_intervals) {
    if (missing(kaggle_db)) stop("kaggle db required to get competition intervals")
    competition_intervals <- get_competition_intervals(kaggle_db)
    leaderboards %<>%
      left_join(competition_intervals) %>%
      mutate(PropCompetitionTime = TotalTimeSec/CompetitionDurationSec)
  }

  if (with_relative_submissions) {
    leaderboards %<>% calculate_relative_submissions()
  }

  if (label_team_types) {
    leaderboards %<>% divide_into_team_types()
  }

  leaderboards
}


#' Create a map of team ids to competition ids.
#' @import dplyr
#' @export
get_team_competitions <- function(kaggle_db) {
  kaggle_db %>%
    tbl("Teams") %>%
    select(TeamId = Id, CompetitionId) %>%
    as_data_frame()
}


#' Calculate Kaggle team sizes.
#' @import dplyr
#' @export
get_team_sizes <- function(kaggle_db) {
  kaggle_db %>%
    tbl("TeamMemberships") %>%
    select(TeamId, UserId) %>%
    count(TeamId) %>%
    rename(TeamSize = n) %>%
    as_data_frame()
}


#' Get competition start and end times.
#' @import dplyr
#' @export
get_competition_intervals <- function(kaggle_db) {
  kaggle_db %>%
    tbl("Competitions") %>%
    as_data_frame() %>%
    mutate(
      CompetitionStart = lubridate::ymd_hms(DateEnabled),
      CompetitionEnd = lubridate::ymd_hms(Deadline),
      CompetitionDuration = interval_duration(CompetitionStart, CompetitionEnd),
      CompetitionDurationSec = as.numeric(CompetitionDuration)
    ) %>%
    select(CompetitionId = Id, CompetitionDuration, CompetitionDurationSec)
}


#' Label predicted place.
#' @import dplyr
#' @export
predict_place <- function(submissions, leaderboards) {
  determine_breaks <- function(competition_id) {
    leaderboards %>%
      filter(CompetitionId == competition_id) %>%
      .$Score %>%
      unique %>%
      c(-Inf, ., Inf)
  }

  predict_place <- function(scores, competition_id) {
    cut(rev(scores), breaks = determine_breaks(competition_id), labels = FALSE)
  }

  submissions %>%
    group_by(CompetitionId) %>%
    mutate(PredictedPlace = predict_place(Score, CompetitionId[[1]]))
}


#' Compute time interval between first and last submissions.
#' @import dplyr
#' @export
calculate_submission_intervals <- function(submissions) {
  submissions %>%
    group_by(TeamId) %>%
    summarize(
      FirstSubmissionTime = min(DateSubmitted),
      LastSubmissionTime = max(DateSubmitted)
    ) %>%
    mutate(
      TotalTime = interval_duration(FirstSubmissionTime, LastSubmissionTime),
      TotalTimeSec = as.numeric(TotalTime)
    )
}


#' Cut total submissions into bins and label with the middle value.
#' @import dplyr
#' @export
label_submission_bins <- function(leaderboards, submission_bin_width = 10) {
  breaks <- seq(1, max(leaderboards$TotalSubmissions) + 1, by = submission_bin_width)
  leaderboards %>%
    mutate(TotalSubmissionsBin = cut_and_label_bin_means(TotalSubmissions, breaks))
}


#' Cut submission intervals into bins and label with the middle time value.
#' @param interval_bin_width Number of seconds to include in the bin. Defaults
#'    to one day.
#' @import dplyr
#' @export
label_submission_interval_bins <- function(leaderboards, interval_bin_width) {
  if (missing(interval_bin_width)) interval_bin_width <- lubridate::ddays(1) %>% as.numeric
  breaks <- seq(0, max(leaderboards$TotalTimeSec) + 1, by = interval_bin_width)
  leaderboards %>%
    mutate(SubmissionIntervalBin = cut_and_label_bin_means(TotalTimeSec, breaks))
}


#' Cut values into bins based on breaks and label with numeric bin means.
#' @import dplyr
#' @export
cut_and_label_bin_means <- function(values, breaks) {
  cut(values, breaks=breaks, labels=label_bin_breaks(breaks), right=FALSE) %>%
    as.character() %>% as.numeric()  # factor -> character -> numeric
}


#' Calculate the mean between bin min and bin max.
#' @import dplyr
#' @export
label_bin_breaks <- function(breaks) {
  cbind(break_min = breaks, break_max = lead(breaks, n = 1) - 1) %>%
    as_data_frame %>%
    head(-1) %>%  # drop last row containing open interval
    mutate(break_means = rowMeans(.[, c("break_min", "break_max")])) %>%
    .$break_means
}


#' Create a ggplot2 scale object for time variable in seconds.
#' @import ggplot2
#' @export
make_time_scale <- function(name, breaks_days,
                            scale_obj = "scale_y_continuous", ...) {
  breaks_sec <- breaks_days * 24 * 3600
  get(scale_obj)(name, breaks = breaks_sec, labels = breaks_days, ...)
}


#' Label different place groups, i.e., FirstPlaceTeam.
#' @export
label_place_groups <- function(frame) {
  dplyr::mutate(frame, FirstPlaceTeam = (Place == 1))
}


#' Summarize team properties in each place.
#'
#' Each row is a place. For example, the row for Place == 1
#' describes the average number of TotalSubmissions and average
#' TeamSize for all teams that finished in this place.
#'
#' @examples
#' leaderboards %>% filter(Place <= 100) %>% summarize_by_place()
#'
#' @import dplyr
#' @export
summarize_by_place <- function(leaderboards) {
  leaderboards %>%
    group_by(Place) %>%
    summarize(
      TotalSubmissions = mean(TotalSubmissions),
      SubmissionsToFirstPlace = mean(SubmissionsToFirstPlace),
      TeamSize = mean(TeamSize, na.rm = TRUE),
      TotalTime = mean(TotalTime),
      PropCompetitionTime = mean(PropCompetitionTime, na.rm = TRUE)
    ) %>%
    label_place_groups()
}


#' Summarize team performance in a group.
#'
#' This functional sequence is meant to be passed a grouped data frame.
#'
#' @examples
#' leaderboards %>% group_by(TeamSize) %>% summarize_teams_in_group()
#'
#' @import dplyr
#' @export
summarize_teams_in_group <- function(grouped) {
  grouped %>%
    summarize(
      Place = mean(Place),
      TotalTime = mean(TotalTime),
      PropCompetitionTime = mean(PropCompetitionTime),
      NTeams = n()
    ) %>%
    mutate(
      PercentTeams = NTeams/sum(NTeams),
      PercentTeamsLabel = scales::percent(PercentTeams)
    )
}


#' Get model predictions for a model predicting Place from TotalSubmissions.
#'
#' @import dplyr
#' @export
get_place_mod_preds <- function(mod, predict_fn, x_preds) {
  if (missing(x_preds)) x_preds <- data_frame(TotalSubmissions = 1:200)
  if (missing(predict_fn)) predict_fn <- predict

  x_preds %>%
    cbind(., predict_fn(mod, ., se = TRUE)) %>%
    rename(Place = fit, SE = se.fit) %>%
    mutate(TotalSubmissionsBin = TotalSubmissions)  # for consistency with summary
}


#' Transform submissions to be relative to the first place team.
#'
#' Requires first place team to be labeled (FirstPlaceTeam columnn).
#' See \code{\link{label_place_groups}}.
#'
#' @import dplyr
#' @export
calculate_relative_submissions <- function(leaderboards) {
  leaderboards %>%
    label_place_groups() %>%
    group_by(CompetitionId) %>%
    mutate(
      FirstPlaceTeamSubmissions = TotalSubmissions[FirstPlaceTeam == TRUE],
      SubmissionsToFirstPlace = TotalSubmissions - FirstPlaceTeamSubmissions
    ) %>%
    ungroup()
}


interval_duration <- function(start, end) {
  lubridate::interval(start, end) %>% lubridate::as.duration()
}


#' Assign team types by median splits on time and submissions.
#'
#' See also \code{\link{recode_team_type}}.
#'
#' @import dplyr
#' @export
divide_into_team_types <- function(leaderboards) {

  # Augment team type map to merge based on TimeSplit and SubmissionSplit
  team_type_map <- recode_team_type() %>%
    mutate(
      TimeSplit = c(1, 0, 0, 1),
      SubmissionsSplit = c(1, 1, 0, 0)
    )

  # Remove any team type columns already in leaderboards
  for(map_col in colnames(team_type_map)) {
    if(map_col %in% colnames(leaderboards)) leaderboards[map_col] <- NULL
  }

  median_split <- function(x) as.numeric(x < median(x))

  leaderboards %>%
    mutate(
      TimeSplit = median_split(TotalTime),
      SubmissionsSplit = median_split(TotalSubmissions)
    ) %>%
    left_join(team_type_map) %>%
    select(-TimeSplit, -SubmissionsSplit)  # only needed for merge
}

#' Recode team type for labels and contrast coding.
#'
#' If no data is provided, the team type map is returned.
#'
#' @import dplyr
#' @export
recode_team_type <- function(frame) {
  team_type_levels <- c("steady", "long", "short", "rapid")

  contr_rel_short <- contr.treatment(n = length(team_type_levels), base = 3) %>%
    as_data_frame()
  names(contr_rel_short) <- c("ShortVSteady", "ShortVLong", "ShortVRapid")
  contr_rel_short$TeamType <- team_type_levels

  team_type_map <- data_frame(
    TeamType = team_type_levels,
    TeamLabel = factor(team_type_levels, levels = team_type_levels),
    TeamNum = seq_along(team_type_levels)
  ) %>%
    left_join(contr_rel_short)

  if (missing(frame)) return(team_type_map)

  left_join(frame, team_type_map)
}
