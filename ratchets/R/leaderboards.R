#' Create Kaggle leaderboards from submissions.
#'
#' Provide either submissions or a connection to the Kaggle db.
#'
#' @examples
#' kaggle_db <- connect_kaggle()
#' leaderboards <- make_leaderboards(kaggle_db)
#'
#' @param kaggle_db sqlite connector. Required unless submissions and team sizes
#'    are both provided.
#' @param submissions data frame of Kaggle submissions. If submissions aren't
#'    provided, they will be created.
#' @param team_sizes data frame map of Kaggle team ids to team sizes. Required
#'    if connection to Kaggle db is not provided.
#' @param with_submission_intervals Logical. Should submission intervals
#'    be calculated? Defaults to TRUE. Calls
#'    \code{\link{calculate_submission_intervals}} to create the intervals.
#' @param with_competition_intervals Logical. Should start and end time
#'    intervals be calculated for each competition/ Defaults to TRUE.
#'    Calls \code{\link{get_competition_intervals}} to calculate the
#'    competition intervals.
#' @param with_relative_submissions Logical. Should an additional column
#'    be created calculating the number of submissions made for each team
#'    relative to the team that took first place? Defaults to TRUE.
#'    Calls \code{\link{calculate_relative_submissions}} to calculate
#'    the relative submissions.
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
