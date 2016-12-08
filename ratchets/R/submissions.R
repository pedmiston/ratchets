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


#' Create a map of team ids to competition ids.
#' @import dplyr
#' @export
get_team_competitions <- function(kaggle_db) {
  kaggle_db %>%
    tbl("Teams") %>%
    select(TeamId = Id, CompetitionId) %>%
    as_data_frame()
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
