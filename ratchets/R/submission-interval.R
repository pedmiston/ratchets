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


interval_duration <- function(start, end) {
  lubridate::interval(start, end) %>% lubridate::as.duration()
}
