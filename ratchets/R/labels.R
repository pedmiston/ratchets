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


#' Label different place groups, i.e., FirstPlaceTeam.
#' @export
label_place_groups <- function(frame) {
  dplyr::mutate(frame, FirstPlaceTeam = (Place == 1))
}
