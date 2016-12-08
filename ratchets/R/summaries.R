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
