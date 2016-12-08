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
