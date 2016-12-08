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
