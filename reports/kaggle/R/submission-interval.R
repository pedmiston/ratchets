source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- submission-interval-per-place
ggplot(top_100_places, aes(Place, TotalTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  make_time_scale("submission interval (days)", breaks_days = seq(0, 30, by = 5)) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 30 * 24 * 3600)) +
  base_theme +
  labs(title = "Consistent submission intervals across places")

# ---- prop-time-per-place
ggplot(top_100_places, aes(Place, PropCompetitionTime)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_continuous("proportion time used", labels = percent) +
  scale_color_manual(values = c(colors[["green"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = c(0, 1)) +
  base_theme

# ---- place-from-submission-interval-mod
z_score <- function(x) (x - mean(x))/sd(x)
top_100$TotalTimeSecZ <- z_score(top_100$TotalTimeSec)
interval_mod <- lmer(Place ~ TotalTimeSecZ + (TotalTimeSecZ|CompetitionId),
                     data = top_100)

# ---- place-from-submission-interval
gg_place_from_interval <- ggplot(top_100_by_interval_bin, aes(SubmissionIntervalBin, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  make_time_scale("submission interval (days)", c(1, seq(20, 200, by = 20)),
                  scale_obj = "scale_x_continuous") +
  base_theme +
  theme(legend.position = "bottom")
gg_place_from_interval
