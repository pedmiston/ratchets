source("docs/kaggle/R/setup.R")
source("docs/kaggle/R/theme.R")

# ---- correlation
gg_correlation <- ggplot(top_100, aes(TotalSubmissions, TotalTimeSec)) +
  geom_point(shape = 1, alpha = 0.2) +
  scale_x_total_submissions +
  make_time_scale("submission interval (days)", seq(0, 400, by = 50)) +
  coord_cartesian(
    xlim = c(1, 300),
    ylim = ddays(c(0, 200))
  ) +
  base_theme

gg_correlation

# ---- correlation-by-place
ggplot(top_100_places, aes(TotalSubmissions, TotalTime)) +
  geom_text(aes(label = Place, color = FirstPlaceTeam, alpha = Place), size = 2,
            check_overlap = TRUE) +
  scale_x_continuous("submissions", breaks = c(1, seq(5, 100, by = 5))) +
  make_time_scale("submission interval (days)", breaks_days = seq(0, 100, by = 5)) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1, 0.2)) +
  base_theme

# ---- interaction-mod
interaction_mod <- lm(Place ~ TotalSubmissionsZ * TotalTimeZ, data = top_100)
