source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- teamsize-from-place
ggplot(top_100_places, aes(Place, TeamSize)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_team_size +
  scale_color_manual(values = c(colors[["team_size"]], colors[["first_place"]])) +
  coord_cartesian(xlim = c(1, 100), ylim = c(1:3)) +
  base_theme

# ---- place-from-teamsize
gg_place_from_teamsize <- ggplot(top_100_by_team_size, aes(TeamSize, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["team_size"]]) +
  scale_x_continuous("team size", breaks = c(1, seq(10, 40, by = 10))) +
  scale_y_place +
  scale_size_continuous("Teams", labels = percent) +
  base_theme +
  theme(
    legend.position = "bottom"
  )

place_mod <- lm(Place ~ TotalSubmissions + TeamSize, data = top_100)

max_team_size <- 5
x_preds <- expand.grid(TotalSubmissions = mean(top_100$TotalSubmissions),
                       TeamSize = 1:max_team_size)
y_preds <- predict(place_mod, x_preds, se = TRUE)
preds <- cbind(x_preds, y_preds) %>%
  rename(Place = fit, SE = se.fit)

gg_place_from_teamsize +
  geom_smooth(aes(ymin = Place - SE, ymax = Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]])

# ---- place-from-teamsize-zoom
gg_place_from_teamsize %+% filter(top_100_by_team_size, TeamSize <= max_team_size) +
  geom_smooth(aes(ymin = Place - SE, ymax = Place + SE), data = preds,
              stat = "identity", color = colors[["orange"]]) +
  scale_x_continuous("team size", breaks = 1:max_team_size) +
  geom_text(aes(label = PercentTeamsLabel), nudge_y = 1.2, size = 3) +
  guides(size = "none")
