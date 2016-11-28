# ---- theme
# Theme for plots and other R outputs
library(ggplot2)

base_theme <- theme_minimal() +
  theme(
    legend.position = "none",
    axis.ticks = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 12)
  )

# Colors
colors <- RColorBrewer::brewer.pal(4, "Set2")
names(colors) <- c("green", "orange", "blue", "pink")
colors["first_place"] <- colors[["orange"]]
colors["submissions"] <- colors[["blue"]]
colors["team_size"] <- colors[["green"]]
team_type_colors <- unname(colors[c("green", "blue", "pink", "orange")])

default_alpha <- 0.6

# Scales
scale_x_place <- scale_x_continuous("place", breaks = c(1, seq(10, 100, by = 10)))
scale_y_place <- scale_y_reverse("place", breaks = c(1, seq(10, 100, by = 10)))

scale_x_team_size <- scale_x_continuous("team size", breaks = 1:4)
scale_y_team_size <- scale_y_continuous("team size", breaks = 1:4)

scale_x_total_submissions <- scale_x_continuous(
  "total submissions", breaks = c(1, seq(100, 600, by = 100)))
scale_x_submission_number <- scale_x_continuous(
  "submission number", breaks = c(1, seq(100, 600, by = 100)))
scale_y_submissions <- scale_y_continuous(
  "total submissions", breaks = c(1, seq(5, 100, by = 5)))

scale_y_total_time <- evoteams::make_time_scale("submission interval (days)",
                                                seq(0, 200, by = 20))

# Limits
top_100_submissions_ylim <- c(1, 39)
top_100_places_xlim <- c(1, 100)

# Team type colors
team_types <- recode_team_type()
scale_x_team_label <- scale_x_discrete("", labels = team_types$TeamLabel)
scale_x_team_num <- scale_x_continuous("", breaks = team_types$TeamNum,
                                       labels = team_types$TeamLabel)
scale_fill_team_type <- scale_fill_manual(values = team_type_colors)
scale_color_team_type <- scale_color_manual(values = team_type_colors)