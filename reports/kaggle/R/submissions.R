source("reports/kaggle/R/setup.R")
source("reports/kaggle/R/theme.R")

# ---- submissions-per-place
gg_submissions_per_place <- ggplot(top_100_places, aes(Place, TotalSubmissions)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha) +
  scale_x_place +
  scale_y_submissions +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim) +
  base_theme +
  labs(title = "Top place teams make more submissions")
gg_submissions_per_place

# ---- relative-submissions-per-place
gg_relative_submissions_per_place <- ggplot(top_100_places, aes(Place, SubmissionsToFirstPlace)) +
  geom_point(aes(color = FirstPlaceTeam), alpha = default_alpha,
             stat = "summary", fun.y = "mean") +
  scale_x_place +
  scale_y_continuous("submissions\nrelative to first place team",
                     breaks = seq(-100, 10, by = 5)) +
  scale_color_manual(values = c(colors[["submissions"]], colors[["first_place"]])) +
  coord_cartesian(xlim = top_100_places_xlim, ylim = top_100_submissions_ylim - 40) +
  base_theme +
  labs(title = "Top place teams make more submissions")
gg_relative_submissions_per_place

# ---- place-from-submissions-mod
place_mod <- glmer(Place ~ TotalSubmissions + (TotalSubmissions|CompetitionId),
                   family = "poisson", data = top_100)

# ---- place-from-submissions
gg_place_from_submissions <- ggplot(top_100_by_submission_bin, aes(TotalSubmissionsBin, Place)) +
  geom_point(aes(size = PercentTeams), alpha = default_alpha,
             color = colors[["submissions"]]) +
  scale_x_total_submissions +
  scale_y_place +
  scale_size_continuous("proportion of teams", labels = percent, breaks = rev(c(0.01, 0.05, 0.15, 0.6))) +
  coord_cartesian(xlim = c(1, 600), ylim = c(1, 100)) +
  base_theme +
  theme(legend.position = "bottom") +
  ggtitle("Making more submissions improves place")

place_preds <- get_place_mod_preds(place_mod, predict_fn = predictSE)
# The predictions for this hierarchical place mod do not align with means,
# indicating that there are large differences between competitions.
# The conclusions are the same, but the plot looks weird. In addition
# to showing the hierarchical model preds, here I'm also showing
# the predictions of a simple linear model.
place_mod_lm <- lm(Place ~ TotalSubmissions, data = top_100)
place_mod_lm_preds <- get_place_mod_preds(place_mod_lm)

gg_place_from_submissions <- gg_place_from_submissions +
  geom_line(data = place_mod_lm_preds, color = colors[["orange"]])
  # geom_line(data = place_preds, color = colors[["green"]])

# ---- predicted-place-from-submission
sample_teams <- function(n_teams = 1, min_submissions = 50, 
                         min_final_place = 100, seed = NA) {
  
  if (!is.na(seed)) set.seed(seed)
  
  team_ids <- leaderboards %>%
    filter(
      TotalSubmissions >= min_submissions,
      Place <= min_final_place
    ) %>%
    sample_n(n_teams) %>%
    .$TeamId
  
  submissions %>% filter(TeamId %in% team_ids)
}

n_teams <- 200
submissions_sample <- sample_teams(n_teams = n_teams, seed = 821)

gg_predicted_place_from_submissions <- ggplot(submissions_sample, aes(SubmissionNum, PredictedPlace)) +
  geom_smooth(aes(group = TeamId), method = "lm", se = FALSE,
              size = 0.4, alpha = 0.4, color = colors[["submissions"]]) +
  scale_x_submission_number +
  scale_y_reverse("place", breaks = c(1, 500, seq(1000, 5000, by = 1000))) +
  base_theme +
  theme(legend.position = "none") +
  labs(title = paste("Changes in performance for", n_teams, "teams"))

team_submissions_mod <- lmer(PredictedPlace ~ SubmissionNum + 
                               (SubmissionNum|CompetitionId/TeamId),
                             data = submissions_sample)

team_submissions_preds <- data_frame(SubmissionNum = 1:100) %>%
  cbind(., predictSE(team_submissions_mod, newdata = .)) %>%
  rename(PredictedPlace = fit, SE = se.fit)

gg_predicted_place_from_submissions <- gg_predicted_place_from_submissions +
  geom_smooth(aes(ymin = PredictedPlace - SE, ymax = PredictedPlace + SE),
              data = team_submissions_preds, color = colors[["orange"]],
              size = 1.5)

# ---- team-size-submissions-correlation
set.seed(431)
ggplot(top_100, aes(TeamSize, TotalSubmissions)) +
  geom_point(shape = 1, position = position_jitter(width = 0.55)) +
  scale_x_continuous("team size", breaks = 1:25) +
  scale_y_continuous("submissions", breaks = c(1, seq(50, 600, by = 50))) +
  base_theme

# ---- submissions-by-team-size-per-place
ggplot(top_100_places, aes(TeamSize, TotalSubmissions)) +
  geom_text(aes(label = Place, color = FirstPlaceTeam), size = 2,
            check_overlap = TRUE) +
  scale_x_continuous("team size", breaks = 1:4) +
  scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5))) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1, 0.2)) +
  coord_cartesian(xlim = c(1, 3), ylim = c(1, 39)) +
  base_theme

# ---- submissions-controlling-for-team-size

# ---- total-time-submissions-correlation
gg_submissions_per_time <- ggplot(top_100, aes(TotalTimeSec, TotalSubmissions)) +
  geom_point(alpha = 0.2) +
  make_time_scale("submission interval (days)", seq(0, 450, by = 50),
                  "scale_x_continuous") +
  scale_y_total_submissions +
  scale_alpha_continuous(range = c(1, 0.2)) +
  base_theme

top_100$TotalSubmissionsSqr <- top_100$TotalSubmissions^2

submissions_per_time_mod <- lm(TotalTimeSec ~ TotalSubmissions + TotalSubmissionsSqr,
                               data = top_100)
submissions_per_time_preds <- data_frame(
  TotalSubmissions = 1:200,
  TotalSubmissionsSqr = TotalSubmissions^2
) %>% cbind(., predict(submissions_per_time_mod, newdata = ., se = TRUE)) %>%
  rename(TotalTimeSec = fit, SE = se.fit)

gg_submissions_per_time + 
  geom_smooth(aes(ymin = TotalTimeSec - SE, ymax = TotalTimeSec + SE),
              data = submissions_per_time_preds, stat = "identity")

# ---- submissions-by-team-size-per-place
ggplot(top_100_places, aes(TeamSize, TotalSubmissions)) +
  geom_text(aes(label = Place, color = FirstPlaceTeam), size = 2,
            check_overlap = TRUE) +
  scale_x_continuous("team size", breaks = 1:4) +
  scale_y_continuous("submissions", breaks = c(1, seq(5, 100, by = 5))) +
  scale_color_manual(values = c("black", colors[["first_place"]])) +
  scale_alpha_continuous(range = c(1, 0.2)) +
  coord_cartesian(xlim = c(1, 3), ylim = c(1, 39)) +
  base_theme
