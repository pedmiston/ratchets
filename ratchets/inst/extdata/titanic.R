source("docs/kaggle/R/setup.R")
source("docs/kaggle/R/theme.R")

# ---- titanic
data("titanic")
titanic %<>%
  select(Age, Sex, Survived) %>%
  filter(complete.cases(.)) %>%
  mutate(SexF = factor(Sex))

# library(randomForest)
# f0 <- randomForest(Survived ~ SexF, data = titanic)
# f1 <- randomForest(Survived ~ SexF + Age, data = titanic)

scale_y_survival <- scale_y_continuous("Survival rate (training data)",
                                       breaks = seq(0, 1, by = 0.25),
                                       labels = percent)
ylim_survival <- c(0, 1)

titanic_plot <- ggplot(titanic) +
  scale_y_survival +
  scale_fill_manual(values = get_colors(c("green", "blue"))) +
  scale_color_manual(values = get_colors(c("green", "blue"))) +
  coord_cartesian(ylim = ylim_survival) +
  base_theme

overall_sex <- titanic_plot +
  aes(Sex, Survived, fill = Sex) +
  geom_bar(stat = "summary", fun.y = "mean", alpha = default_alpha)

sex_by_age <- titanic_plot +
  aes(Age, Survived, color = Sex) +
  geom_smooth(method = "glm", se = FALSE) +
  geom_rug(sides = "b")

grid.arrange(overall_sex, sex_by_age,
             nrow = 1)
