source("docs/kaggle/R/setup.R")
source("docs/kaggle/R/theme.R")

# ---- titanic
data("titanic")
titanic %<>%
  select(Age, Sex, Survived) %>%
  filter(complete.cases(.)) %>%
  mutate(SexF = factor(Sex),
         AgeBin = cut(Age, breaks = 10))

age_bins <- data_frame(AgeBin = unique(titanic$AgeBin)) %>%
  mutate(AgeStr = as.character(AgeBin) %>% substring(., 2, nchar(.)-1),
         AgeMin = str_split_fixed(AgeStr, ",", n = 2)[,1] %>% as.numeric(),
         AgeMax = str_split_fixed(AgeStr, ",", n = 2)[,2] %>% as.numeric(),
         AgeBinMean = rowMeans(cbind(AgeMax, AgeMin))) %>%
  select(AgeBin, AgeBinMean) %>%
  left_join(titanic) %>%
  group_by(AgeBinMean, Sex) %>%
  summarize(Survived = mean(Survived, na.rm = TRUE),
            People = n())

# library(randomForest)
# f0 <- randomForest(Survived ~ SexF, data = titanic)
# f1 <- randomForest(Survived ~ SexF + Age, data = titanic)

scale_y_survival <- scale_y_continuous("survival rate (training data)",
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
  labs(x = "") +
  geom_bar(stat = "summary", fun.y = "mean", alpha = default_alpha) +
  theme(panel.grid.major.x = element_blank())

sex_by_age <- titanic_plot +
  aes(Age, Survived, color = Sex) +
  labs(x = "age") +
  geom_smooth(method = "glm", se = FALSE) +
  geom_point(aes(x = AgeBinMean, size = People),
             data = age_bins)

grid.arrange(overall_sex, sex_by_age,
             nrow = 1)
