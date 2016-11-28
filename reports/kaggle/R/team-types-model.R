lmer(Place ~ TeamType + (1|CompetitionId), data = top_100_team_types) %>%
  tidy(effects = "fixed") %>%
  select(term, estimate) %>%
  knitr::kable(digits = 4)

lm(Place ~ TeamType, data = top_100_team_types) %>%
  tidy() %>%
  select(term, estimate) %>%
  knitr::kable(digits = 4)

lmList(Place ~ TeamType | CompetitionId, data = top_100_team_types) %>%
  coef() %>%
  colMeans(na.rm = TRUE) %>%
  t() %>% as_data_frame() %>%
  gather(Term, Estimate) %>%
  knitr::kable(digits = 4)