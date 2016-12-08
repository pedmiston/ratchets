#' Get model predictions for a model predicting Place from TotalSubmissions.
#'
#' @import dplyr
#' @export
get_place_mod_preds <- function(mod, predict_fn, x_preds) {
  if (missing(x_preds)) x_preds <- data_frame(TotalSubmissions = 1:200)
  if (missing(predict_fn)) predict_fn <- predict

  x_preds %>%
    cbind(., predict_fn(mod, ., se = TRUE)) %>%
    rename(Place = fit, SE = se.fit) %>%
    mutate(TotalSubmissionsBin = TotalSubmissions)  # for consistency with summary
}
