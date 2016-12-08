#' Create a ggplot2 scale object for time variable in seconds.
#' @import ggplot2
#' @export
make_time_scale <- function(name, breaks_days,
                            scale_obj = "scale_y_continuous", ...) {
  breaks_sec <- breaks_days * 24 * 3600
  get(scale_obj)(name, breaks = breaks_sec, labels = breaks_days, ...)
}
