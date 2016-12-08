#' Connect to the Kaggle SQLite database.
#'
#' @param sqlite_location Location of Kaggle sqlite database. Download this file
#'   from \url{https://www.kaggle.com/kaggle/meta-kaggle/downloads/database.sqlite.zip},
#'   unzip it, and put it wherever you'd like. By default, the file is expected
#'   to be found in the "ratchets" package at "inst/extdata/kaggle.sqlite".
#'
#' @examples
#' kaggle_db <- connect_kaggle("path/to/kaggle.sqlite")
#' kaggle_db <- connect_kaggle()  # assumes db included in ratchets pkg
#'
#' @export
connect_kaggle <- function(sqlite_location) {
  if (missing(sqlite_location)) {
    sqlite_location <- system.file("extdata/kaggle.sqlite", package = "ratchets")
  }
  if (!file.exists(sqlite_location)) stop("kaggle.sqlite not found")

  dplyr::src_sqlite(sqlite_location)
}
