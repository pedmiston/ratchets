library(devtools)
library(readr)

titanic <- read_csv("data-raw/titanic.csv")
use_data(titanic)
