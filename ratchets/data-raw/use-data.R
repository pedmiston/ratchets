library(devtools)
library(readr)

read_titanic <- . %>% file.path("data-raw/titanic", .) %>% read_csv()

titanic_train <- read_titanic("train.csv") %>% mutate(Type = "train")
titanic_test <- read_titanic("test.csv")   %>% mutate(Type = "test")
titanic <- bind_rows(titanic_train, titanic_test)

use_data(titanic, overwrite = TRUE)
