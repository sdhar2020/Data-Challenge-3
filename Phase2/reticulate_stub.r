library(reticulate)
library(dplyr)

source_python('RUL_Model.py')
dat <- read.csv('test_FD001.txt')

transformed <- preprocess(dat)
predictions <- preprocess_and_predict(dat)