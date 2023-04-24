library(shiny)
library(filterWidgets)
library(htmlwidgets)
library(DT)

#load modules
files = list.files(path = "mod/", pattern = "*.R")
if(length(files) > 0) sapply(paste0("mod/", files), source, .GlobalEnv)

#example input data
rawdata <- data.frame(nationality = sample(c("French", "German", "British", "American", "Canadian", "Dutch"), 1000, replace=TRUE, prob=c(0.2, 0.3, 0.3, .1, .05, .05)),
                      sex = sample(c("Male", "Female"), 1000, replace=TRUE, prob=c(0.5, 0.5)),
                      age = sample(c("child", "adult", "older adult"), 1000, replace=TRUE, prob=c(0.1, 0.7, 0.2)),
                      politics = sample(c("left", "center", "right"), 1000, replace=TRUE, prob=c(0.3, 0.4, 0.3)))
filterVar <- "nationality"
