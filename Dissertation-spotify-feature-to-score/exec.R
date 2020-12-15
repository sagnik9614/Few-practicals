options(scipen = 999)

suppressWarnings(suppressMessages(source("packages.R", echo = T)))
suppressWarnings(suppressMessages(source("EDA.R", echo = T)))
suppressWarnings(suppressMessages(source("classifier.R", echo = T)))
suppressWarnings(suppressMessages(source("regression.R", echo = T)))

save(list=ls(), file = "objects.RData")

suppressWarnings(suppressMessages(source("outputs.R", echo = T)))