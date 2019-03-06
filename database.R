library(tidyverse)
library(RSQLite)
library(WindR)
library(readxl)
library(xlsx)
w.start()


fields <- read_xlsx("fields.xlsx", col_names = TRUE)
flds <- paste(fields$fields, sep = "", collapse = ", ")

ticker <- read_xlsx("CB_list.xlsx", col_names = TRUE)

date1 <- as.Date("2019/1/1")
date2 <- Sys.Date() - 1

data_fetch <- function(ticker) {
  
  data <- w.wsd(ticker, flds, date1, date2)$Data
  
}

data <- lapply(ticker$ticker, data_fetch) 
data <- do.call(bind_rows, data)

