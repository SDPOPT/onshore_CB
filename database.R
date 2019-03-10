library(tidyverse)
library(RSQLite)
library(WindR)
library(readxl)
library(formattable)
library(xlsx)
library(xts)
w.start()


cb_des <- read_xlsx("fields.xlsx", sheet = "cb_description",col_names = TRUE)
des_cb <- paste(cb_des$fields, sep = "", collapse = ", ")

equity_des <- read_xlsx("fields.xlsx", sheet = "equity_description", col_names = TRUE)
des_equity <- paste(equity_des$fields, sep = "", collapse = ", ")

cb_ticker <- read_xlsx("CB_list.xlsx", col_names = TRUE)
ticker_cb <- paste(cb_ticker$ticker, sep = "", collapse = ", ")

cb_daily <- read_xlsx("fields.xlsx", sheet = "daily_cb", col_names = TRUE)
equity_daily <- read_xlsx("fields.xlsx", sheet = "daily_equity", col_names = TRUE)


cb <- w.wss(ticker_cb, des_cb)$Data



equity_ticker <- cb %>% select(ticker = UNDERLYINGCODE)
ticker_equity <- paste(unique(equity_ticker$ticker), sep = "", collapse = ", ")

equity <- w.wss(ticker_equity, des_equity, 
                'industryType = 1', 'unit = 1', 
                'tradeDate = 20190308')$Data

data_fetch_cb <- function(ticker) {
  
  date1 <- Sys.Date() - 66
  date2 <- Sys.Date() - 1
  cb_daily <- read_xlsx("fields.xlsx", sheet = "daily_cb", col_names = TRUE)
  daily_cb <- paste(cb_daily$fields, sep = "", collapse = ", ")
  
  data <- w.wsd(ticker, daily_cb, date1, date2)$Data
  
  return(data)
  
}

cb_trade <- lapply(cb_ticker$ticker, data_fetch_cb)
cb_trade <- do.call(bind_rows, cb_trade)


data_fetch_eq <- function(ticker) {
  
  date1 <- Sys.Date() - 66
  date2 <- Sys.Date() - 1
  equity_daily <- read_xlsx("fields.xlsx", sheet = "daily_equity", col_names = TRUE)
  daily_equity <- paste(equity_daily$fields, sep = "", collapse = ", ")
  
  data <- w.wsd(ticker, daily_equity, date1, date2)$Data
  
  return(data)
  
}

equity_trade <- lapply(cb_ticker$ticker, data_fetch_eq)
equity_trade <- do.call(bind_rows, equity_trade)

names(cb) <- c("转债代码", cb_des$中文)
names(equity) <- c("正股代码", equity_des$中文)
names(cb_trade) <- c("date", cb_daily$中文)
names(equity_trade) <- c("date", equity_daily$中文)

cb_status <- cb_trade %>%
  filter(date == as.Date("2019/3/8")) %>%
  mutate(`平价底价溢价率` = `转债平价`/`纯债价值` - 1) %>%
  select(`转债代码`, `收盘价`,`到期收益率`, `纯债溢价率`, 
         `转股价格`, `转债平价`, `转股溢价率`, `平价底价溢价率`) %>%
  right_join(cb) %>%
  left_join(equity) %>%
  select(-剩余面值, -剩余比例, -万得行业, -证监行业) %>%
  mutate(类型 = ifelse(平价底价溢价率 >= 0.2, "偏股型",
                            ifelse(平价底价溢价率 <= -0.2, "偏债型", "平衡型"))) %>%
  mutate(收盘价 = round(收盘价, digits = 2),
         到期收益率 = percent(到期收益率 / 100),
         纯债溢价率 = percent(纯债溢价率 / 100),
         转债平价 = round(转债平价, digits = 2),
         转股溢价率 = percent(转股溢价率 / 100),
         平价底价溢价率 = percent(平价底价溢价率),
         市值 = accounting(市值 / 1000000),
         PETTM = round(PETTM, digits = 0),
         PE_2019 = round(PE_2019, digits = 0),
         PE_2020 = round(PE_2020, digits = 0),
         PB = round(PB, digits = 2),
         PB_2019 = round(PB_2019, digits = 2)) %>%
  select(转债代码, 转债名称, 正股代码, 正股名称, 转债价格 = 收盘价,
         转债平价, 到期收益率, 纯债溢价率, 转股溢价率,
         平价底价溢价率, 类型, 行业 = 申万行业, 市值,
         PE = PETTM, PE_2019, PE_2020, PB, PB_2019,
         债项评级, 主体评级)

cb_call <- read_xlsx("赎回条款.xlsx", col_names = TRUE) %>%
  mutate(触发比率 = percent(进度条 / 100),
             触发价格溢价率 = percent(`(触发价格-正股价格)/正股价格`/ 100)) %>%
  select(转债代码 = 债券代码, 转债名称 = 债券名称, 触发条件, 赎回起始日, 
             转股价格 = 转股价, 触发价格 = 触发价, 正股价格, 触发价格溢价率,
             触发进度, 触发比率)

cb_put <- read_xlsx("回售条款.xlsx", col_names = TRUE) %>%
  mutate(转债代码 = 债券代码,
             触发比率 = percent(进度条 / 100)) %>%
  left_join(select(cb_call, 转债代码, 正股价格)) %>%
  select(转债代码, 转债名称 = 债券名称, 触发条件, 回售起始日,
             转股价格 = 转股价, 触发价格 = 触发价, 正股价格, 
             触发进度, 触发比率)

cb_modified <- read_xlsx("下修条款.xlsx", col_names = TRUE) %>%
  mutate(转债代码 = 债券代码,
             触发比率 = percent(进度条 / 100)) %>%
  left_join(select(cb_call, 转债代码, 正股价格)) %>%
  select(转债代码, 转债名称 = 债券名称, 触发条件, 修正起始日,
             转股价格 = 转股价, 触发价格 = 触发价, 正股价格, 
             触发进度, 触发比率)

cb_equity <- cb %>%
  select(转债代码, 转债名称, 正股代码, 正股名称)

cb_return <- cb_trade %>%
  left_join(cb_equity) %>%
  select(date, 转债代码, 转债名称, 收盘价, 涨跌幅,
         成交金额, 剩余面值) %>%
  group_by(转债代码, 转债名称) %>%
  arrange(-desc(date)) %>%
  mutate(换手率 = 成交金额 / 剩余面值,
         return = 收盘价 / 收盘价[1] - 1,
         YTD = rep(last(return), times = NROW(return))) %>%
  ungroup() %>%
  group_by(转债代码, 转债名称, YTD) %>%
  summarise(平均换手率 = mean(换手率, na.rm = TRUE),
            中位换手率 = median(换手率, na.rm = TRUE),
            最高换手率 = max(换手率, na.rm = TRUE),
            最低换手率 = min(换手率, na.rm = TRUE),
            平均涨跌幅 = mean(涨跌幅 / 100, na.rm = TRUE),
            中位涨跌幅 = median(涨跌幅 / 100, na.rm = TRUE),
            最大涨幅 = max(涨跌幅 / 100, na.rm = TRUE),
            最大跌幅 = min(涨跌幅 / 100, na.rm = TRUE)) %>%
  filter(is.nan(YTD) == FALSE) %>%
  ungroup() %>%
  mutate_at(c("YTD", "平均换手率", "中位换手率", "最高换手率", "最低换手率",
              "平均涨跌幅", "中位涨跌幅", "最大涨幅", "最大跌幅"), percent)



cb_list <- list(status = cb_status, call = cb_call,
                put = cb_put, modified = cb_modified,
                return = cb_return)
saveRDS(cb_list, "cb_list.rds")
