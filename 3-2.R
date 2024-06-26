# 2-1
library(gtrendsR)
library(tidyquant)
library(tidyverse)

# weekly data collection
trends_weekly <- function(keyword, geo) {
  unadjusted <- data.frame()
  # every 5 years
  for (yr in c(2004,2020)) {
    start <- ymd(paste(yr, "01", "01", sep = "-"))
    end <- ymd(paste(yr+4, "12-31", sep = "-"))
    span <- paste(start, end, sep = " ")
    temp <- gtrends(keyword = keyword, geo = geo, time = span, onlyInterest = TRUE) %>% .[[1]]
    unadjusted <- rbind(unadjusted, temp)
    Sys.sleep(3)
  }
  trends_all <- gtrends(keyword = keyword, geo = geo, time = "all", onlyInterest = TRUE) %>% .[[1]] %>% 
    select(weight = hits)
  end_yearmon <- paste(year(today()), formatC(nrow(trends_all)%%12, width = 2, flag = "0"), "01", sep = "-")
  trends_all <- mutate(trends_all, link = format(seq(ymd("2004-01-01"), ymd(end_yearmon), by = "month"), "%Y-%m"))
  trends_adjusted <- unadjusted %>% 
    mutate(link = format(date, "%Y-%m")) %>% 
    merge(trends_all, by = "link", all.x = TRUE) %>% 
    mutate(SVI = hits * weight/100,
           date = as.Date(date))
  return(trends_adjusted)
}

trends_abnormal <- function(weekly_data) {
  log_SVI <- log(weekly_data$SVI)
  ASVI <- rep(NA, length(log_SVI))
  for (week in 9:length(log_SVI)) {
    SVI_m <- median(log_SVI[(week-8):(week-1)])
    ASVI[week] <- log_SVI[week] - SVI_m
  }
  weekly_data <- weekly_data %>% 
    select(date, keyword, SVI) %>% 
    mutate(log_SVI = log_SVI, ASVI = ASVI)
  return(weekly_data)
}

# 2330
svi_2330 <- trends_weekly("2330", "TW")
svi_2330 <- trends_abnormal(svi_2330)
sink(file = "TSMC_demo.txt")
tail(svi_2330)
sink()

# 3-2
library(tidyquant)
library(tidyverse)
library(highcharter)

x <- getSymbols("2330.TW", auto.assign = FALSE)
highchart(type = "stock") |> 
  hc_add_series(x)

stock_2330 <- tq_get("2330.TW", from = "2009-01-01", to = "2024-06-12") |>
  mutate(year = year(date), week = week(date)) |> 
  group_by(year, week) |> 
  summarize(
    max_price = max(adjusted, na.rm = TRUE),
    min_price = min(adjusted, na.rm = TRUE),
    avg_price = mean(adjusted, na.rm = TRUE)
  ) |> 
  ungroup()

svi_2330 <- svi_2330 |> 
  mutate(year = year(date), week = week(date))
trends_2330 <- merge(stock_2330, svi_2330, by = c("year", "week"))
