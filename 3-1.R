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
    mutate(SVI = hits*weight/100,
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

svi_nd <- trends_weekly("納斯達克", "TW")

# 1-2
svi_unem <- svi_unem |>
  filter(year(date) != 2008) |>
  group_by(year(date)) |>
  summarise(SVI = sum(SVI))
# colnames(svi_unem)[colnames(svi_unem) == "year(date)"] <- "year"

# 3-1 pearson's correlation
library(writexl)
library(tidyverse)
library(broom)
library(highcharter)

results <- data.frame(keyword = character(), r = numeric(), p = numeric())
taiex <- read_excel("2023T1-01.xls")

data <- merge(taiex, hedge, by = "year")
key <- "unem"

result <- cor.test(data[["SVI"]], data[["avg_TAIEX"]], method = "pearson")
r_value <- result$estimate
p_value <- result$p.value
results <- rbind(results, data.frame(key, r_value, p_value))
stat_text <- paste("r = ", sprintf("%.3f", r_value),
                   "  p =", sprintf("%.3e", p_value))

# write_csv(results, "3-1_stats.csv")
# results_sorted <- results[order(results$r_value), ]

highchart() |> 
  hc_chart(type = "line") |> 
  hc_xAxis(categories = data$year, 
           title = list(text = "Time")) |> 
  hc_yAxis_multiples(list(
    labels = list(format = '{value:.0f}'),
    title = list(text = "Search Volume Index")),
    list(
      labels = list(format = '{value:.0f}'),
      title = list(text = "Stock Price"),
      opposite = T
    )) |> 
  hc_add_series(data$SVI, name = "SVI (left)", color = "#115f9a") |> 
  hc_add_series(data$avg_TAIEX, name = "TAIEX (right)", yAxis = 1, color = "#c86558") |> 
  hc_title(text = stat_text, align = "left", style = list(fontSize = "14px", fontWeight = "bold", color = "#333333"))
