library(tidyverse)
library(gtrendsR)

data("countries")
countries <- countries |> filter(grepl("TAIWAN", name))
sink(file = "geo_code.txt")
print(countries)
sink()