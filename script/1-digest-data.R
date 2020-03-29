# digest data = memuat + denormalisasi data

library(tidyverse)

curl::curl_download(
  url = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
  destfile = "./data/time_series_covid19-confirmed.csv"
)

raw_data <- readr::read_csv("./data/time_series_covid19-confirmed.csv") %>%
  select(-"Province/State", -Lat, -Long, negara = "Country/Region") %>%
  pivot_longer(-negara, names_to = "tanggal", values_to = "jumlah") %>%
  mutate(tanggal = lubridate::mdy(tanggal))

write_csv2(raw_data, "./data/raw_data.csv")
