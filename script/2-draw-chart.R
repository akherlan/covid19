# membuat dan menyimpan grafik sebagai gambar

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(negara == "Indonesia" & jumlah > 0) %>%
  mutate(hari = lubridate::day(tanggal))

# berapa seharusnya nilai a dan r yang tepat?
trend <- nls(jumlah ~ a * (1 + r)^(hari),
  data = subset(clean_data, nrow(clean_data) < nrow(clean_data)-14),
  start = list(a = 1, r = .01))

besok <- data.frame(hari = (nrow(clean_data)-14):(nrow(clean_data)+1))

prediksi <- data.frame(
  tanggal = c(as.Date("2020-04-03"):max(clean_data$tanggal)) %>%
    as.Date(origin = as.Date("1970-01-01")),
  jumlah = predict(trend, newdata = besok))

ggplot(data = clean_data, aes(x = tanggal, y = jumlah)) +
  geom_text(
    data = prediksi, 
    mapping = aes(
      x = tanggal, y = jumlah,
      label = formatC(jumlah, big.mark = ".", decimal.mark = ",", 
        format = "f", digits = 0)),
    size = 2.4,
    hjust = 1.3,
    color = "gray50") +
  geom_smooth(
    data = filter(clean_data, tanggal >= as.Date("2020-04-10")), 
    aes(color = "gray50"), 
    method = "lm", 
    size = .4, fullrange = T, se = F, linetype = "dashed") +
  geom_line(aes(color = "firebrick"), lwd = 1.2) +
  geom_point(data = prediksi, pch = 8, alpha = .4) +
  geom_text(
    data = slice(clean_data, which.max(tanggal)),
    mapping = aes(
      x = tanggal, y = jumlah, 
      label = formatC(jumlah, big.mark = ".", decimal.mark = ",", 
                      format = "f", digits = 0)), 
    hjust = -.5, color = "firebrick", size = 4) +
  labs(
    title = "Kecenderungan infeksi COVID-19 di Indonesia", 
    subtitle = "Prediksi * * * sejak 2 pekan terakhir",
    caption = paste("Sumber data: Johns Hopkins per", 
      max(clean_data$tanggal) %>% format(format = "%d %b %Y"))) +
  scale_x_date(
    date_breaks = "1 day", 
    minor_breaks = NULL, 
    labels = scales::date_format(format = "%d/%m"), 
    limits = as.Date(c("2020-03-10", "2020-04-25"))) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_identity(
    labels = c("jumlah kasus", 
               paste0("tren dalam satu pekan terakhir (sejak ", 
                      as.character(
                        format.Date(head(tail(clean_data, 7), 1)$tanggal,
                                    format = "%d %b %Y")), 
                      ")")), 
    guide = guide_legend(title.position = "bottom", title.hjust = 0.5)) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25"),
    legend.title = element_blank(),
    legend.position = "bottom")

ggsave("./img/image.png", dpi = 300, units = "cm", width = 25, height = 20)
