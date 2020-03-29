# membuat dan menyimpan grafik sebagai gambar

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(negara == "Indonesia" & jumlah > 0) %>%
  mutate(hari = lubridate::day(tanggal))

trend <- nls(jumlah ~ a * (1 + r)^(hari),
  data = subset(clean_data, hari < 29),
  start = list(a = 1, r = .01))

besok <- data.frame(hari = 29:41)

prediksi <- data.frame(
  tanggal = c(as.Date("2020-03-29"):as.Date("2020-04-10")) %>% as.Date(origin = as.Date("1970-01-01")),
  jumlah = predict(trend, newdata = besok))

ggplot(data = clean_data, aes(x = tanggal, y = jumlah)) +
#  geom_text(
#    data = prediksi, 
#    mapping = aes(
#      x = tanggal, y = jumlah,
#      label = formatC(jumlah, big.mark = ".", decimal.mark = ",", 
#        format = "f", digits = 0)),
#    hjust = 1.2,
#    color = "gray50") +
  geom_smooth(
    data = filter(clean_data, tanggal >= as.Date("2020-03-20")), 
    aes(color = "gray75"), 
    method = "lm", 
    size = .5, fullrange = T, se = F, linetype = "dashed") +
  geom_line(aes(color = "firebrick"), lwd = 1.2) +
  geom_point(data = prediksi, pch = 4) +
  geom_text(
    data = slice(clean_data, which.max(tanggal)),
    mapping = aes(
      x = tanggal, y = jumlah, 
      label = formatC(jumlah, big.mark = ".", decimal.mark = ",", 
                      format = "f", digits = 0)), 
    hjust = -.5, color = "firebrick") +
  labs(
    title = "Kecenderungan infeksi COVID-19 di Indonesia", 
    color = "Keterangan grafik", 
    caption = paste("Sumber data: Johns Hopkins per", 
      max(clean_data$tanggal) %>% format(format = "%d %b %Y"))) +
  scale_x_date(
    date_breaks = "1 day", 
    minor_breaks = NULL, 
    labels = scales::date_format(format = "%d/%m"), 
    limits = as.Date(c("2020-03-10", "2020-04-11"))) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_identity(
    labels = c("jumlah kasus", "tren sejak 20-Mar-2020"), 
    guide = guide_legend(title.position = "top", title.hjust = 0.5)) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25"),
    legend.position = "bottom")

ggsave("./img/gambar.pdf", dpi = 300, units = "cm", width = 25, height = 15)
ggsave("./img/gambar.png", dpi = 300, units = "cm", width = 25, height = 15)
