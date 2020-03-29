# sebagai perbandingan...

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(negara %in% c(
    "China", "Italy", "Japan", "Korea, South", "Indonesia",
    "United Kingdom", "Malaysia", "Germany", "Iran", "US"
  ) & jumlah > 0) %>%
  mutate(negara = fct_relevel(as.factor(negara), c("Indonesia", "Italy"))) %>%
  group_by(negara, tanggal) %>%
  summarise(jumlah = sum(jumlah)) %>%
  filter(jumlah > 5) %>% # sangat fluktuatif di atas lima persen...
  group_by(negara) %>%
  arrange(tanggal) %>%
  mutate(hari = row_number())


ggplot(data = clean_data, aes(x = hari, y = jumlah, color = negara, alpha = negara)) +
  geom_line(aes(size = negara)) +
  geom_text(
    data = slice(clean_data, which.max(hari)),
    aes(x = hari, y = jumlah, label = formatC(jumlah, big.mark = ".", decimal.mark = ",", format = "f", digits = 0)),
    hjust = -.25, show.legend = F
  ) +
  labs(
    title = "Tren penyebaran COVID-19 di Indonesia dan dunia",
    color = "Kasus terkonfirmasi di negara: ",
    x = "Hari sejak awal pelaporan",
    y = "Jumlah yang terinfeksi (skala log)",
    caption = paste("Sumber data: Johns Hopkins per", max(clean_data$tanggal) %>%
      format(format = "%d-%m-%Y"))
  ) +
  scale_x_continuous(limits = c(1, max(clean_data$hari) + 3)) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_manual(
    values = c(
      "Indonesia" = "firebrick",
      "Malaysia" = "goldenrod2",
      "China" = "gray45",
      "Italy" = "springgreen4",
      "Japan" = "cornflowerblue",
      "Korea, South" = "coral",
      "United Kingdom" = "red",
      "Germany" = "black",
      "US" = "yellowgreen",
      "Iran" = "yellow"
    ),
    guide = guide_legend(
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(size = 2)
    )
  ) +
  scale_alpha_manual(
    values = c(
      "Indonesia" = 1,
      "Malaysia" = 1,
      "China" = .6,
      "Italy" = .6,
      "Japan" = .6,
      "Korea, South" = .6,
      "United Kingdom" = .6,
      "Germany" = .6,
      "Iran" = .6,
      "US" = .6
    ),
    guide = guide_none()
  ) +
  scale_size_manual(
    values = c(
      "Indonesia" = 1.5,
      "Malaysia" = 1.5,
      "China" = .8,
      "Italy" = .8,
      "Japan" = .8,
      "Korea, South" = .8,
      "United Kingdom" = .8,
      "Germany" = .8,
      "Iran" = .8,
      "US" = .8
    ),
    guide = guide_none()
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 90),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25"),
    legend.position = "bottom"
  )

ggsave("./img/foreign.png", dpi = 150, units = "cm", width = 25, height = 20)
