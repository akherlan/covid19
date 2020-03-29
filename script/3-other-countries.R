# sebagai perbandingan...

library(tidyverse)

clean_data <- read_csv2("./data/raw_data.csv") %>%
  filter(negara %in% c(
    "China", "Italy", "Japan", "Korea, South", "Indonesia",
    "United Kingdom", "Malaysia", "Singapore", "Iran", "US"
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
    aes(x = hari, y = jumlah, 
        label = paste0(negara, " (",
                       formatC(jumlah, big.mark = ".", decimal.mark = ",",
                               format = "f", digits = 0), ")")),
    hjust = -.05, show.legend = F
  ) +
  labs(
    title = "Penyebaran COVID-19 di Indonesia dan dunia",
    color = "Kasus terkonfirmasi di negara: ",
    x = "Hari (sejak awal pelaporan)",
    y = "Jumlah yang terinfeksi (skala log)",
    caption = paste("Sumber data: Johns Hopkins per", max(clean_data$tanggal) %>%
      format(format = "%d %b %Y"))
  ) +
  scale_x_continuous(limits = c(1, max(clean_data$hari) + 6)) +
  scale_y_log10(labels = scales::number_format()) +
  scale_color_manual(
    values = c(
      "Indonesia" = "firebrick",
      "Malaysia" = "goldenrod2",
      "China" = "gray45",
      "Italy" = "springgreen4",
      "Japan" = "cornflowerblue",
      "Korea, South" = "purple3",
      "United Kingdom" = "red",
      "Singapore" = "black",
      "US" = "yellowgreen",
      "Iran" = "yellow3"
    ),
    guide = guide_none()
  ) +
  scale_alpha_manual(
    values = c(
      "Indonesia" = 1,
      "Malaysia" = .6,
      "China" = .6,
      "Italy" = .6,
      "Japan" = .6,
      "Korea, South" = .6,
      "United Kingdom" = .6,
      "Singapore" = .6,
      "Iran" = .6,
      "US" = .6
    ),
    guide = guide_none()
  ) +
  scale_size_manual(
    values = c(
      "Indonesia" = 1.2,
      "Malaysia" = .8,
      "China" = .8,
      "Italy" = .8,
      "Japan" = .8,
      "Korea, South" = .8,
      "United Kingdom" = .8,
      "Singapore" = .8,
      "Iran" = .8,
      "US" = .8
    ),
    guide = guide_none()
  ) +
  theme_linedraw() +
  theme(
    axis.text.x = element_text(angle = 0),
    axis.title.y = element_blank(),
    panel.grid.major = element_line(color = "gray75"),
    plot.caption = element_text(color = "gray25")
  )

ggsave("./img/foreign.png", dpi = 300, units = "cm", width = 30, height = 18)
