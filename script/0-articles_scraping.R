library(rvest)
library(dplyr)
library(stringr)

# 1. Berita ITB -----
url <- "https://www.itb.ac.id/news/read/57444/home/peneliti-itb-buat-pemodelan-prediksi-puncak-penyebaran-covid-19-di-indonesia"

page <- read_html(url)

title <- page %>% html_node("h2") %>% html_text() %>% str_squish()

content <- read_html(url) %>% 
  html_nodes(".col-md-9 div") %>% 
  html_text() %>% 
  str_squish() %>% 
  .[5:25] %>% 
  .[seq(1, length(.), 2)]

file.create("news/itb.md")
write.table(content, file = "news/itb.md", sep = "\n", quote = FALSE, 
            col.names = paste("#", title, sep = " "), row.names = FALSE)

# 2. Berita MI -----
url <- "https://mediaindonesia.com/read/detail/289262-kemenkes-prediksi-penanganan-virus-korona-lebih-dari-6-bulan"

page <- read_html(url)

title <- page %>% html_node("h1 b") %>% html_text() %>% str_squish()

content <- read_html(url) %>% 
  html_nodes(".jap p") %>% 
  html_text() %>% 
  str_squish()

file.create("news/mi.md")
write.table(content, file = "news/mi.md", sep = "\n", quote = FALSE, 
            col.names = paste("#", title, sep = " "), row.names = FALSE)

# 3. Berita waspada (nanti diganti lah) -----

url <- "http://waspada.co.id/2020/03/bin-prediksi-puncak-virus-corona-di-indonesia-saat-ramadhan/"

page <- read_html(url)

title <- page %>% html_node(".entry-title") %>% html_text() %>% str_squish()

content <- page %>% 
  html_nodes("p") %>% 
  html_text() %>% 
  str_squish() %>% 
  .[1:20]

file.create("news/wpd.md")
write.table(content, file = "news/wpd.md", sep = "\n", quote = FALSE, 
            col.names = paste("#", title, sep = " "), row.names = FALSE)

# kemudian rapikan format sambil membaca -----