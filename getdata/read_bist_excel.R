library(readxl)
library(dplyr)
library(stringr)

read_bist_excel <- function(file_path) {
  # Excel dosyasını oku (örnek: ilk 7 satır başlık olabilir)
  df <- read_excel(file_path, skip = 7)

  # Gerekli sütunları seç
  df <- df %>%
    rename(
      Kod = `HİSSE KODU`,
      Acilis = `ALIŞ`,
      Satis = `SATIŞ`,
      Kapanis = `KAPANIŞ`,
      EnYuksek = `YÜKSEK`,
      EnDusuk = `DÜŞÜK`,
      Hacim = `HACİM (TL)`
    ) %>%
    select(Kod, Acilis, Satis, Kapanis, EnYuksek, EnDusuk, Hacim) %>%
    mutate(across(where(is.character), ~ str_trim(.)))

  df <- df %>%
    mutate(across(c(Acilis, Satis, Kapanis, EnYuksek, EnDusuk, Hacim), ~ as.numeric(str_replace_all(., "\\.", "") %>% str_replace_all(",", ".")))) %>%
    filter(!is.na(Kod), Kod != "")

  return(df)
}
