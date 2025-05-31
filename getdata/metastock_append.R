append_metastock_data_to_existing <- function(df, date_str, target_dir = "C:/MSDATA/GUNLUK/IMKB") {
  if (!dir.exists(target_dir)) {
    stop("Hedef klasör bulunamadı: ", target_dir)
  }

  for (i in 1:nrow(df)) {
    row <- df[i, ]
    kod <- row$Kod
    close <- row$Kapanis
    high <- row$EnYuksek
    low  <- row$EnDusuk
    volume <- row$Hacim
    open <- ifelse(row$Acilis > 0, row$Acilis, close)

    # Metastock formatlı satır: YYYYMMDD,OPEN,HIGH,LOW,CLOSE,VOLUME
    line <- sprintf("%s,%.2f,%.2f,%.2f,%.2f,%.0f", date_str, open, high, low, close, volume)

    file_path <- file.path(target_dir, paste0(kod, ".txt"))

    if (file.exists(file_path)) {
      existing_lines <- readLines(file_path)
      if (any(grepl(date_str, existing_lines))) {
        message(sprintf("🟡 %s dosyasında %s tarihi zaten var, atlandı.", kod, date_str))
        next
      }
    }

    write(line, file = file_path, append = TRUE)
    message(sprintf("✅ %s için veri eklendi: %s", kod, line))
  }
}
