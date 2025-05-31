library(shiny)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)
Sys.setlocale(category = "LC_ALL", locale = "tr_TR.UTF-8")
save_dir <- "C:/MSDATA/GUNLUK/IMKB"

ui <- fluidPage(
  titlePanel("BIST Günlük Bülten İndir ve İşle"),
  sidebarLayout(
    sidebarPanel(
      dateInput("tarih", "Tarih Seçin:", value = Sys.Date(), language = "tr"),
      actionButton("indir_btn", "İndir ve İşle"),
      br(), br(),
      downloadButton("indir_excel", "İşlenmiş Excel'i İndir")
    ),
    mainPanel(
      verbatimTextOutput("durum"),
      verbatimTextOutput("icerik")
    )
  )
)

server <- function(input, output, session) {
  export_yolu <- reactiveVal(NULL)

  observeEvent(input$indir_btn, {
    tarih <- input$tarih
    yil <- format(tarih, "%Y")
    ay <- format(tarih, "%m")
    gun <- format(tarih, "%d")

    # Burada zip adı sonundaki '1' ekleniyor ve "thb" kullanılıyor
    zip_adi <- paste0("thb", yil, ay, gun, "1.zip")
    url <- paste0("https://www.borsaistanbul.com/data/thb/", yil, "/", ay, "/", zip_adi)
    zip_yol <- file.path(save_dir, zip_adi)

    tryCatch({
      download.file(url, destfile = zip_yol, mode = "wb", quiet = TRUE)

      if (!file.exists(zip_yol) || file.info(zip_yol)$size < 5000) {
        stop("Dosya indirilemedi veya boyutu çok küçük.")
      }

      zip_icerik <- unzip(zip_yol, list = TRUE)

      dosya_ad <- zip_icerik$Name[grepl("\\.(csv|xls)$", zip_icerik$Name, ignore.case = TRUE)][1]

      if (is.na(dosya_ad)) stop("ZIP içinde CSV veya XLS dosyası bulunamadı.")

      unzip(zip_yol, exdir = save_dir, files = dosya_ad, overwrite = TRUE)

      tam_yol <- file.path(save_dir, dosya_ad)

      veri <- if (grepl("\\.csv$", tam_yol, ignore.case = TRUE)) {
        read_csv(tam_yol, locale = locale(encoding = "UTF-8"), show_col_types = FALSE)
      } else {
        read_excel(tam_yol)
      }

      # Sadece belirli sütunları seç (2,3,7,8,9)
      veri_secili_

  output$indir_excel <- downloadHandler(
    filename = function() {
      paste0("islenmis_bulten_", format(input$tarih, "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      req(export_yolu())
      file.copy(export_yolu(), file, overwrite = TRUE)
    }
  )
}

shinyApp(ui, server)
