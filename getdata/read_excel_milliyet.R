library(shiny)
library(httr)
library(rvest)
library(stringr)
library(lubridate)

# Kaydedilecek klasör
save_dir <- "C:/MSDATA/GUNLUK/IMKB"

ui <- fluidPage(
  titlePanel("Borsa İstanbul Günlük Bülten İndirici"),

  sidebarLayout(
    sidebarPanel(
      dateInput("tarih", "Bülten Tarihi Seç:", value = Sys.Date(), language = "tr", weekstart = 1),
      actionButton("indir_btn", "Bülteni İndir"),
      br(), br(),
      verbatimTextOutput("log")
    ),
    mainPanel(
      textOutput("status")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$indir_btn, {
    tryCatch({
      secilen_tarih <- format(input$tarih, "%d.%m.%Y")
      gun <- format(input$tarih, "%d")
      ay <- format(input$tarih, "%m")
      yil <- format(input$tarih, "%Y")

      # BIST sayfasındaki Excel linklerini çek
      url <- "https://borsaistanbul.com/tr/sayfa/468/pay-piyasasi-verileri-bulten-verileri"
      sayfa <- read_html(url)
      linkler <- sayfa %>% html_elements("a") %>% html_attr("href")
      xls_linkler <- linkler[grepl("\\.xls$", linkler, ignore.case = TRUE)]

      # Seçilen tarihe ait dosyayı bul
      hedef_link <- xls_linkler[str_detect(xls_linkler, fixed(paste0(gun, ".", ay, ".", yil)))]

      if (length(hedef_link) == 0) {
        output$status <- renderText(paste("Seçilen tarihe ait dosya bulunamadı:", secilen_tarih))
        return()
      }

      tam_link <- paste0("https://borsaistanbul.com", hedef_link[1])
      dosya_adi <- basename(tam_link)
      kayit_yolu <- file.path(save_dir, dosya_adi)

      # İndirme
      download.file(tam_link, destfile = kayit_yolu, mode = "wb")

      output$status <- renderText(paste("İndirme tamamlandı:", dosya_adi))
    }, error = function(e) {
      output$status <- renderText(paste("HATA:", e$message))
    })
  })
}

shinyApp(ui, server)

