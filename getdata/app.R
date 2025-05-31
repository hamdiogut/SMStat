library(shiny)
library(readxl)

data_dir <- "C:/MSDATA/GUNLUK/IMKB"

ui <- fluidPage(
  titlePanel("İndirilen IMKB Bültenini Göster"),
  sidebarLayout(
    sidebarPanel(
      actionButton("oku_btn", "Son Dosyayı Göster")
    ),
    mainPanel(
      tableOutput("veri")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$oku_btn, {
    dosyalar <- list.files(data_dir, pattern = "\\.xls$", full.names = TRUE)
    son_dosya <- dosyalar[which.max(file.info(dosyalar)$mtime)]

    veri <- read_excel(son_dosya, skip = 2)
    output$veri <- renderTable({
      head(veri, 20)
    })
  })
}

shinyApp(ui, server)

