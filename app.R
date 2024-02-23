library(shiny)
library(rstac)
library(magrittr)
library(terra)
library(lidR)
library(fs)

ui <- fluidPage(
  titlePanel("LiDAR Data Downloader"),
  sidebarLayout(
    sidebarPanel(
      textInput("output_folder", "Output Folder"),
      actionButton("download", "Download Data")
    ),
    mainPanel(
      textOutput("download_info")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$download, {
    s_obj <- stac("https://planetarycomputer.microsoft.com/api/stac/v1/")
    bbox <- c(-121.755981, 40.623334, -121.699677, 40.647304)
    
    it_obj <- s_obj %>%
      stac_search(collections = "3dep-lidar-copc", bbox = bbox) %>%
      get_request() %>%
      items_sign(sign_fn = sign_planetary_computer())
    
    output_folder <- input$output_folder
    dir.create(output_folder, showWarnings = FALSE)
    
    for (i in seq_along(it_obj$features)) {
      url <- it_obj$features[[i]]$assets$data$href
      output_file <- file.path(output_folder, paste0("data_", i, ".laz"))
      download.file(url, destfile = output_file, mode = 'wb')
    }
    
    zip_file <- paste0(output_folder, ".zip")
    fs::zip(zip_file, files = fs::dir_ls(output_folder))
    
    output$download_info <- renderText({
      paste("Download complete. ZIP file available:", zip_file)
    })
  })
  
}

shinyApp(ui, server)
