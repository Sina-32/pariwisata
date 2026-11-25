
dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Dashboard"),
    shiny::p(
      "Ringkasan kondisi pariwisata Kabupaten Sleman pada tahun yang dipilih."
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Total kunjungan wisatawan"),
          shiny::uiOutput(ns("total_visits"))
        )
      ),
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Jumlah destinasi unik"),
          shiny::uiOutput(ns("destination_count"))
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Periode dan sumber data"),
          shiny::textOutput(ns("period")),
          shiny::textOutput(ns("source_note"))
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::wellPanel(
          shiny::h4("Catatan kualitas data"),
          shiny::textOutput(ns("quality_note"))
        )
      )
    )
  )
}
