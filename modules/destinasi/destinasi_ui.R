
destinasi_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Analisis Destinasi"),
    shiny::p(
      "Membandingkan kunjungan objek wisata dan mengeksplorasi detail destinasi."
    ),
    
    shiny::wellPanel(
      shiny::h4("Filter destinasi"),
      shiny::fluidRow(
        shiny::column(
          6,
          shiny::selectInput(
            ns("category_filter"),
            "Kategori wisata",
            choices = "Semua"
          )
        ),
        shiny::column(
          6,
          shiny::selectInput(
            ns("area_filter"),
            "Kapanewon",
            choices = "Semua"
          )
        )
      )
    ),
    
    shiny::fluidRow(
      shiny::column(
        7,
        shiny::wellPanel(
          shiny::h4("10 destinasi dengan kunjungan tertinggi"),
          shiny::plotOutput(
            ns("top_plot"),
            height = "370px"
          )
        )
      ),
      
      shiny::column(
        5,
        shiny::wellPanel(
          shiny::h4("Kunjungan terendah"),
          shiny::uiOutput(ns("lowest_destination")),
          shiny::p(
            "Hanya destinasi dengan data 12 bulan WNI/WNA lengkap dan tanpa duplikasi yang dibandingkan."
          )
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Daftar seluruh destinasi"),
      shiny::p(
        "Gunakan fitur pencarian dan pengurutan pada tabel untuk menemukan destinasi."
      ),
      DT::DTOutput(ns("destination_table"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Detail kunjungan destinasi"),
      shiny::selectInput(
        ns("destination_detail"),
        "Pilih destinasi",
        choices = character(0)
      ),
      shiny::plotOutput(
        ns("detail_plot"),
        height = "330px"
      ),
      DT::DTOutput(ns("detail_table")),
      shiny::textOutput(ns("detail_note"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Catatan kualitas data"),
      shiny::textOutput(ns("quality_note"))
    )
  )
}
