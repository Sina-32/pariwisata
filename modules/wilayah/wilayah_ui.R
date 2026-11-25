
wilayah_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Analisis Wilayah"),
    shiny::p(
      "Membandingkan jumlah kunjungan wisatawan berdasarkan Kapanewon. Halaman ini menggunakan grafik dan tabel, tanpa peta."
    ),
    
    shiny::wellPanel(
      shiny::h4("Kunjungan per Kapanewon"),
      
      shiny::plotOutput(
        ns("area_plot"),
        height = "430px"
      ),
      
      DT::DTOutput(ns("area_table")),
      
      shiny::textOutput(ns("area_note"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Catatan kualitas data"),
      shiny::textOutput(ns("quality_note"))
    )
  )
}
