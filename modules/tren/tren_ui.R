
tren_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Tren Kunjungan"),
    shiny::p(
      "Melihat perubahan kunjungan wisatawan dari bulan ke bulan pada tahun terpilih."
    ),
    
    shiny::wellPanel(
      shiny::h4("Statistik kunjungan bulanan"),
      shiny::uiOutput(ns("monthly_statistics")),
      shiny::plotOutput(
        ns("monthly_plot"),
        height = "340px"
      ),
      DT::DTOutput(ns("monthly_table"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Perbandingan total kunjungan antar tahun"),
      shiny::p(
        "Konfirmasi hanya jika definisi, cakupan, dan struktur data antar tahun sudah diperiksa dan sebanding."
      ),
      
      shiny::checkboxInput(
        ns("confirm_comparable"),
        "Saya memastikan data antar tahun memiliki definisi, cakupan, dan struktur yang sebanding.",
        value = FALSE
      ),
      
      shiny::textOutput(ns("growth_note")),
      DT::DTOutput(ns("annual_table"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Catatan kualitas data"),
      shiny::textOutput(ns("quality_note"))
    )
  )
}
