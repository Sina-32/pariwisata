
profil_wisatawan_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Profil Wisatawan"),
    shiny::p(
      "Membandingkan komposisi kunjungan wisatawan nusantara dan mancanegara."
    ),
    
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Komposisi WNI dan WNA"),
          shiny::plotOutput(
            ns("composition_plot"),
            height = "300px"
          )
        )
      ),
      
      shiny::column(
        6,
        shiny::wellPanel(
          shiny::h4("Ringkasan kelompok wisatawan"),
          DT::DTOutput(ns("profile_table")),
          
          shiny::h4("Probabilitas empiris kunjungan WNA"),
          shiny::textOutput(ns("wna_probability"))
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Komposisi kunjungan setiap bulan"),
      shiny::plotOutput(
        ns("monthly_plot"),
        height = "340px"
      ),
      shiny::textOutput(ns("monthly_note"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Catatan kualitas data"),
      shiny::textOutput(ns("quality_note"))
    )
  )
}
