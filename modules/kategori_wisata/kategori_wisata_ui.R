
kategori_wisata_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    shiny::h3("Kategori Wisata"),
    shiny::p(
      "Membandingkan jumlah kunjungan pada kategori utama sesuai klasifikasi sumber data."
    ),
    
    shiny::fluidRow(
      shiny::column(
        7,
        shiny::wellPanel(
          shiny::h4("Jumlah kunjungan per kategori"),
          shiny::plotOutput(
            ns("category_plot"),
            height = "340px"
          )
        )
      ),
      
      shiny::column(
        5,
        shiny::wellPanel(
          shiny::h4("Komposisi kategori"),
          shiny::plotOutput(
            ns("category_donut"),
            height = "300px"
          )
        )
      )
    ),
    
    shiny::wellPanel(
      shiny::h4("Perbandingan dan kontribusi kategori"),
      DT::DTOutput(ns("category_table")),
      shiny::textOutput(ns("category_note"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Rincian subkategori"),
      shiny::textOutput(ns("subcategory_note"))
    ),
    
    shiny::wellPanel(
      shiny::h4("Catatan kualitas data"),
      shiny::textOutput(ns("quality_note"))
    )
  )
}
