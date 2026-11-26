
dashboard_ui <- function(id) {
  ns <- shiny::NS(id)
  
  shiny::tagList(
    
    # Judul dashboard
    shiny::div(
      class = "dashboard-heading",
      shiny::h3("Ringkasan Pariwisata"),
      shiny::p(
        "Gambaran umum statistik kunjungan wisatawan Kabupaten Sleman."
      )
    ),
    
    # Insight utama
    shiny::uiOutput(ns("insight")),
    
    # Kartu indikator
    shiny::fluidRow(
      shiny::column(
        3,
        shiny::div(
          class = "dashboard-kpi kpi-blue",
          shiny::div(class = "kpi-label", "Total Kunjungan"),
          shiny::uiOutput(ns("total_visits"))
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "dashboard-kpi kpi-green",
          shiny::div(class = "kpi-label", "Destinasi Lengkap"),
          shiny::uiOutput(ns("destination_count"))
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "dashboard-kpi kpi-purple",
          shiny::div(class = "kpi-label", "Rata-rata Bulanan"),
          shiny::uiOutput(ns("monthly_average"))
        )
      ),
      shiny::column(
        3,
        shiny::div(
          class = "dashboard-kpi kpi-orange",
          shiny::div(class = "kpi-label", "Bulan Tertinggi"),
          shiny::uiOutput(ns("highest_month"))
        )
      )
    ),
    
    # Grafik utama
    shiny::fluidRow(
      shiny::column(
        8,
        shiny::div(
          class = "dashboard-card",
          shiny::h4("Tren Kunjungan Wisatawan"),
          shiny::p(
            class = "card-description",
            "Perkembangan jumlah kunjungan setiap bulan."
          ),
          shiny::plotOutput(
            ns("monthly_plot"),
            height = "310px"
          )
        )
      ),
      shiny::column(
        4,
        shiny::div(
          class = "dashboard-card",
          shiny::h4("Komposisi Kategori"),
          shiny::p(
            class = "card-description",
            "Kontribusi masing-masing kategori wisata."
          ),
          shiny::plotOutput(
            ns("category_plot"),
            height = "310px"
          )
        )
      )
    ),
    
    # Destinasi unggulan
    shiny::fluidRow(
      shiny::column(
        12,
        shiny::div(
          class = "dashboard-card",
          shiny::h4("5 Destinasi Unggulan"),
          shiny::p(
            class = "card-description",
            "Destinasi dengan jumlah kunjungan tertinggi berdasarkan data lengkap."
          ),
          shiny::tableOutput(ns("top_destinations"))
        )
      )
    ),
    
    # Informasi sumber data dan kualitas
    shiny::fluidRow(
      shiny::column(
        6,
        shiny::div(
          class = "dashboard-card info-card",
          shiny::h4("Periode dan Sumber Data"),
          shiny::textOutput(ns("period")),
          shiny::p(SOURCE_DESCRIPTION)
        )
      ),
      shiny::column(
        6,
        shiny::div(
          class = "dashboard-card info-card",
          shiny::h4("Kualitas Data"),
          shiny::textOutput(ns("quality_note"))
        )
      )
    )
  )
}
