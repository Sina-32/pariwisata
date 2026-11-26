
# ============================================================
# SLEMAN TOURISM INSIGHT
# Aplikasi Analisis Statistik Pariwisata Kabupaten Sleman
# Theme: Modern Heritage - Brown & White
# ============================================================


# ============================================================
# 1. LOAD DATA
# ============================================================

load_result <- tryCatch(
  list(data = load_app_data(), error = NULL),
  error = function(e) list(data = NULL, error = conditionMessage(e))
)

APP_DATA <- load_result$data
APP_DATA_ERROR <- load_result$error

APP_YEARS <- if (!is.null(APP_DATA) && nrow(APP_DATA) > 0) {
  sort(unique(as.integer(APP_DATA$tahun)))
} else {
  integer(0)
}


# ============================================================
# 2. APPLICATION THEME
# ============================================================

app_theme <- bslib::bs_theme(
  version = 5,
  bootswatch = "flatly",
  
  primary = "#795548",
  secondary = "#A98262",
  success = "#8C735B",
  
  bg = "#FAF8F5",
  fg = "#352820"
)


# ============================================================
# 3. USER INTERFACE
# ============================================================

ui <- shiny::fluidPage(
  
  theme = app_theme,
  
  shiny::tags$head(
    
    shiny::tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    
    # Custom CSS
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "css/style.css"
    ),
    
    shiny::tags$title(APP_TITLE)
  ),
  
  # MAIN APPLICATION CONTAINER
  shiny::div(
    class = "app-shell",
    
    # --------------------------------------------------------
    # HEADER
    # --------------------------------------------------------
    
    shiny::div(
      class = "app-header",
      
      shiny::div(
        class = "brand-mark",
        "STI"
      ),
      
      shiny::div(
        shiny::h2(APP_TITLE),
        shiny::p(APP_SUBTITLE)
      )
    ),
    
    
    # --------------------------------------------------------
    # GLOBAL TOOLBAR
    # --------------------------------------------------------
    
    shiny::div(
      class = "global-toolbar",
      
      shiny::div(
        class = "year-filter",
        
        shiny::selectInput(
          "tahun_global",
          "Tahun analisis",
          
          choices = if (length(APP_YEARS)) {
            as.character(APP_YEARS)
          } else {
            "Tidak tersedia"
          },
          
          selected = if (length(APP_YEARS)) {
            as.character(max(APP_YEARS))
          } else {
            "Tidak tersedia"
          },
          
          width = "180px"
        )
      ),
      
      shiny::div(
        class = "toolbar-caption",
        "Pilihan tahun digunakan bersama pada seluruh halaman analisis."
      )
    ),
    
    
    # --------------------------------------------------------
    # DATA STATUS
    # --------------------------------------------------------
    
    shiny::uiOutput("data_status"),
    
    
    # --------------------------------------------------------
    # MAIN NAVIGATION
    # --------------------------------------------------------
    
    shiny::tabsetPanel(
      
      id = "main_navigation",
      type = "tabs",
      
      shiny::tabPanel(
        "Dashboard",
        dashboard_ui("dashboard")
      ),
      
      shiny::tabPanel(
        "Tren Kunjungan",
        tren_ui("tren")
      ),
      
      shiny::tabPanel(
        "Profil Wisatawan",
        profil_wisatawan_ui("profil_wisatawan")
      ),
      
      shiny::tabPanel(
        "Kategori Wisata",
        kategori_wisata_ui("kategori_wisata")
      ),
      
      shiny::tabPanel(
        "Analisis Destinasi",
        destinasi_ui("destinasi")
      ),
      
      shiny::tabPanel(
        "Analisis Wilayah",
        wilayah_ui("wilayah")
      )
    ),
    
    
    # --------------------------------------------------------
    # FOOTER
    # --------------------------------------------------------
    
    shiny::div(
      class = "app-footer",
      
      "Sleman Tourism Insight · Aplikasi analisis statistik kunjungan wisatawan · Data bersumber dari dokumen Dinas Pariwisata Kabupaten Sleman"
    )
  )
)


# ============================================================
# 4. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # SHARED DATA
  # ----------------------------------------------------------
  
  data_shared <- shiny::reactive({
    
    shiny::req(
      !is.null(APP_DATA),
      nrow(APP_DATA) > 0
    )
    
    APP_DATA
  })
  
  
  # ----------------------------------------------------------
  # GLOBAL YEAR FILTER
  # ----------------------------------------------------------
  
  selected_year <- shiny::reactive({
    
    if (
      is.null(input$tahun_global) ||
      input$tahun_global == "Tidak tersedia"
    ) {
      return(NA_integer_)
    }
    
    as.integer(input$tahun_global)
  })
  
  
  # ----------------------------------------------------------
  # DATA STATUS INFORMATION
  # ----------------------------------------------------------
  
  output$data_status <- shiny::renderUI({
    
    if (!is.null(APP_DATA_ERROR)) {
      
      return(
        shiny::div(
          class = "notice notice-danger",
          
          shiny::strong("Data belum dapat dimuat."),
          
          shiny::p(APP_DATA_ERROR),
          
          shiny::p(
            "Pastikan file Excel sumber tersedia di data/raw atau file hasil preprocessing tersedia di data/processed. Jalankan aplikasi dari folder utama proyek."
          )
        )
      )
    }
    
    q <- quality_summary(APP_DATA)
    
    shiny::div(
      class = "notice notice-success",
      
      paste0(
        "Data berhasil dimuat: ",
        fmt_number(length(unique(APP_DATA$tahun))),
        " tahun, ",
        fmt_number(q$rows),
        " baris pengamatan bulanan."
      )
    )
  })
  
  
  # ----------------------------------------------------------
  # MODULE SERVERS
  # ----------------------------------------------------------
  
  dashboard_server(
    "dashboard",
    data_shared,
    selected_year
  )
  
  tren_server(
    "tren",
    data_shared,
    selected_year
  )
  
  profil_wisatawan_server(
    "profil_wisatawan",
    data_shared,
    selected_year
  )
  
  kategori_wisata_server(
    "kategori_wisata",
    data_shared,
    selected_year
  )
  
  destinasi_server(
    "destinasi",
    data_shared,
    selected_year
  )
  
  wilayah_server(
    "wilayah",
    data_shared,
    selected_year
  )
  
}


# ============================================================
# 5. RUN APPLICATION
# ============================================================

shiny::shinyApp(
  ui = ui,
  server = server
)
