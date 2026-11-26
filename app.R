# ============================================================
# ANALISIS PARIWISATA
# Aplikasi Analisis Statistik Pariwisata Kabupaten Sleman
# Theme: Modern Heritage - Brown & White
# ============================================================

# global.R memuat paket, config.R, fungsi, dan seluruh modul.
source("global.R")

# Batas ukuran file unggahan: 30 MB.
options(shiny.maxRequestSize = 30 * 1024^2)


# ============================================================
# 1. APPLICATION THEME
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
# 2. USER INTERFACE
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
        shiny::h2(APP_TITLE),
        shiny::p(APP_SUBTITLE)
      )
    ),
    
    
    # --------------------------------------------------------
    # TOOLBAR: UNGGAH DATA + FILTER TAHUN
    # --------------------------------------------------------
    
    shiny::div(
      class = "global-toolbar",
      
      shiny::div(
        class = "upload-filter",
        
        shiny::fileInput(
          "file_data",
          "Data kunjungan (.xlsx)",
          multiple = TRUE,
          accept = c(".xlsx", ".xls"),
          buttonLabel = "Pilih file",
          placeholder = "Belum ada file",
          width = "300px"
        )
      ),
      
      shiny::div(
        class = "year-filter",
        
        shiny::selectInput(
          "tahun_global",
          "Tahun analisis",
          choices = "Tidak tersedia",
          selected = "Tidak tersedia",
          width = "180px"
        )
      ),
      
      shiny::div(
        class = "toolbar-caption",
        "Unggah satu file Excel untuk setiap tahun. Pilihan tahun digunakan bersama pada seluruh halaman analisis."
      )
    ),
    
    
    # --------------------------------------------------------
    # DATA STATUS
    # --------------------------------------------------------
    
    shiny::uiOutput("data_status"),
    
    
    # --------------------------------------------------------
    # TAMPILAN AWAL (SEBELUM ADA DATA)
    # --------------------------------------------------------
    
    shiny::conditionalPanel(
      condition = "!output.data_ready",
      
      shiny::div(
        class = "empty-state",
        
        shiny::h3("Unggah data untuk memulai"),
        
        shiny::p(
          "Pilih file Excel kunjungan wisatawan pada kolom \"Data kunjungan\" di atas. ",
          "Beberapa file bisa dipilih sekaligus untuk membandingkan beberapa tahun."
        ),
        
        shiny::tags$ul(
          shiny::tags$li(
            "Format file: Excel (.xlsx atau .xls) dengan sheet bernama \"Rekap\", sama seperti data Dinas Pariwisata Kabupaten Sleman."
          ),
          shiny::tags$li(
            "Nama file harus memuat tahun, misalnya Kunjungan_2025.xlsx."
          ),
          shiny::tags$li(
            "Satu file untuk setiap tahun."
          )
        )
      )
    ),
    
    
    # --------------------------------------------------------
    # MAIN NAVIGATION (SELALU TAMPIL)
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
      
      "Analisis Pariwisata · Aplikasi analisis statistik kunjungan wisatawan · Data bersumber dari file yang diunggah pengguna"
    )
  )
)


# ============================================================
# 3. SERVER
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # STATE DATA (per sesi pengguna, tidak disimpan ke disk)
  # ----------------------------------------------------------
  
  app_data     <- shiny::reactiveVal(NULL)
  app_info     <- shiny::reactiveVal(NULL)
  upload_error <- shiny::reactiveVal(NULL)
  
  
  # ----------------------------------------------------------
  # PROSES UNGGAH FILE
  # ----------------------------------------------------------
  
  shiny::observeEvent(input$file_data, {
    
    berkas <- input$file_data
    
    hasil <- tryCatch(
      shiny::withProgress(
        message = "Membaca file Excel...",
        value = 0.5,
        baca_unggahan(berkas$datapath, berkas$name)
      ),
      unggahan_gagal = function(e) {
        upload_error(e$gagal)
        NULL
      },
      error = function(e) {
        upload_error(conditionMessage(e))
        NULL
      }
    )
    
    # Jika gagal, data sebelumnya (kalau ada) tetap dipakai.
    if (is.null(hasil)) {
      return()
    }
    
    # File yang gagal dilewati, file lain tetap dimuat.
    upload_error(if (length(hasil$gagal) > 0) hasil$gagal else NULL)
    app_data(hasil$data)
    app_info(list(
      files = hasil$dimuat,
      peringatan = hasil$peringatan
    ))
    
    tahun <- sort(unique(hasil$data$tahun))
    
    shiny::updateSelectInput(
      session,
      "tahun_global",
      choices = as.character(tahun),
      selected = as.character(max(tahun))
    )
  })
  
  
  # ----------------------------------------------------------
  # PENANDA DATA SIAP (dipakai conditionalPanel di UI)
  # ----------------------------------------------------------
  
  output$data_ready <- shiny::reactive(!is.null(app_data()))
  shiny::outputOptions(output, "data_ready", suspendWhenHidden = FALSE)
  
  
  # ----------------------------------------------------------
  # SHARED DATA
  # ----------------------------------------------------------
  
  data_shared <- shiny::reactive({
    
    shiny::req(
      !is.null(app_data()),
      nrow(app_data()) > 0
    )
    
    app_data()
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
    
    pesan_error <- NULL
    pesan_sukses <- NULL
    
    if (!is.null(upload_error())) {
      
      pesan_error <- shiny::div(
        class = "notice notice-danger",
        
        shiny::strong(
          if (is.null(app_data())) {
            "File belum dapat dimuat."
          } else {
            "Sebagian file tidak dapat dimuat."
          }
        ),
        
        shiny::tags$ul(
          lapply(upload_error(), shiny::tags$li)
        )
      )
    }
    
    d <- app_data()
    
    if (!is.null(d)) {
      
      q <- quality_summary(d)
      info <- app_info()
      
      pesan_sukses <- shiny::div(
        class = "notice notice-success",
        
        shiny::p(
          paste0(
            "Data berhasil dimuat: ",
            fmt_number(length(unique(d$tahun))),
            " tahun, ",
            fmt_number(q$rows),
            " baris pengamatan bulanan."
          )
        ),
        
        shiny::p(
          class = "notice-detail",
          paste("File:", paste(info$files, collapse = ", "))
        ),
        
        if (length(info$peringatan) > 0) {
          shiny::tags$details(
            shiny::tags$summary(
              paste0(length(info$peringatan), " catatan saat membaca file")
            ),
            shiny::tags$ul(
              lapply(info$peringatan, shiny::tags$li)
            )
          )
        }
      )
    }
    
    shiny::tagList(pesan_error, pesan_sukses)
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
# 4. RUN APPLICATION
# ============================================================

shiny::shinyApp(
  ui = ui,
  server = server
)