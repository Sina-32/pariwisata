# File ini otomatis dijalankan oleh Shiny sebelum app.R.
required_packages <- c(
  "shiny", "bslib", "dplyr", "tidyr", "stringr", "readr", "readxl",
  "ggplot2", "DT", "scales", "tibble", "rlang"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Package berikut belum terpasang: ", paste(missing_packages, collapse = ", "),
    ". Jalankan install.packages(c(", paste(sprintf("'%s'", missing_packages), collapse = ", "), "))."
  )
}

invisible(lapply(required_packages, library, character.only = TRUE))
source("config.R", local = FALSE, encoding = "UTF-8")
source(file.path("functions", "helpers.R"), local = FALSE, encoding = "UTF-8")
source(file.path("functions", "statistics.R"), local = FALSE, encoding = "UTF-8")
source(file.path("functions", "visualization.R"), local = FALSE, encoding = "UTF-8")
source(file.path("functions", "data_loader.R"), local = FALSE, encoding = "UTF-8")

# Modul dimuat sekali agar fungsi UI/server tersedia di app.R.
module_files <- c(
  "modules/dashboard/dashboard_ui.R", "modules/dashboard/dashboard_server.R",
  "modules/destinasi/destinasi_ui.R", "modules/destinasi/destinasi_server.R",
  "modules/tren/tren_ui.R", "modules/tren/tren_server.R",
  "modules/wilayah/wilayah_ui.R", "modules/wilayah/wilayah_server.R",
  "modules/profil_wisatawan/profil_wisatawan_ui.R", "modules/profil_wisatawan/profil_wisatawan_server.R",
  "modules/kategori_wisata/kategori_wisata_ui.R", "modules/kategori_wisata/kategori_wisata_server.R"
)
for (file in module_files) source(file, local = FALSE, encoding = "UTF-8")
