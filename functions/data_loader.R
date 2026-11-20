# functions/data_loader.R
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

RAW_DIR  <- "data/raw"
PROC_DIR <- "data/processed"

KATEGORI_MAP <- c("I" = "Wisata Alam", "II" = "Wisata Budaya", "III" = "Wisata Buatan")
BLOK_DIKELUARKAN <- "event|mice|olahraga|mall"

# Samakan nama destinasi yang berubah antar tahun: "nama lama" = "nama baru"
NAMA_ALIAS <- c(
  # "Kaliurang" = "Tlogo Putri"
)

BULAN <- c("Jan","Feb","Mar","Apr","Mei","Jun","Jul","Agu","Sep","Okt","Nov","Des")

tahun_dari_nama <- function(path) {
  m <- str_extract(basename(path), "20\\d{2}")
  if (is.na(m)) 2024L else as.integer(m)
}

baca_satu_file <- function(path) {
  tahun <- tahun_dari_nama(path)
  
  raw <- read_excel(path, sheet = "Rekap", col_names = FALSE,
                    skip = 5, col_types = "text", .name_repair = "minimal")
  raw <- raw[, 1:30]
  names(raw) <- c("no", "nama", "kapanewon",
                  paste0("wni_", BULAN), "wni_total",
                  paste0("wna_", BULAN), "wna_total",
                  "total")
  
  raw <- raw %>%
    mutate(across(c(no, nama, kapanewon), ~ str_squish(.x))) %>%
    filter(!(is.na(nama) & is.na(kapanewon) & is.na(no)))
  
  raw <- raw %>%
    mutate(
      romawi = str_match(no, "^([IVX]+)\\.?$")[, 2],
      blok   = if_else(!is.na(romawi), nama, NA_character_)
    ) %>%
    fill(romawi, blok, .direction = "down")
  
  idx_stop <- which(str_detect(str_to_lower(coalesce(raw$nama, "")),
                               "^(total|jumlah|sumber|catatan)"))
  idx_stop <- idx_stop[idx_stop > 1]
  if (length(idx_stop) > 0) raw <- raw[seq_len(min(idx_stop) - 1), ]
  
  dest <- raw %>%
    filter(
      !is.na(kapanewon), kapanewon != "",
      !is.na(nama),
      is.na(str_match(no, "^([IVX]+)\\.?$")[, 2]),
      romawi %in% names(KATEGORI_MAP),
      !str_detect(str_to_lower(coalesce(blok, "")), BLOK_DIKELUARKAN)
    ) %>%
    mutate(
      tahun     = tahun,
      kategori  = unname(KATEGORI_MAP[romawi]),
      destinasi = ifelse(is.na(match(nama, names(NAMA_ALIAS))), nama,
                         unname(NAMA_ALIAS)[match(nama, names(NAMA_ALIAS))]),
      kapanewon = str_to_title(kapanewon)
    ) %>%
    mutate(across(starts_with("wni_") | starts_with("wna_") | all_of("total"),
                  ~ replace_na(suppressWarnings(as.numeric(.x)), 0)))
  
  dest %>% select(tahun, kategori, destinasi, kapanewon,
                  starts_with("wni_"), starts_with("wna_"), total)
}

build_kunjungan <- function(raw_dir = RAW_DIR, proc_dir = PROC_DIR) {
  files <- list.files(raw_dir, pattern = "\\.xlsx?$", full.names = TRUE)
  files <- files[!str_detect(basename(files), "^~\\$")]
  if (length(files) == 0) stop("Tidak ada file Excel di ", raw_dir)
  
  wide <- bind_rows(lapply(files, baca_satu_file))
  
  long <- wide %>%
    select(-total, -wni_total, -wna_total) %>%
    pivot_longer(
      cols = matches("^(wni|wna)_(Jan|Feb|Mar|Apr|Mei|Jun|Jul|Agu|Sep|Okt|Nov|Des)$"),
      names_to  = c("asal", "bulan"),
      names_sep = "_",
      values_to = "jumlah"
    ) %>%
    mutate(
      asal      = toupper(asal),
      bulan_num = match(bulan, BULAN),
      bulan     = factor(bulan, levels = BULAN),
      tanggal   = as.Date(sprintf("%d-%02d-01", tahun, bulan_num))
    ) %>%
    arrange(tahun, kategori, destinasi, bulan_num, asal)
  
  dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(long, file.path(proc_dir, "kunjungan_long.rds"))
  write_csv(long, file.path(proc_dir, "kunjungan_long.csv"))
  
  cek_kunjungan(wide, long)
  invisible(long)
}

load_kunjungan <- function(proc_dir = PROC_DIR) {
  f <- file.path(proc_dir, "kunjungan_long.rds")
  if (!file.exists(f)) stop("Jalankan build_kunjungan() dulu.")
  readRDS(f)
}

cek_kunjungan <- function(wide, long) {
  cat("\n== Jumlah destinasi per tahun (awal: 2023=141, 2024=154 termasuk event/MICE, 2025=141) ==\n")
  print(wide %>% count(tahun, name = "n_destinasi"))
  
  cat("\n== Per tahun & kategori ==\n")
  print(wide %>% count(tahun, kategori) %>% pivot_wider(names_from = kategori, values_from = n))
  
  cat("\n== Total dari bulan vs kolom Jum.Total ==\n")
  print(full_join(
    long %>% group_by(tahun) %>% summarise(dari_bulan = sum(jumlah), .groups = "drop"),
    wide %>% group_by(tahun) %>% summarise(dari_kolom_total = sum(total), .groups = "drop"),
    by = "tahun"
  ))
  
  cat("\n== Destinasi yang hanya ada di sebagian tahun ==\n")
  print(wide %>% distinct(tahun, destinasi) %>% group_by(destinasi) %>%
          filter(n() < n_distinct(wide$tahun)) %>% arrange(destinasi, tahun), n = 50)
}