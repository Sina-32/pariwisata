# functions/data_loader.R
# Membaca, membersihkan, mengubah, dan memvalidasi data kunjungan wisatawan.
# Jalankan dari direktori utama proyek (folder yang berisi pariwisata.Rproj).

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)

RAW_DIR  <- file.path("data", "raw")
PROC_DIR <- file.path("data", "processed")

# Pemetaan kode kategori pada file Excel sumber.
KATEGORI_MAP <- c(
  "I"   = "Wisata Alam",
  "II"  = "Wisata Budaya",
  "III" = "Wisata Buatan"
)

# Subkategori yang tidak dimasukkan ke analisis destinasi.
# Pengecualian diterapkan pada nama subkategori, bukan nama kategori utama.
BLOK_DIKELUARKAN <- "event|mice|olahraga|mall"

# Alias nama destinasi jika sudah dipastikan merujuk pada tempat yang sama.
# Format: "nama lama" = "nama baku".
# Jangan mengisi alias hanya karena dua nama terlihat mirip.
NAMA_ALIAS <- c(
  # "Kaliurang" = "Tlogo Putri"
)

BULAN <- c(
  "Jan", "Feb", "Mar", "Apr", "Mei", "Jun",
  "Jul", "Agu", "Sep", "Okt", "Nov", "Des"
)

KOLOM_BULAN_WNI <- paste0("wni_", BULAN)
KOLOM_BULAN_WNA <- paste0("wna_", BULAN)
KOLOM_ANGKA <- c(
  KOLOM_BULAN_WNI, "wni_total",
  KOLOM_BULAN_WNA, "wna_total", "total"
)

# Mengambil tahun dari nama file. Tidak menggunakan tahun default agar
# file dengan nama yang tidak sesuai tidak salah diberi tahun.
tahun_dari_nama <- function(path) {
  tahun <- stringr::str_extract(basename(path), "20\\d{2}")
  
  if (is.na(tahun)) {
    stop("Tahun tidak ditemukan pada nama file: ", basename(path))
  }
  
  as.integer(tahun)
}

# Mengubah kolom angka tanpa mengubah nilai kosong menjadi nol.
# Nilai yang tidak kosong tetapi gagal dibaca sebagai angka akan diberi peringatan.
konversi_ke_angka <- function(x, nama_kolom) {
  teks <- stringr::str_squish(as.character(x))
  hasil <- suppressWarnings(as.numeric(teks))
  
  nilai_tidak_valid <- !is.na(teks) & teks != "" & is.na(hasil)
  
  if (any(nilai_tidak_valid)) {
    contoh <- unique(teks[nilai_tidak_valid])
    warning(
      "Ada nilai bukan angka pada kolom ", nama_kolom,
      ". Contoh: ", paste(utils::head(contoh, 5), collapse = ", "),
      call. = FALSE
    )
  }
  
  hasil
}

# Membaca satu file Excel dengan format sumber yang digunakan proyek.
baca_satu_file <- function(path) {
  if (!file.exists(path)) {
    stop("File tidak ditemukan: ", path)
  }
  
  tahun <- tahun_dari_nama(path)
  
  raw <- readxl::read_excel(
    path,
    sheet = "Rekap",
    col_names = FALSE,
    skip = 5,
    col_types = "text",
    .name_repair = "minimal"
  )
  
  jumlah_kolom_dibutuhkan <- 30L
  
  if (ncol(raw) < jumlah_kolom_dibutuhkan) {
    stop(
      "Format file ", basename(path), " tidak sesuai. Ditemukan ",
      ncol(raw), " kolom, sedangkan dibutuhkan minimal ",
      jumlah_kolom_dibutuhkan, " kolom."
    )
  }
  
  # Format sumber menggunakan 30 kolom: identitas, WNI, WNA, dan total.
  raw <- raw[, seq_len(jumlah_kolom_dibutuhkan)]
  
  names(raw) <- c(
    "no", "nama", "kapanewon",
    KOLOM_BULAN_WNI, "wni_total",
    KOLOM_BULAN_WNA, "wna_total", "total"
  )
  
  raw <- raw %>%
    mutate(
      across(
        all_of(c("no", "nama", "kapanewon")),
        ~ stringr::str_squish(.x)
      )
    ) %>%
    filter(if_any(
      all_of(c("no", "nama", "kapanewon")),
      ~ !is.na(.x) & .x != ""
    ))
  
  # Baris kategori utama menggunakan angka Romawi pada kolom nomor.
  raw <- raw %>%
    mutate(
      romawi = stringr::str_match(no, "^([IVX]+)\\.?$")[, 2],
      section_id = cumsum(!is.na(romawi)),
      blok = if_else(!is.na(romawi), nama, NA_character_),
      subblok = if_else(
        stringr::str_detect(no, "^[A-Z]$"),
        nama,
        NA_character_
      )
    ) %>%
    fill(romawi, blok, .direction = "down") %>%
    group_by(section_id) %>%
    fill(subblok, .direction = "down") %>%
    ungroup()
  
  # Hentikan pembacaan sebelum baris rekap total/sumber/catatan.
  label_stop <- dplyr::coalesce(raw$nama, raw$no, "")
  indeks_stop <- which(stringr::str_detect(
    stringr::str_to_lower(label_stop),
    "^(jumlah\\s+total|total|sumber|catatan)"
  ))
  
  if (length(indeks_stop) > 0) {
    raw <- raw[seq_len(min(indeks_stop) - 1L), , drop = FALSE]
  }
  
  # Hanya baris destinasi dalam kategori I, II, dan III yang dipertahankan.
  # Baris subkategori MICE, olahraga, event, dan mall dikeluarkan berdasarkan
  # nama subkategori yang diwariskan ke baris destinasi di bawahnya.
  dest <- raw %>%
    filter(
      !is.na(kapanewon), kapanewon != "",
      !is.na(nama), nama != "",
      romawi %in% names(KATEGORI_MAP),
      !stringr::str_detect(
        stringr::str_to_lower(dplyr::coalesce(subblok, "")),
        BLOK_DIKELUARKAN
      )
    ) %>%
    mutate(
      tahun = tahun,
      kategori = unname(KATEGORI_MAP[romawi]),
      destinasi = stringr::str_squish(nama),
      kapanewon = stringr::str_to_title(kapanewon)
    )
  
  # Terapkan alias hanya jika sudah ditentukan pada NAMA_ALIAS.
  if (length(NAMA_ALIAS) > 0) {
    indeks_alias <- match(dest$destinasi, names(NAMA_ALIAS))
    cocok_alias <- !is.na(indeks_alias)
    dest$destinasi[cocok_alias] <- unname(NAMA_ALIAS[indeks_alias[cocok_alias]])
  }
  
  # Konversi kolom numerik. Sel kosong tetap NA, bukan otomatis menjadi 0.
  for (kolom in KOLOM_ANGKA) {
    dest[[kolom]] <- konversi_ke_angka(dest[[kolom]], kolom)
  }
  
  dest %>%
    select(
      tahun, kategori, destinasi, kapanewon,
      all_of(KOLOM_BULAN_WNI), "wni_total",
      all_of(KOLOM_BULAN_WNA), "wna_total", "total"
    )
}

# Membaca seluruh file Excel di data/raw dan menghasilkan format panjang.
build_kunjungan <- function(raw_dir = RAW_DIR, proc_dir = PROC_DIR) {
  if (!dir.exists(raw_dir)) {
    stop("Folder data mentah tidak ditemukan: ", raw_dir)
  }
  
  files <- list.files(
    raw_dir,
    pattern = "\\.xlsx?$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  # Abaikan file sementara Excel seperti ~$nama_file.xlsx.
  files <- files[!stringr::str_detect(basename(files), "^~\\$")]
  
  if (length(files) == 0) {
    stop("Tidak ada file Excel yang ditemukan di folder: ", raw_dir)
  }
  
  tahun_file <- vapply(files, tahun_dari_nama, integer(1))
  
  if (anyDuplicated(tahun_file)) {
    tahun_duplikat <- unique(tahun_file[duplicated(tahun_file)])
    stop(
      "Ditemukan lebih dari satu file untuk tahun: ",
      paste(tahun_duplikat, collapse = ", "),
      ". Periksa isi folder data/raw."
    )
  }
  
  # Urutkan file berdasarkan tahun agar proses dan hasil lebih konsisten.
  files <- files[order(tahun_file)]
  
  daftar_data <- lapply(files, baca_satu_file)
  wide <- bind_rows(daftar_data)
  
  if (nrow(wide) == 0) {
    stop("Tidak ada baris destinasi yang berhasil dibaca dari file sumber.")
  }
  
  long <- wide %>%
    select(
      tahun, kategori, destinasi, kapanewon,
      all_of(KOLOM_BULAN_WNI), all_of(KOLOM_BULAN_WNA)
    ) %>%
    pivot_longer(
      cols = all_of(c(KOLOM_BULAN_WNI, KOLOM_BULAN_WNA)),
      names_to = c("asal", "bulan"),
      names_pattern = "^(wni|wna)_(.+)$",
      values_to = "jumlah"
    ) %>%
    mutate(
      asal = toupper(asal),
      bulan_num = match(bulan, BULAN),
      bulan = factor(bulan, levels = BULAN),
      tanggal = as.Date(sprintf("%d-%02d-01", tahun, bulan_num))
    ) %>%
    arrange(tahun, kategori, destinasi, bulan_num, asal)
  
  dir.create(proc_dir, recursive = TRUE, showWarnings = FALSE)
  
  saveRDS(long, file.path(proc_dir, "kunjungan_long.rds"))
  readr::write_csv(long, file.path(proc_dir, "kunjungan_long.csv"), na = "")
  
  cek_kunjungan(wide, long)
  
  message(
    "Preprocessing selesai. Data tersimpan di: ",
    normalizePath(proc_dir, winslash = "/", mustWork = FALSE)
  )
  
  invisible(long)
}

# Memuat hasil preprocessing dalam format RDS.
load_kunjungan <- function(proc_dir = PROC_DIR) {
  file_rds <- file.path(proc_dir, "kunjungan_long.rds")
  
  if (!file.exists(file_rds)) {
    stop(
      "File hasil preprocessing tidak ditemukan: ", file_rds,
      ". Jalankan build_kunjungan() terlebih dahulu."
    )
  }
  
  data <- readRDS(file_rds)
  kolom_wajib <- c(
    "tahun", "kategori", "destinasi", "kapanewon",
    "asal", "bulan", "jumlah", "bulan_num", "tanggal"
  )
  kolom_hilang <- setdiff(kolom_wajib, names(data))
  
  if (length(kolom_hilang) > 0) {
    stop(
      "Struktur file RDS tidak sesuai. Kolom yang tidak ditemukan: ",
      paste(kolom_hilang, collapse = ", ")
    )
  }
  
  data
}

# Menampilkan ringkasan pemeriksaan hasil preprocessing.
cek_kunjungan <- function(wide, long) {
  tahun_tersedia <- dplyr::n_distinct(wide$tahun)
  
  cat("\n== Jumlah destinasi unik per tahun ==\n")
  print(
    wide %>%
      distinct(tahun, destinasi) %>%
      count(tahun, name = "jumlah_destinasi")
  )
  
  cat("\n== Jumlah destinasi unik per tahun dan kategori ==\n")
  print(
    wide %>%
      distinct(tahun, kategori, destinasi) %>%
      count(tahun, kategori, name = "jumlah_destinasi") %>%
      pivot_wider(
        names_from = kategori,
        values_from = jumlah_destinasi,
        values_fill = 0
      )
  )
  
  cat("\n== Perbandingan total kunjungan bulanan dan kolom total sumber ==\n")
  rekap_bulan <- long %>%
    group_by(tahun) %>%
    summarise(
      total_dari_bulan = sum(jumlah, na.rm = TRUE),
      jumlah_nilai_kosong = sum(is.na(jumlah)),
      .groups = "drop"
    )
  
  rekap_total <- wide %>%
    group_by(tahun) %>%
    summarise(
      total_dari_kolom_sumber = sum(total, na.rm = TRUE),
      .groups = "drop"
    )
  
  print(full_join(rekap_bulan, rekap_total, by = "tahun"))
  
  cat("\n== Jumlah nilai kunjungan negatif ==\n")
  print(sum(long$jumlah < 0, na.rm = TRUE))
  
  cat("\n== Destinasi yang tercatat pada sebagian tahun ==\n")
  print(
    wide %>%
      distinct(tahun, destinasi) %>%
      group_by(destinasi) %>%
      summarise(jumlah_tahun = n_distinct(tahun), .groups = "drop") %>%
      filter(jumlah_tahun < tahun_tersedia) %>%
      arrange(destinasi),
    n = 50
  )
  
  cat("\n== Nama destinasi yang muncul lebih dari sekali dalam tahun/kategori ==\n")
  print(
    wide %>%
      count(tahun, kategori, destinasi, name = "jumlah_baris") %>%
      filter(jumlah_baris > 1) %>%
      arrange(tahun, kategori, destinasi),
    n = 50
  )
  
  cat("\n== Destinasi yang tercatat dalam lebih dari satu kategori ==\n")
  
  print(
    wide %>%
      distinct(tahun, kategori, destinasi) %>%
      group_by(tahun, destinasi) %>%
      summarise(
        jumlah_kategori = n_distinct(kategori),
        daftar_kategori = paste(
          unique(kategori),
          collapse = ", "
        ),
        .groups = "drop"
      ) %>%
      filter(jumlah_kategori > 1)
  )
  
  invisible(TRUE)
}
