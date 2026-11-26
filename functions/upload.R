# functions/upload.R
# Membaca file Excel yang diunggah pengguna dan mengubahnya menjadi format
# panjang yang sama dengan data olahan sebelumnya. Tidak ada yang disimpan ke disk.

# Membaca satu atau beberapa file Excel unggahan.
#   paths : lokasi sementara file (input$file_data$datapath)
#   names : nama asli file (input$file_data$name); tahun diambil dari nama ini
# Setiap file diperiksa sendiri-sendiri: file yang bermasalah dilewati dan
# dicatat, sedangkan file yang valid tetap dimuat.
# Hasil: list(data, peringatan, dimuat, gagal).
# Jika tidak ada satu pun file yang berhasil, error dilempar dengan
# class "unggahan_gagal" dan daftar alasan per file di elemen $gagal.
baca_unggahan <- function(paths, names) {
  if (length(paths) == 0) {
    stop("Tidak ada file yang dipilih.", call. = FALSE)
  }
  
  names <- basename(names)
  
  # baca_satu_file() mengambil tahun dari nama file, jadi salin berkas
  # sementara dengan nama aslinya.
  folder <- tempfile("unggah_")
  dir.create(folder)
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  
  peringatan <- character()
  gagal <- character()
  hasil <- list()
  tahun_dipakai <- integer()
  dimuat <- character()
  
  for (i in seq_along(paths)) {
    nama <- names[i]
    
    alasan <- tryCatch({
      
      if (!grepl("\\.xlsx?$", nama, ignore.case = TRUE)) {
        stop("Bukan file Excel (.xlsx atau .xls).", call. = FALSE)
      }
      
      tahun <- stringr::str_extract(nama, "20\\d{2}")
      if (is.na(tahun)) {
        stop(
          "Tahun tidak ditemukan pada nama file. ",
          "Tambahkan tahun, misalnya Kunjungan_2025.xlsx.",
          call. = FALSE
        )
      }
      tahun <- as.integer(tahun)
      
      if (tahun %in% tahun_dipakai) {
        stop(
          "Tahun ", tahun, " sudah dimuat dari file lain. ",
          "Unggah satu file untuk setiap tahun.",
          call. = FALSE
        )
      }
      
      tujuan <- file.path(folder, nama)
      if (!file.copy(paths[i], tujuan, overwrite = TRUE)) {
        stop("File gagal diproses. Coba unggah ulang.", call. = FALSE)
      }
      
      data_file <- withCallingHandlers(
        tryCatch(
          baca_satu_file(tujuan),
          error = function(e) {
            pesan <- conditionMessage(e)
            if (grepl("not found", pesan, fixed = TRUE)) {
              pesan <- "Sheet \"Rekap\" tidak ditemukan"
            }
            pesan <- sub("[.]?\\s*$", ".", pesan)
            stop(
              pesan, " Pastikan format sama dengan data Dinas Pariwisata ",
              "(sheet \"Rekap\").",
              call. = FALSE
            )
          }
        ),
        warning = function(w) {
          peringatan <<- c(peringatan, paste0(nama, ": ", conditionMessage(w)))
          invokeRestart("muffleWarning")
        }
      )
      
      if (nrow(data_file) == 0) {
        stop("Tidak ada baris destinasi yang berhasil dibaca.", call. = FALSE)
      }
      
      hasil[[length(hasil) + 1]] <- data_file
      tahun_dipakai <- c(tahun_dipakai, tahun)
      dimuat <- c(dimuat, nama)
      NULL
      
    }, error = function(e) conditionMessage(e))
    
    if (!is.null(alasan)) {
      gagal[nama] <- alasan
    }
  }
  
  if (length(hasil) == 0) {
    stop(structure(
      class = c("unggahan_gagal", "error", "condition"),
      list(
        message = paste(
          paste0(names(gagal), ": ", gagal), collapse = " | "
        ),
        call = NULL,
        gagal = paste0(names(gagal), ": ", gagal)
      )
    ))
  }
  
  daftar_data <- hasil
  
  wide <- dplyr::bind_rows(daftar_data)
  
  if (nrow(wide) == 0) {
    stop(
      "Tidak ada baris destinasi yang berhasil dibaca. ",
      "Periksa kembali isi file.",
      call. = FALSE
    )
  }
  
  long <- wide |>
    dplyr::select(
      tahun, kategori, destinasi, kapanewon,
      dplyr::all_of(KOLOM_BULAN_WNI), dplyr::all_of(KOLOM_BULAN_WNA)
    ) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(c(KOLOM_BULAN_WNI, KOLOM_BULAN_WNA)),
      names_to = c("asal", "bulan"),
      names_pattern = "^(wni|wna)_(.+)$",
      values_to = "jumlah"
    ) |>
    dplyr::mutate(
      asal = toupper(asal),
      bulan_num = match(bulan, BULAN),
      bulan = factor(bulan, levels = MONTHS_ID),
      tanggal = as.Date(sprintf("%d-%02d-01", tahun, bulan_num)),
      # Sel kosong dicatat sebagai "kosong", bukan dianggap nol.
      status_nilai = dplyr::if_else(is.na(jumlah), "kosong", "valid")
    ) |>
    dplyr::arrange(tahun, kategori, destinasi, bulan_num, asal)
  
  long <- tibble::as_tibble(long)
  long$tahun <- as.integer(long$tahun)
  long$jumlah <- suppressWarnings(as.numeric(long$jumlah))
  long$bulan_num <- as.integer(long$bulan_num)
  
  list(
    data = long,
    peringatan = unique(peringatan),
    dimuat = dimuat,
    gagal = if (length(gagal)) paste0(names(gagal), ": ", gagal) else character()
  )
}