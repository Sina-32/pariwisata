# Fungsi bantuan bersama untuk seluruh modul.

fmt_number <- function(x, digits = 0) {
  if (length(x) == 0 || is.na(x[1])) return("Tidak tersedia")
  format(round(x[1], digits), big.mark = ".", decimal.mark = ",", scientific = FALSE, trim = TRUE)
}

fmt_percent <- function(x, digits = 1) {
  if (length(x) == 0 || is.na(x[1]) || !is.finite(x[1])) return("Tidak tersedia")
  paste0(format(round(x[1], digits), decimal.mark = ",", nsmall = digits), "%")
}

safe_sum <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) return(NA_real_)
  sum(x)
}

filter_year_data <- function(data, year) {
  if (is.null(data) || nrow(data) == 0) return(data[0, , drop = FALSE])
  if (is.null(year) || identical(year, "Semua")) return(data)
  dplyr::filter(data, .data$tahun == as.integer(year))
}

# Menghitung jumlah per bulan. Nilai yang diketahui dijumlahkan; banyaknya
# nilai NA tetap disimpan agar jumlah parsial tidak disalahartikan sebagai total pasti.
monthly_totals <- function(data, year = NULL) {
  d <- filter_year_data(data, year)
  if (nrow(d) == 0) {
    return(data.frame(bulan = factor(MONTHS_ID, levels = MONTHS_ID), bulan_num = 1:12,
                      total = NA_real_, nilai_kosong = NA_integer_))
  }
  dplyr::as_tibble(d) |>
    dplyr::group_by(.data$bulan, .data$bulan_num) |>
    dplyr::summarise(
      total = safe_sum(.data$jumlah),
      nilai_kosong = sum(is.na(.data$jumlah)),
      .groups = "drop"
    ) |>
    dplyr::right_join(
      tibble::tibble(bulan = factor(MONTHS_ID, levels = MONTHS_ID), bulan_num = 1:12),
      by = c("bulan", "bulan_num")
    ) |>
    dplyr::arrange(.data$bulan_num)
}

# Ringkasan destinasi. Kombinasi destinasi-kategori-Kapanewon dipertahankan
# agar nama yang sama di lokasi berbeda tidak dilebur tanpa verifikasi.
destination_totals <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble(
    destinasi = character(), kategori = character(), kapanewon = character(),
    total = double(), nilai_kosong = integer(), duplikasi = logical(),
    jumlah_kombinasi = integer(), lengkap = logical()
  ))
  d <- dplyr::as_tibble(data)
  keys <- c("destinasi", "kategori", "kapanewon", "asal", "bulan")
  per_month <- d |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      n_baris = dplyr::n(),
      jumlah = if (dplyr::n() == 1) dplyr::first(.data$jumlah) else NA_real_,
      .groups = "drop"
    )
  per_month |>
    dplyr::group_by(.data$destinasi, .data$kategori, .data$kapanewon) |>
    dplyr::summarise(
      total = if (any(.data$n_baris > 1)) NA_real_ else safe_sum(.data$jumlah),
      nilai_kosong = sum(is.na(.data$jumlah)),
      duplikasi = any(.data$n_baris > 1),
      jumlah_kombinasi = dplyr::n(),
      lengkap = dplyr::n() == 24 && all(!is.na(.data$jumlah)) && !any(.data$n_baris > 1),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$total))
}

quality_summary <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(list(rows = 0L, missing = 0L, dash = 0L, invalid = 0L,
                legacy_unknown = 0L, zeros = 0L, duplicates = 0L, missing_area = 0L))
  }
  status <- as.character(data$status_nilai)
  list(
    rows = nrow(data),
    missing = sum(status == "kosong", na.rm = TRUE),
    dash = sum(status == "tanda_minus", na.rm = TRUE),
    invalid = sum(status == "tidak_valid", na.rm = TRUE),
    legacy_unknown = sum(status == "status_lama_tidak_diketahui", na.rm = TRUE),
    zeros = sum(status == "nol", na.rm = TRUE),
    duplicates = sum(duplicated(data[c("tahun", "kategori", "destinasi", "kapanewon", "asal", "bulan")])),
    missing_area = sum(is.na(data$kapanewon) | trimws(data$kapanewon) == "")
  )
}

quality_note <- function(data, short = FALSE) {
  q <- quality_summary(data)
  if (q$rows == 0) return("Tidak ada data untuk ditampilkan.")
  if (short) {
    return(paste0("Nilai kosong: ", q$missing, " | tanda '-': ", q$dash,
                  " | nilai tidak valid: ", q$invalid,
                  " | status lama tidak diketahui: ", q$legacy_unknown))
  }
  paste0(
    "Pemeriksaan data: ", q$missing, " nilai kosong, ", q$dash,
    " tanda '-', ", q$invalid, " nilai tidak valid, ", q$zeros,
    " nilai nol, ", q$legacy_unknown, " nilai dari file olahan lama yang status aslinya tidak diketahui, ",
    q$duplicates, " baris duplikat pada kunci analisis, dan ",
    q$missing_area, " baris tanpa Kapanewon. Nilai kosong/tanda '-' tidak dianggap nol."
  )
}

# Hasil agregasi tahunan yang tidak menyembunyikan adanya nilai tidak lengkap.
annual_totals <- function(data) {
  if (nrow(data) == 0) return(tibble::tibble())
  data |>
    dplyr::group_by(.data$tahun) |>
    dplyr::summarise(
      total = safe_sum(.data$jumlah),
      nilai_kosong = sum(is.na(.data$jumlah)),
      jumlah_destinasi = dplyr::n_distinct(.data$destinasi),
      kategori = paste(sort(unique(stats::na.omit(.data$kategori))), collapse = ", "),
      bulan_tersedia = dplyr::n_distinct(.data$bulan[!is.na(.data$jumlah)]),
      duplikasi = any(duplicated(paste(.data$kategori, .data$destinasi,
                                       .data$kapanewon, .data$asal,
                                       as.character(.data$bulan), sep = "|"))),
      lengkap = all(!is.na(.data$jumlah)) && dplyr::n_distinct(.data$bulan) == 12 &&
        !any(duplicated(paste(.data$kategori, .data$destinasi, .data$kapanewon,
                              .data$asal, as.character(.data$bulan), sep = "|"))),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$tahun)
}
