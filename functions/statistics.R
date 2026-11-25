# Perhitungan statistik yang digunakan di seluruh halaman.

calc_descriptive <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  if (length(x) == 0) {
    return(tibble::tibble(n = 0L, mean = NA_real_, median = NA_real_,
                          minimum = NA_real_, maximum = NA_real_,
                          rentang = NA_real_, sd = NA_real_))
  }
  tibble::tibble(
    n = length(x),
    mean = mean(x),
    median = stats::median(x),
    minimum = min(x),
    maximum = max(x),
    rentang = max(x) - min(x),
    sd = if (length(x) > 1) stats::sd(x) else NA_real_
  )
}

calc_proportions <- function(data, group_col, value_col = "jumlah") {
  group_col <- rlang::ensym(group_col)
  value_col <- rlang::ensym(value_col)
  data |>
    dplyr::group_by(!!group_col) |>
    dplyr::summarise(total = safe_sum(!!value_col), .groups = "drop") |>
    dplyr::mutate(
      persentase = 100 * .data$total / sum(.data$total, na.rm = TRUE),
      probabilitas_empiris = .data$persentase / 100
    ) |>
    dplyr::arrange(dplyr::desc(.data$total))
}

calc_empirical_probability <- function(group_total, all_total) {
  if (is.na(group_total) || is.na(all_total) || all_total <= 0) return(NA_real_)
  group_total / all_total
}

calc_monthly_statistics <- function(data, year) {
  monthly <- monthly_totals(data, year)
  calc_descriptive(monthly$total)
}

calc_growth_rates <- function(annual_data) {
  if (nrow(annual_data) == 0) return(tibble::tibble())
  annual_data |>
    dplyr::arrange(.data$tahun) |>
    dplyr::mutate(
      tahun_sebelumnya = dplyr::lag(.data$tahun),
      total_sebelumnya = dplyr::lag(.data$total),
      growth_rate = dplyr::if_else(
        !is.na(.data$tahun_sebelumnya) & .data$tahun - .data$tahun_sebelumnya == 1 &
          .data$lengkap & dplyr::lag(.data$lengkap) &
          .data$kategori == dplyr::lag(.data$kategori) &
          !is.na(.data$total_sebelumnya) & .data$total_sebelumnya > 0,
        100 * (.data$total - .data$total_sebelumnya) / .data$total_sebelumnya,
        NA_real_
      )
    )
}
