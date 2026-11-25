# Fungsi visualisasi bersama. Semua grafik memakai data yang telah difilter modul.

base_plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 13),
      plot.subtitle = ggplot2::element_text(color = "#64748b"),
      axis.title = ggplot2::element_text(color = "#475569"),
      panel.grid.minor = ggplot2::element_blank(),
      legend.position = "bottom"
    )
}

plot_monthly_trend <- function(monthly, title = "Tren kunjungan bulanan") {
  ggplot2::ggplot(monthly, ggplot2::aes(x = .data$bulan_num, y = .data$total, group = 1)) +
    ggplot2::geom_line(color = "#176B87", linewidth = 1) +
    ggplot2::geom_point(color = "#176B87", size = 2.5) +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = MONTHS_ID) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(title = title, x = NULL, y = "Jumlah kunjungan") +
    base_plot_theme()
}

plot_top_destinations <- function(data, title = "10 destinasi dengan kunjungan tertinggi") {
  if (nrow(data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Belum ada data lengkap") + base_plot_theme())
  data <- data |>
    dplyr::slice_max(order_by = .data$total, n = 10, with_ties = FALSE) |>
    dplyr::mutate(label = factor(.data$destinasi, levels = rev(.data$destinasi)))
  ggplot2::ggplot(data, ggplot2::aes(x = .data$label, y = .data$total)) +
    ggplot2::geom_col(fill = "#176B87", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(title = title, x = NULL, y = "Jumlah kunjungan") +
    base_plot_theme()
}

plot_area_visits <- function(data) {
  if (nrow(data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Data Kapanewon belum tersedia") + base_plot_theme())
  data <- data |>
    dplyr::mutate(kapanewon = factor(.data$kapanewon, levels = rev(.data$kapanewon)))
  ggplot2::ggplot(data, ggplot2::aes(x = .data$kapanewon, y = .data$total)) +
    ggplot2::geom_col(fill = "#2A9D8F", width = 0.72) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(title = "Perbandingan kunjungan per Kapanewon", x = NULL, y = "Jumlah kunjungan") +
    base_plot_theme()
}

plot_profile_monthly <- function(data) {
  if (nrow(data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Data profil wisatawan belum tersedia") + base_plot_theme())
  ggplot2::ggplot(data, ggplot2::aes(x = .data$bulan_num, y = .data$total, fill = .data$asal)) +
    ggplot2::geom_col() +
    ggplot2::scale_x_continuous(breaks = 1:12, labels = MONTHS_ID) +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::scale_fill_manual(values = c("WNI" = "#176B87", "WNA" = "#E9A23B")) +
    ggplot2::labs(title = "Komposisi WNI dan WNA setiap bulan", x = NULL, y = "Jumlah kunjungan", fill = "Kelompok") +
    base_plot_theme()
}

plot_donut <- function(data, label_col = "kelompok", title = "Komposisi kunjungan") {
  if (nrow(data) == 0 || all(is.na(data$total))) return(ggplot2::ggplot() + ggplot2::labs(title = "Data belum tersedia") + base_plot_theme())
  ggplot2::ggplot(data, ggplot2::aes(x = 2, y = .data$total, fill = .data[[label_col]])) +
    ggplot2::geom_col(width = 0.9, color = "white") +
    ggplot2::coord_polar(theta = "y") +
    ggplot2::xlim(0.5, 2.5) +
    ggplot2::labs(title = title, fill = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 13), legend.position = "bottom")
}

plot_category_visits <- function(data) {
  if (nrow(data) == 0) return(ggplot2::ggplot() + ggplot2::labs(title = "Kategori belum tersedia") + base_plot_theme())
  data <- data |>
    dplyr::mutate(kategori = factor(.data$kategori, levels = .data$kategori[order(.data$total)]))
  ggplot2::ggplot(data, ggplot2::aes(x = .data$kategori, y = .data$total)) +
    ggplot2::geom_col(fill = "#8A5FBF", width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::label_number(big.mark = ".", decimal.mark = ",")) +
    ggplot2::labs(title = "Jumlah kunjungan per kategori", x = NULL, y = "Jumlah kunjungan") +
    base_plot_theme()
}
