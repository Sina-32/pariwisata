
wilayah_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {

    selected_data <- shiny::reactive({
      d <- filter_year_data(data(), selected_year())

      d |>
        dplyr::filter(
          !is.na(.data$kapanewon),
          trimws(.data$kapanewon) != ""
        )
    })

    area_summary <- shiny::reactive({
      d <- selected_data()

      if (nrow(d) == 0) {
        return(tibble::tibble())
      }

      duplicate_keys <- d |>
        dplyr::count(
          .data$kapanewon,
          .data$kategori,
          .data$destinasi,
          .data$asal,
          .data$bulan,
          name = "n"
        ) |>
        dplyr::filter(.data$n > 1)

      duplicate_areas <- unique(
        duplicate_keys$kapanewon
      )

      result <- d |>
        dplyr::group_by(.data$kapanewon) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          nilai_kosong = sum(is.na(.data$jumlah)),
          jumlah_destinasi = dplyr::n_distinct(.data$destinasi),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          duplikasi = .data$kapanewon %in% duplicate_areas,
          lengkap = .data$nilai_kosong == 0 &
            !.data$duplikasi
        ) |>
        dplyr::arrange(dplyr::desc(.data$total))

      all_complete <- nrow(result) > 0 &&
        all(result$lengkap) &&
        !is.na(safe_sum(result$total)) &&
        safe_sum(result$total) > 0

      result$kontribusi <- if (all_complete) {
        100 * result$total / sum(result$total)
      } else {
        NA_real_
      }

      result$peringkat <- ifelse(
        result$lengkap,
        rank(-result$total, ties.method = "min"),
        NA_real_
      )

      result
    })

    output$area_plot <- shiny::renderPlot({
      summary <- area_summary() |>
        dplyr::filter(
          .data$lengkap,
          !is.na(.data$total)
        ) |>
        dplyr::arrange(.data$total)

      if (nrow(summary) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Belum ada Kapanewon dengan data lengkap"
            ) +
            base_plot_theme()
        )
      }

      plot_area_visits(summary)
    })

    output$area_table <- DT::renderDT({
      summary <- area_summary()

      if (nrow(summary) == 0) {
        return(
          DT::datatable(
            data.frame(
              Pesan = "Data Kapanewon belum tersedia."
            ),
            rownames = FALSE
          )
        )
      }

      result <- data.frame(
        Peringkat = summary$peringkat,
        Kapanewon = summary$kapanewon,
        `Jumlah tercatat` = summary$total,
        `Destinasi unik` = summary$jumlah_destinasi,
        `Nilai kosong` = summary$nilai_kosong,
        `Kontribusi (%)` = summary$kontribusi,
        Status = ifelse(
          summary$lengkap,
          "Lengkap",
          "Perlu diperiksa"
        ),
        check.names = FALSE
      )

      DT::datatable(
        result,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })

    output$area_note <- shiny::renderText({
      summary <- area_summary()

      if (nrow(summary) == 0) {
        return("Tidak ada data Kapanewon pada tahun terpilih.")
      }

      if (any(!summary$lengkap)) {
        return(
          paste0(
            "Grafik dan peringkat hanya menggunakan Kapanewon dengan data lengkap dan tanpa duplikasi. ",
            "Kontribusi persentase belum dihitung karena sebagian wilayah perlu diperiksa. ",
            "Analisis ini hanya mencakup baris yang memiliki informasi Kapanewon pada data hasil preprocessing."
          )
        )
      }

      paste0(
        "Kapanewon dengan kunjungan tertinggi: ",
        summary$kapanewon[1],
        ". Kontribusi dihitung terhadap total kunjungan seluruh Kapanewon yang tersedia. ",
        "Analisis hanya mencakup baris yang memiliki informasi Kapanewon pada data hasil preprocessing."
      )
    })

    output$quality_note <- shiny::renderText(
      quality_note(
        filter_year_data(data(), selected_year())
      )
    )
  })
}
