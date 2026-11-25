
kategori_wisata_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {
    
    selected_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
    category_summary <- shiny::reactive({
      d <- selected_data()
      
      if (nrow(d) == 0) {
        return(tibble::tibble())
      }
      
      duplicate_keys <- d |>
        dplyr::count(
          .data$tahun,
          .data$kategori,
          .data$destinasi,
          .data$kapanewon,
          .data$asal,
          .data$bulan,
          name = "n"
        ) |>
        dplyr::filter(.data$n > 1)
      
      duplicate_categories <- unique(
        duplicate_keys$kategori
      )
      
      result <- d |>
        dplyr::group_by(.data$kategori) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          nilai_kosong = sum(is.na(.data$jumlah)),
          jumlah_destinasi = dplyr::n_distinct(.data$destinasi),
          .groups = "drop"
        ) |>
        dplyr::mutate(
          duplikasi = .data$kategori %in% duplicate_categories,
          lengkap = .data$nilai_kosong == 0 &
            !.data$duplikasi
        ) |>
        dplyr::arrange(dplyr::desc(.data$total))
      
      all_complete <- nrow(result) == length(CATEGORY_DEFAULTS) &&
        all(result$lengkap) &&
        !is.na(safe_sum(result$total)) &&
        safe_sum(result$total) > 0
      
      result$persentase <- if (all_complete) {
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
    
    output$category_plot <- shiny::renderPlot({
      summary <- category_summary()
      
      if (
        nrow(summary) == 0 ||
        all(is.na(summary$total))
      ) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Data kategori belum tersedia"
            ) +
            base_plot_theme()
        )
      }
      
      plot_category_visits(summary)
    })
    
    output$category_donut <- shiny::renderPlot({
      summary <- category_summary()
      
      if (
        nrow(summary) == 0 ||
        any(is.na(summary$persentase))
      ) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Komposisi belum dapat dihitung karena data belum lengkap"
            ) +
            base_plot_theme()
        )
      }
      
      plot_donut(
        summary,
        label_col = "kategori",
        title = "Kontribusi kategori wisata"
      )
    })
    
    output$category_table <- DT::renderDT({
      summary <- category_summary()
      
      if (nrow(summary) == 0) {
        return(
          DT::datatable(
            data.frame(
              Pesan = "Data kategori belum tersedia."
            ),
            rownames = FALSE
          )
        )
      }
      
      result <- data.frame(
        Peringkat = summary$peringkat,
        Kategori = summary$kategori,
        `Jumlah tercatat` = summary$total,
        `Destinasi unik` = summary$jumlah_destinasi,
        `Nilai kosong` = summary$nilai_kosong,
        `Kontribusi (%)` = summary$persentase,
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
          searching = FALSE
        )
      )
    })
    
    output$category_note <- shiny::renderText({
      summary <- category_summary()
      
      if (nrow(summary) == 0) {
        return("Tidak ada kategori pada tahun terpilih.")
      }
      
      if (any(is.na(summary$persentase))) {
        return(
          "Jumlah yang ditampilkan merupakan penjumlahan nilai yang tersedia. Kontribusi persentase belum ditampilkan karena terdapat nilai kosong atau duplikasi."
        )
      }
      
      paste0(
        "Kategori dengan jumlah kunjungan tertinggi: ",
        summary$kategori[1],
        ". Persentase dihitung terhadap total seluruh kategori yang dibandingkan."
      )
    })
    
    output$subcategory_note <- shiny::renderText({
      "Rincian subkategori belum dapat ditampilkan karena data hasil data_loader.R saat ini hanya menyediakan kategori utama, tanpa kolom subkategori."
    })
    
    output$quality_note <- shiny::renderText(
      quality_note(selected_data())
    )
  })
}
