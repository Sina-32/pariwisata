
profil_wisatawan_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {
    
    selected_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
    profile_summary <- shiny::reactive({
      d <- selected_data()
      
      if (nrow(d) == 0) {
        return(tibble::tibble())
      }
      
      result <- d |>
        dplyr::group_by(.data$asal) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          nilai_kosong = sum(is.na(.data$jumlah)),
          .groups = "drop"
        )
      
      all_complete <- all(!is.na(d$jumlah)) &&
        setequal(
          unique(as.character(d$asal)),
          c("WNI", "WNA")
        ) &&
        !any(duplicated(d[
          c("tahun", "kategori", "destinasi",
            "kapanewon", "asal", "bulan")
        ]))
      
      overall <- safe_sum(result$total)
      
      result$persentase <- if (
        all_complete &&
        !is.na(overall) &&
        overall > 0
      ) {
        100 * result$total / overall
      } else {
        NA_real_
      }
      
      result$lengkap <- all_complete
      
      result
    })
    
    monthly_profile <- shiny::reactive({
      d <- selected_data()
      
      if (nrow(d) == 0) {
        return(tibble::tibble())
      }
      
      d |>
        dplyr::group_by(
          .data$asal,
          .data$bulan,
          .data$bulan_num
        ) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          nilai_kosong = sum(is.na(.data$jumlah)),
          .groups = "drop"
        ) |>
        dplyr::arrange(
          .data$bulan_num,
          .data$asal
        )
    })
    
    output$composition_plot <- shiny::renderPlot({
      summary <- profile_summary()
      d <- selected_data()
      
      complete <- nrow(d) > 0 &&
        all(!is.na(d$jumlah)) &&
        setequal(
          unique(as.character(d$asal)),
          c("WNI", "WNA")
        ) &&
        !any(duplicated(d[
          c("tahun", "kategori", "destinasi",
            "kapanewon", "asal", "bulan")
        ]))
      
      if (
        !complete ||
        nrow(summary) == 0 ||
        any(is.na(summary$total))
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
        label_col = "asal",
        title = "Komposisi WNI dan WNA"
      )
    })
    
    output$profile_table <- DT::renderDT({
      summary <- profile_summary()
      
      if (nrow(summary) == 0) {
        return(
          DT::datatable(
            data.frame(
              Pesan = "Data profil wisatawan belum tersedia."
            ),
            rownames = FALSE
          )
        )
      }
      
      result <- data.frame(
        Kelompok = summary$asal,
        `Jumlah tercatat` = summary$total,
        `Nilai kosong` = summary$nilai_kosong,
        `Persentase (%)` = summary$persentase,
        check.names = FALSE
      )
      
      DT::datatable(
        result,
        rownames = FALSE,
        options = list(
          dom = "t",
          ordering = FALSE
        )
      )
    })
    
    output$wna_probability <- shiny::renderText({
      summary <- profile_summary()
      
      if (
        nrow(summary) == 0 ||
        any(is.na(summary$persentase))
      ) {
        return(
          "Belum dapat dihitung karena data WNI/WNA belum lengkap atau terdapat duplikasi."
        )
      }
      
      value <- summary$persentase[
        summary$asal == "WNA"
      ]
      
      if (length(value) == 0) {
        return("Data WNA tidak tersedia.")
      }
      
      paste0(
        "P(WNA) = ",
        fmt_percent(value),
        " berdasarkan seluruh kunjungan tercatat."
      )
    })
    
    output$monthly_plot <- shiny::renderPlot({
      monthly <- monthly_profile()
      
      if (
        nrow(monthly) == 0 ||
        all(is.na(monthly$total))
      ) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Data bulanan belum tersedia"
            ) +
            base_plot_theme()
        )
      }
      
      plot_profile_monthly(monthly)
    })
    
    output$monthly_note <- shiny::renderText({
      d <- selected_data()
      
      missing <- sum(is.na(d$jumlah))
      
      duplicate_count <- sum(duplicated(d[
        c("tahun", "kategori", "destinasi",
          "kapanewon", "asal", "bulan")
      ]))
      
      if (missing > 0 || duplicate_count > 0) {
        return(paste0(
          "Grafik menampilkan nilai yang tersedia. Nilai kosong: ",
          fmt_number(missing),
          "; baris duplikat pada kunci analisis: ",
          fmt_number(duplicate_count),
          ". Periksa kualitas data sebelum menafsirkan grafik."
        ))
      }
      
      "Seluruh nilai pada periode ini tersedia dan tidak ditemukan duplikasi pada kunci analisis."
    })
    
    output$quality_note <- shiny::renderText(
      quality_note(selected_data())
    )
  })
}
