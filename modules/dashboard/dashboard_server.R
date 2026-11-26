
dashboard_server <- function(id, data, selected_year) {
  
  shiny::moduleServer(id, function(input, output, session) {
    
    # Mengambil data berdasarkan tahun yang dipilih
    year_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
    # Ringkasan bulanan
    monthly_data <- shiny::reactive({
      monthly_totals(year_data(), selected_year())
    })
    
    # Ringkasan kategori
    category_data <- shiny::reactive({
      
      d <- year_data()
      
      if (nrow(d) == 0) {
        return(tibble::tibble(
          kategori = character(),
          total = double()
        ))
      }
      
      d |>
        dplyr::group_by(.data$kategori) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          .groups = "drop"
        ) |>
        dplyr::filter(!is.na(.data$total)) |>
        dplyr::arrange(dplyr::desc(.data$total))
    })
    
    # Ringkasan destinasi lengkap
    destination_data <- shiny::reactive({
      
      destination_totals(year_data()) |>
        dplyr::filter(
          .data$lengkap,
          !is.na(.data$total)
        ) |>
        dplyr::arrange(dplyr::desc(.data$total))
    })
    
    # 1. Total kunjungan
    output$total_visits <- shiny::renderUI({
      
      d <- year_data()
      
      duplicate_keys <- if (nrow(d) > 0) {
        any(duplicated(d[
          c("tahun", "kategori", "destinasi",
            "kapanewon", "asal", "bulan")
        ]))
      } else {
        FALSE
      }
      
      total <- if (duplicate_keys) {
        NA_real_
      } else {
        safe_sum(d$jumlah)
      }
      
      shiny::tagList(
        shiny::div(
          class = "kpi-value",
          fmt_number(total)
        ),
        shiny::div(
          class = "kpi-footnote",
          "Total kunjungan tercatat"
        )
      )
    })
    
    # 2. Jumlah destinasi
    output$destination_count <- shiny::renderUI({
      
      n <- nrow(destination_data())
      
      shiny::tagList(
        shiny::div(
          class = "kpi-value",
          fmt_number(n)
        ),
        shiny::div(
          class = "kpi-footnote",
          "Destinasi dengan data lengkap"
        )
      )
    })
    
    # 3. Rata-rata kunjungan bulanan
    output$monthly_average <- shiny::renderUI({
      
      m <- monthly_data()
      valid <- m$total[!is.na(m$total)]
      
      avg <- if (length(valid) > 0) {
        mean(valid)
      } else {
        NA_real_
      }
      
      shiny::tagList(
        shiny::div(
          class = "kpi-value",
          fmt_number(avg)
        ),
        shiny::div(
          class = "kpi-footnote",
          "Rata-rata dari bulan tersedia"
        )
      )
    })
    
    # 4. Bulan dengan kunjungan tertinggi
    output$highest_month <- shiny::renderUI({
      
      m <- monthly_data() |>
        dplyr::filter(!is.na(.data$total))
      
      if (nrow(m) == 0) {
        return(shiny::div(
          class = "kpi-value",
          "Tidak tersedia"
        ))
      }
      
      peak <- m |>
        dplyr::slice_max(
          order_by = .data$total,
          n = 1,
          with_ties = FALSE
        )
      
      month_name <- unname(
        MONTH_LABELS[as.character(peak$bulan[1])]
      )
      
      shiny::tagList(
        shiny::div(
          class = "kpi-value kpi-month",
          month_name
        ),
        shiny::div(
          class = "kpi-footnote",
          paste(
            fmt_number(peak$total[1]),
            "kunjungan"
          )
        )
      )
    })
    
    # Insight otomatis
    output$insight <- shiny::renderUI({
      
      m <- monthly_data() |>
        dplyr::filter(!is.na(.data$total))
      
      c <- category_data()
      
      if (nrow(m) == 0) {
        return(shiny::div(
          class = "dashboard-insight",
          "Data belum tersedia untuk tahun yang dipilih."
        ))
      }
      
      peak <- m |>
        dplyr::slice_max(
          order_by = .data$total,
          n = 1,
          with_ties = FALSE
        )
      
      month_name <- unname(
        MONTH_LABELS[as.character(peak$bulan[1])]
      )
      
      category_text <- if (nrow(c) > 0) {
        paste0(
          "Kategori dengan kunjungan tertinggi adalah ",
          c$kategori[1],
          "."
        )
      } else {
        "Data kategori belum tersedia."
      }
      
      shiny::div(
        class = "dashboard-insight",
        shiny::strong("Insight tahun analisis: "),
        paste0(
          "Kunjungan tertinggi terjadi pada ",
          month_name,
          " dengan ",
          fmt_number(peak$total[1]),
          " kunjungan. ",
          category_text
        )
      )
    })
    
    # Grafik tren bulanan
    output$monthly_plot <- shiny::renderPlot({
      
      d <- year_data()
      shiny::req(nrow(d) > 0)
      
      plot_monthly_trend(
        monthly_data(),
        "Tren kunjungan wisatawan"
      )
      
    }, res = 96)
    
    # Grafik kategori
    output$category_plot <- shiny::renderPlot({
      
      c <- category_data()
      
      if (nrow(c) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Data kategori belum tersedia"
            ) +
            ggplot2::theme_void()
        )
      }
      
      plot_donut(
        c,
        label_col = "kategori",
        title = "Komposisi kategori wisata"
      )
      
    }, res = 96)
    
    # Tabel destinasi unggulan
    output$top_destinations <- shiny::renderTable({
      
      d <- destination_data()
      
      if (nrow(d) == 0) {
        return(data.frame(
          Informasi = "Belum tersedia data destinasi lengkap."
        ))
      }
      
      d |>
        dplyr::slice_head(n = 5) |>
        dplyr::transmute(
          Destinasi = .data$destinasi,
          Kategori = .data$kategori,
          Kapanewon = .data$kapanewon,
          `Jumlah kunjungan` = vapply(
            .data$total,
            fmt_number,
            character(1)
          )
        )
      
    },
    striped = TRUE,
    bordered = FALSE,
    hover = TRUE,
    spacing = "s",
    width = "100%"
    )
    
    # Informasi periode
    output$period <- shiny::renderText({
      
      year <- selected_year()
      
      if (length(year) == 0 || is.na(year[1])) {
        return("Periode data belum tersedia.")
      }
      
      paste("Tahun analisis:", year[1])
    })
    
    # Catatan kualitas data
    output$quality_note <- shiny::renderText({
      quality_note(year_data())
    })
    
  })
}
