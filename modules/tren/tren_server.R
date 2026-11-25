
tren_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {
    
    selected_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
    monthly_data <- shiny::reactive({
      monthly_totals(data(), selected_year())
    })
    
    output$monthly_statistics <- shiny::renderUI({
      monthly <- monthly_data()
      stats <- calc_descriptive(monthly$total)
      
      shiny::tagList(
        shiny::p(
          paste("Rata-rata:", fmt_number(stats$mean))
        ),
        shiny::p(
          paste("Median:", fmt_number(stats$median))
        ),
        shiny::p(
          paste("Standar deviasi:", fmt_number(stats$sd))
        ),
        shiny::p(
          paste("Kunjungan tertinggi:", fmt_number(stats$maximum))
        ),
        shiny::p(
          paste("Kunjungan terendah:", fmt_number(stats$minimum))
        ),
        shiny::p(
          paste("Rentang:", fmt_number(stats$rentang))
        ),
        shiny::p(
          paste(
            "Bulan dengan nilai tersedia:",
            sum(!is.na(monthly$total)),
            "dari 12 bulan."
          )
        )
      )
    })
    
    output$monthly_plot <- shiny::renderPlot({
      monthly <- monthly_data()
      
      if (all(is.na(monthly$total))) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Data bulanan belum tersedia"
            ) +
            base_plot_theme()
        )
      }
      
      plot_monthly_trend(monthly)
    })
    
    output$monthly_table <- DT::renderDT({
      monthly <- monthly_data()
      
      result <- data.frame(
        Bulan = as.character(monthly$bulan),
        `Jumlah tercatat` = monthly$total,
        `Nilai kosong` = monthly$nilai_kosong,
        Status = ifelse(
          is.na(monthly$total),
          "Tidak tersedia",
          ifelse(
            monthly$nilai_kosong > 0,
            "Parsial",
            "Lengkap"
          )
        ),
        check.names = FALSE
      )
      
      DT::datatable(
        result,
        rownames = FALSE,
        options = list(
          pageLength = 12,
          searching = FALSE,
          paging = FALSE,
          info = FALSE
        )
      )
    })
    
    output$annual_table <- DT::renderDT({
      annual <- annual_totals(data())
      
      if (nrow(annual) == 0) {
        return(
          DT::datatable(
            data.frame(Pesan = "Data tahunan belum tersedia."),
            rownames = FALSE
          )
        )
      }
      
      if (isTRUE(input$confirm_comparable)) {
        annual <- calc_growth_rates(annual)
        growth <- annual$growth_rate
      } else {
        growth <- rep(NA_real_, nrow(annual))
      }
      
      result <- data.frame(
        Tahun = annual$tahun,
        `Total tercatat` = annual$total,
        `Nilai kosong` = annual$nilai_kosong,
        `Destinasi unik` = annual$jumlah_destinasi,
        `Kelengkapan data` = ifelse(
          annual$lengkap,
          "Lengkap",
          "Perlu diperiksa"
        ),
        `Growth rate (%)` = growth,
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
    
    output$growth_note <- shiny::renderText({
      if (!isTRUE(input$confirm_comparable)) {
        return(
          "Growth rate belum dihitung. Periksa terlebih dahulu kesamaan definisi, cakupan, dan struktur data setiap tahun."
        )
      }
      
      annual <- calc_growth_rates(
        annual_totals(data())
      )
      
      if (
        nrow(annual) == 0 ||
        all(is.na(annual$growth_rate))
      ) {
        return(
          "Growth rate belum dapat dihitung. Periksa kelengkapan data, duplikasi, kategori, dan ketersediaan tahun berurutan."
        )
      }
      
      "Growth rate hanya ditampilkan untuk pasangan tahun berurutan yang memenuhi pemeriksaan kelengkapan pada fungsi statistik."
    })
    
    output$quality_note <- shiny::renderText(
      quality_note(selected_data())
    )
  })
}
