
destinasi_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {
    
    selected_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
    shiny::observeEvent(data(), {
      d <- data()
      
      categories <- sort(unique(
        stats::na.omit(as.character(d$kategori))
      ))
      
      areas <- sort(unique(
        stats::na.omit(as.character(d$kapanewon))
      ))
      
      shiny::updateSelectInput(
        session,
        "category_filter",
        choices = c("Semua", categories),
        selected = "Semua"
      )
      
      shiny::updateSelectInput(
        session,
        "area_filter",
        choices = c("Semua", areas),
        selected = "Semua"
      )
    }, ignoreInit = FALSE)
    
    filtered_data <- shiny::reactive({
      d <- selected_data()
      
      if (
        !is.null(input$category_filter) &&
        input$category_filter != "Semua"
      ) {
        d <- dplyr::filter(
          d,
          .data$kategori == input$category_filter
        )
      }
      
      if (
        !is.null(input$area_filter) &&
        input$area_filter != "Semua"
      ) {
        d <- dplyr::filter(
          d,
          .data$kapanewon == input$area_filter
        )
      }
      
      d
    })
    
    destination_summary <- shiny::reactive({
      summary <- destination_totals(filtered_data())
      
      if (nrow(summary) > 0) {
        summary$id <- paste(
          summary$destinasi,
          summary$kategori,
          summary$kapanewon,
          sep = "|||"
        )
        
        summary$label <- paste(
          summary$destinasi,
          "—",
          summary$kategori,
          "—",
          summary$kapanewon
        )
      }
      
      summary
    })
    
    shiny::observeEvent(destination_summary(), {
      summary <- destination_summary()
      
      choices <- if (nrow(summary) == 0) {
        character(0)
      } else {
        stats::setNames(
          summary$id,
          summary$label
        )
      }
      
      shiny::updateSelectInput(
        session,
        "destination_detail",
        choices = choices,
        selected = if (length(choices)) {
          unname(choices[1])
        } else {
          character(0)
        }
      )
    }, ignoreInit = FALSE)
    
    complete_destinations <- shiny::reactive({
      destination_summary() |>
        dplyr::filter(
          .data$lengkap,
          !is.na(.data$total)
        ) |>
        dplyr::arrange(dplyr::desc(.data$total))
    })
    
    output$top_plot <- shiny::renderPlot({
      summary <- complete_destinations()
      
      if (nrow(summary) == 0) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Belum ada destinasi dengan data lengkap"
            ) +
            base_plot_theme()
        )
      }
      
      plot_top_destinations(summary)
    })
    
    output$lowest_destination <- shiny::renderUI({
      summary <- complete_destinations()
      
      if (nrow(summary) == 0) {
        return(
          shiny::p(
            "Belum dapat ditentukan karena tidak ada destinasi dengan data lengkap."
          )
        )
      }
      
      lowest <- summary |>
        dplyr::arrange(.data$total) |>
        dplyr::slice(1)
      
      shiny::tagList(
        shiny::h4(lowest$destinasi[1]),
        shiny::p(
          paste("Kategori:", lowest$kategori[1])
        ),
        shiny::p(
          paste("Kapanewon:", lowest$kapanewon[1])
        ),
        shiny::h3(fmt_number(lowest$total[1]))
      )
    })
    
    output$destination_table <- DT::renderDT({
      summary <- destination_summary()
      
      if (nrow(summary) == 0) {
        return(
          DT::datatable(
            data.frame(
              Pesan = "Tidak ada destinasi untuk filter yang dipilih."
            ),
            rownames = FALSE
          )
        )
      }
      
      summary <- summary |>
        dplyr::mutate(
          Status = ifelse(
            .data$lengkap,
            "Lengkap",
            "Perlu diperiksa"
          )
        ) |>
        dplyr::select(
          destinasi, kategori, kapanewon,
          total, nilai_kosong, Status
        ) |>
        dplyr::rename(
          Destinasi = destinasi,
          Kategori = kategori,
          Kapanewon = kapanewon,
          `Jumlah tercatat` = total,
          `Nilai kosong` = nilai_kosong
        )
      
      DT::datatable(
        summary,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
    })
    
    selected_destination <- shiny::reactive({
      summary <- destination_summary()
      
      if (
        nrow(summary) == 0 ||
        is.null(input$destination_detail) ||
        input$destination_detail == ""
      ) {
        return(NULL)
      }
      
      summary[
        summary$id == input$destination_detail,
        ,
        drop = FALSE
      ]
    })
    
    detail_data <- shiny::reactive({
      selected <- selected_destination()
      
      if (is.null(selected) || nrow(selected) == 0) {
        return(filtered_data()[0, , drop = FALSE])
      }
      
      d <- filtered_data()
      
      dplyr::filter(
        d,
        .data$destinasi == selected$destinasi[1],
        .data$kategori == selected$kategori[1],
        .data$kapanewon == selected$kapanewon[1]
      )
    })
    
    detail_monthly <- shiny::reactive({
      d <- detail_data()
      
      if (nrow(d) == 0) {
        return(tibble::tibble(
          bulan = factor(MONTHS_ID, levels = MONTHS_ID),
          bulan_num = 1:12,
          total = NA_real_,
          nilai_kosong = NA_integer_
        ))
      }
      
      d |>
        dplyr::group_by(
          .data$bulan,
          .data$bulan_num
        ) |>
        dplyr::summarise(
          total = safe_sum(.data$jumlah),
          nilai_kosong = sum(is.na(.data$jumlah)),
          .groups = "drop"
        ) |>
        dplyr::right_join(
          tibble::tibble(
            bulan = factor(MONTHS_ID, levels = MONTHS_ID),
            bulan_num = 1:12
          ),
          by = c("bulan", "bulan_num")
        ) |>
        dplyr::arrange(.data$bulan_num)
    })
    
    output$detail_plot <- shiny::renderPlot({
      monthly <- detail_monthly()
      
      if (all(is.na(monthly$total))) {
        return(
          ggplot2::ggplot() +
            ggplot2::labs(
              title = "Pilih destinasi untuk melihat detail bulanan"
            ) +
            base_plot_theme()
        )
      }
      
      plot_monthly_trend(
        monthly,
        "Kunjungan bulanan destinasi terpilih"
      )
    })
    
    output$detail_table <- DT::renderDT({
      monthly <- detail_monthly()
      
      result <- data.frame(
        Bulan = as.character(monthly$bulan),
        `Jumlah tercatat` = monthly$total,
        `Nilai kosong` = monthly$nilai_kosong,
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
    
    output$detail_note <- shiny::renderText({
      d <- detail_data()
      
      if (nrow(d) == 0) {
        return("Belum ada detail destinasi untuk ditampilkan.")
      }
      
      missing <- sum(is.na(d$jumlah))
      
      if (missing > 0) {
        return(
          paste0(
            "Grafik menggunakan nilai yang tersedia; terdapat ",
            fmt_number(missing),
            " nilai kosong."
          )
        )
      }
      
      "Seluruh nilai bulanan untuk destinasi terpilih tersedia."
    })
    
    output$quality_note <- shiny::renderText(
      quality_note(selected_data())
    )
  })
}
