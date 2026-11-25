
dashboard_server <- function(id, data, selected_year) {
  shiny::moduleServer(id, function(input, output, session) {
    
    year_data <- shiny::reactive({
      filter_year_data(data(), selected_year())
    })
    
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
      
      incomplete <- sum(is.na(d$jumlah))
      
      shiny::tagList(
        shiny::h2(fmt_number(total)),
        shiny::p(
          if (duplicate_keys) {
            "Total belum ditampilkan karena terdapat duplikasi pada kunci pengamatan."
          } else if (incomplete > 0) {
            paste0(
              "Jumlah dari nilai yang tersedia; ",
              fmt_number(incomplete),
              " nilai belum tersedia."
            )
          } else {
            "Jumlah berdasarkan seluruh nilai bulanan yang tersedia."
          }
        )
      )
    })
    
    output$destination_count <- shiny::renderUI({
      d <- year_data()
      summary <- destination_totals(d)
      
      valid <- summary |>
        dplyr::filter(
          .data$lengkap,
          !is.na(.data$total)
        ) |>
        dplyr::distinct(.data$destinasi)
      
      shiny::tagList(
        shiny::h2(fmt_number(nrow(valid))),
        shiny::p(
          "Destinasi unik dengan 24 kombinasi data bulanan WNI/WNA yang lengkap dan tanpa duplikasi."
        )
      )
    })
    
    output$period <- shiny::renderText({
      year <- selected_year()
      
      if (length(year) == 0 || is.na(year[1])) {
        return("Periode data belum tersedia.")
      }
      
      paste("Tahun analisis:", year[1])
    })
    
    output$source_note <- shiny::renderText(
      SOURCE_DESCRIPTION
    )
    
    output$quality_note <- shiny::renderText(
      quality_note(year_data())
    )
  })
}
