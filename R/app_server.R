#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dados <- golem::get_golem_options("dados")

  observe({
    opcoes <- dados  |> 
      dplyr::filter(season == input$temporada) |> 
      dplyr::distinct(time) |>
      dplyr::pull(time)

    shinyWidgets::updatePickerInput(
      session = session,
      inputId = "times",
      choices = opcoes,
      selected = opcoes[1]
    )
  })

  output$grafico <- plotly::renderPlotly({
    req(input$times)
    tab_temporada <- dados |>
      dplyr::filter(
        season == input$temporada
      )

    tab <- tab_temporada |>
      dplyr::filter(
        time %in% input$times
      )

    if (input$pontuacao_ponderada) {
      tab <- tab |>
        dplyr::mutate(
          pontos = purrr::pmap_dbl(
            list(
              pontos = pontos,
              rodada = rodada,
              time = adversario,
              mando = mando
            ),
            ~ ..1 * calcular_alfa(tab_temporada, ..3, ..2) *
              calcular_beta(tab_temporada, ..3, ..2, ..4)
          )
        )
    }

    if (input$metrica == "media_movel") {
      xlab <- "Rodada"
      tab <- tab |>
        dplyr::arrange(time, rodada) |>
        dplyr::group_by(time) |>
        dplyr::mutate(
          y = zoo::rollmean(pontos, k = 5, fill = NA, align = "right")
        ) |>
        dplyr::rename(x = rodada)
    } else if (input$metrica == "media_mes") {
      xlab <- "Mês"
      tab <- tab |>
        dplyr::mutate(
          x = lubridate::floor_date(date, unit = "months")
        ) |>
        dplyr::group_by(x, time) |>
        dplyr::summarise(
          y = mean(pontos),
          .groups = "drop"
        )
    }

    tab |>
      tidyr::drop_na(y) |>
      plotly::plot_ly(
        x = ~x,
        y = ~y,
        color = ~time,
        type = "scatter",
        mode = "lines+markers"
      ) |> 
      plotly::layout(
        xaxis = list(
          title = xlab
        ),
        yaxis = list(
          title = "Média pontos"
        )
      ) |> 
      plotly::config(displayModeBar = FALSE)
  })
}
