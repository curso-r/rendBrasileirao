#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  dados <- golem::get_golem_options("dados")

  output$grafico <- plotly::renderPlotly({
    tab <- dados  |> 
      dplyr::filter(
        season == input$temporada,
        time %in% input$times
      )

    if (input$metrica == "media_movel") {
      tab <- tab  |> 
        dplyr::arrange(time, rodada) |> 
        dplyr::group_by(time) |> 
        dplyr::mutate(
          y = zoo::rollmean(pontos, k = 5, fill = NA, align = "right")
        ) |> 
        dplyr::rename(x = rodada)
    } else if (input$metrica == "media_mes") {
      tab <- tab  |> 
        dplyr::mutate(
          x = lubridate::floor_date(date, unit = "months")
        ) |> 
        dplyr::group_by(x, time) |> 
        dplyr::summarise(
          y = mean(pontos)
        )
    }

    tab  |> 
      tidyr::drop_na(y) |> 
      plotly::plot_ly(
        x = ~ x,
        y = ~ y,
        color = ~ time,
        type = 'scatter',
        mode = 'lines+markers'
      )
  })

}
