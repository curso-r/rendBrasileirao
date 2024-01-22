#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  dados <- golem::get_golem_options("dados")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_fluid(
      titlePanel("Rendimentos dos times do Brasileirão"),
      hr(),
      bslib::layout_columns(
        col_widths = c(3, 3, 3),
        shinyWidgets::pickerInput(
          inputId = "temporada",
          label = "Temporada",
          choices = sort(unique(dados$season))
        ),
        shinyWidgets::pickerInput(
          inputId = "times",
          label = "Times",
          choices = sort(unique(dados$time)),
          multiple = TRUE,
          selected = "São Paulo"
        ),
        shinyWidgets::pickerInput(
          inputId = "metrica",
          label = "Métrica",
          choices = c(
            "Média móvel" = "media_movel",
            "Média de pontos por mês" = "media_mes"
          )
        )
      ),
      plotly::plotlyOutput("grafico")
    )
  )
}


#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rendBrasileirao"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
