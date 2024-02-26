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
      div(
        class = "app-title",
        img(src = "www/logo_transparente.png"),
        h2("Rendimentos dos times do Brasileirão")
      ),
      hr(),
      bslib::layout_columns(
        class = "align-items-end",
        col_widths = c(3, 3, 3, 3),
        shinyWidgets::pickerInput(
          inputId = "temporada",
          label = "Temporada",
          choices = sort(unique(dados$season))
        ),
        shinyWidgets::pickerInput(
          inputId = "times",
          label = "Times",
          choices = c("Carregando..." = ""),
          multiple = TRUE
        ),
        shinyWidgets::pickerInput(
          inputId = "metrica",
          label = "Métrica",
          choices = c(
            "Média móvel" = "media_movel",
            "Média de pontos por mês" = "media_mes"
          )
        ),
        div(
          class = "text-center",
          bslib::tooltip(
            shinyWidgets::prettyCheckbox(
              inputId = "pontuacao_ponderada",
              label = "Ponderar pontuação",
              value = FALSE,
              status = "primary"
            ),
            "Ponderar a pontuação dos times de acordo com a força do adversário"
          )
        )
      ),
      conditionalPanel(
        condition = "input.pontuacao_ponderada",
        div(
          class = "caixinha-informacao",
          bslib::value_box(
            title = "Sobre a ponderação",
            value = includeMarkdown(
              app_sys("app/md/explicacao_ponderacao.md")
            ),
            showcase = bsicons::bs_icon("info-circle"),
            showcase_layout = bslib::showcase_left_center(
              width = 0.1
            ),
            theme = "bg-info",
            class = "smaller"
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
    favicon(
      ico = "logo_transparente",
      ext = "png"
    ),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "rendBrasileirao"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
