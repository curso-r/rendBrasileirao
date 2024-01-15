url <- "https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv"

dados <- readr::read_csv(url)

calcular_num_pontos <- function(placar, perspectiva = "mandante") {
  gols <- stringr::str_split(placar, pattern = "x")
  gols_mandante <- as.numeric(gols[[1]][1])
  gols_visitante <- as.numeric(gols[[1]][2])

  if (perspectiva == "mandante") {
    if (gols_mandante > gols_visitante) {
      return(3)
    } else if (gols_mandante == gols_visitante) {
      return(1)
    } else {
      return(0)
    }
  } else if (perspectiva == "visitante") {
    if (gols_mandante > gols_visitante) {
      return(0)
    } else if (gols_mandante == gols_visitante) {
      return(1)
    } else {
      return(3)
    }
  }
}

calcular_num_pontos(c("1x0", "1x1"), perspectiva = "visitante")

tab <- dados |>
  dplyr::mutate(
    num_pontos_mandante = purrr::map_dbl(
      score,
      ~ calcular_num_pontos(placar = .x, perspectiva = "mandante")
    ),
    num_pontos_visitante = purrr::map_dbl(
      score,
      ~ calcular_num_pontos(placar = .x, perspectiva = "visitante")
    )
  ) |>
  dplyr::select(
    season,
    date,
    home,
    num_pontos_mandante,
    away,
    num_pontos_visitante
  ) |>
  tidyr::pivot_longer(
    cols = c(home, away),
    names_to = "mando",
    values_to = "time"
  ) |>
  tidyr::pivot_longer(
    cols = c(num_pontos_mandante, num_pontos_visitante),
    names_to = "mando_pontuacao",
    values_to = "pontos"
  ) |>
  dplyr::mutate(
    mando_pontuacao = ifelse(
      mando_pontuacao == "num_pontos_mandante",
      "home",
      "away"
    )
  ) |> 
  dplyr::filter(mando == mando_pontuacao) |> 
  dplyr::arrange(season, time, date) |> 
  dplyr::group_by(season, time) |> 
  dplyr::mutate(
    rodada = dplyr::row_number()
  ) |> 
  dplyr::select(
    season,
    rodada,
    date,
    time,
    mando,
    pontos
  )
