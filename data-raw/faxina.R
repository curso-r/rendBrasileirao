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


################

# pontuacao_ponderada = pontos * alfa * beta

# alfa = f(colocacao_adv)
# 1º: alfa = 2
# 2º: alfa = 1.9
# ...
# 20º: alfa = 0.1


# beta = f(aprov_adv)
# beta = 1 + aprov

url <- "https://raw.githubusercontent.com/williamorim/brasileirao/master/data-raw/csv/matches.csv"

readr::read_csv(url) |>
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
  tidyr::separate_wider_delim(
    cols = score,
    delim = "x",
    names = c("gols_mandante", "gols_visitante")
  ) |>
  dplyr::select(
    season,
    date,
    home,
    num_pontos_mandante,
    gols_mandante,
    away,
    num_pontos_visitante,
    gols_visitante
  ) |>
  tidyr::pivot_longer(
    cols = c(num_pontos_mandante, num_pontos_visitante),
    names_to = "mando",
    values_to = "pontos"
  ) |>
  dplyr::mutate(
    mando = stringr::str_remove(mando, "num_pontos_"),
    time = ifelse(
      mando == "mandante",
      home,
      away
    ),
    adversario = ifelse(
      mando == "mandante",
      away,
      home
    ),
    gols_marcados = ifelse(
      mando == "mandante",
      gols_mandante,
      gols_visitante
    ),
    gols_sofridos = ifelse(
      mando == "mandante",
      gols_visitante,
      gols_mandante
    )
  ) |>
  dplyr::select(
    season,
    date,
    time,
    adversario,
    mando,
    pontos,
    gols_marcados,
    gols_sofridos
  ) |>
  dplyr::arrange(season, time, date) |>
  dplyr::group_by(season, time) |>
  dplyr::mutate(
    rodada = dplyr::row_number(),
    .after = season
  )




devtools::load_all()

tab <- pegar_dados()

tab_2023 <- tab |>
  dplyr::filter(season == 2023)

.rodada <- 38
.time <- "São Paulo"
.mando <- "mandante"

# alfa(1): 2
# alfa(2): 1.9
# alfa(20): 0.1

calcular_alfa(tab_2023, "São Paulo", 30)
calcular_beta(tab_2023, "São Paulo", 30, "mandante")

dados <- pegar_dados()

tab_temporada <- dados |>
  dplyr::filter(
    season == 2023
  )

tab_temporada |> 
  dplyr::filter(
    time == "São Paulo"
  ) |> 
  dplyr::mutate(
    pontos_ponderados = purrr::pmap_dbl(
      list(
        pontos = pontos,
        rodada = rodada,
        time = adversario,
        mando = mando
      ),
      ~ ..1 * calcular_alfa(tab_temporada, ..3, ..2) * 
      calcular_beta(tab_temporada, ..3, ..2, ..4)
      )
    ) |> 
    dplyr::select(
      rodada,
      time,
      adversario,
      mando,
      pontos,
      pontos_ponderados
    ) |> 
    View()
