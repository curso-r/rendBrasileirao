pegar_dados <- function() {
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
      ),
      gols_marcados = as.numeric(gols_marcados),
      gols_sofridos = as.numeric(gols_sofridos)
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
    ) |>
    dplyr::ungroup()
}

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

pegar_colocacao <- function(tab, .time, .rodada) {
  tab |>
    dplyr::filter(rodada <= .rodada) |>
    dplyr::group_by(time) |>
    dplyr::summarise(
      total_pontos = sum(pontos),
      num_vitorias = sum(pontos == 3),
      saldo_gols = sum(gols_marcados) - sum(gols_sofridos),
      total_gols_marcados = sum(gols_marcados)
    ) |>
    dplyr::arrange(
      desc(total_pontos),
      desc(num_vitorias),
      desc(saldo_gols),
      desc(total_gols_marcados)
    ) |>
    dplyr::mutate(
      colocacao = dplyr::row_number()
    ) |>
    dplyr::filter(time == .time) |>
    dplyr::pull(colocacao)
}

pegar_aproveitamento <- function(tab, .time, .rodada, .mando) {
  tab_time <- tab |>
    dplyr::filter(
      rodada <= .rodada,
      time == .time,
      mando == .mando
    )

  if (nrow(tab_time) == 0) {
    return(0)
  }

  tab_time |>
    dplyr::summarise(
      aprov = sum(pontos) / (dplyr::n() * 3)
    ) |>
    dplyr::pull(aprov)
}

calcular_alfa <- function(tab, .adversario, .rodada) {
  if (.rodada == 1) {
    return(1)
  }

  colocacao <- pegar_colocacao(
    tab,
    .time = .adversario,
    .rodada = .rodada - 1
  )

  2 - (colocacao - 1) / 10
}

calcular_beta <- function(tab, .adversario, .rodada, .mando) {
  if (.rodada == 1) {
    return(1)
  }

  aprov_adv <- pegar_aproveitamento(
    tab,
    .time = .adversario,
    .rodada = .rodada - 1,
    .mando = .mando
  )

  1 + aprov_adv
}