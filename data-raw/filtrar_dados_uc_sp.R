gerar_dados_filtrados <- function(
    dados = monitora_aves_masto_florestal,
    nome_ucs = NULL,
    nome_sps = NULL,
    validacao_obs = c("ordem", "familia", "genero", "especie", "na")
) {

  # controlar as possibilidades de filtragem
  if (is.null(nome_ucs) & is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e especie e nivel taxonomico de validacao
    dados_filtrados <- monitora_aves_masto_florestal |>
      dplyr::filter(
        validacao %in% validacao_obs
      )

  } else if (!is.null(nome_ucs) & !is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e especie e nivel taxonomico de validacao
    dados_filtrados <- monitora_aves_masto_florestal |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        nome_sp %in% nome_sps,
        validacao %in% validacao_obs
      )

  } else  if (is.null(nome_ucs)) {

    # gerar o tibble filtrado por especie e nivel taxonomico de validacao
    dados_filtrados <- monitora_aves_masto_florestal |>
      dplyr::filter(
        nome_sp %in% nome_sps,
        validacao %in% validacao_obs
      )

  } else if (is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e nivel taxonomico de validacao
    dados_filtrados <- monitora_aves_masto_florestal |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        validacao %in% validacao_obs
      )

  }

  # retornar o tibble com os dados filtrados e nivel taxonomico de validacao
  return(dados_filtrados)
}

# exemplos
# carregar dados filtrados por uma unidade de conservacao
dados_filtrados_uc1 <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

dplyr::glimpse(dados_filtrados_uc1)

monitora_aves_masto_florestal |>
  dplyr::filter(nome_uc == "resex_tapajos_arapiuns")

# carregar dados filtrados por mais de uma unidade de conservacao
dados_filtrados_uc2 <- gerar_dados_filtrados(
  nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
)

dplyr::glimpse(dados_filtrados_uc2)

monitora_aves_masto_florestal |>
  dplyr::filter(nome_uc %in% c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"))

# carregar dados filtrados por uma especie
dados_filtrados_sp1 <- gerar_dados_filtrados(
  nome_sps = "dasyprocta_croconota"
)

dplyr::glimpse(dados_filtrados_sp1)

monitora_aves_masto_florestal |>
  dplyr::filter(nome_sp == "dasyprocta_croconota")

# carregar dados filtrados por mais de uma especie
dados_filtrados_sp2 <- gerar_dados_filtrados(
  nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
)

dplyr::glimpse(dados_filtrados_sp2)

monitora_aves_masto_florestal |>
  dplyr::filter(nome_sp %in% c("dasyprocta_croconota", "dasyprocta_iacki"))

# carregar dados filtrados por unidade de conservacao e especie
dados_filtrados_uc_sp <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota"
)

dplyr::glimpse(dados_filtrados_uc_sp)

monitora_aves_masto_florestal |>
  dplyr::filter(
    nome_uc == "resex_tapajos_arapiuns",
    nome_sp == "dasyprocta_croconota"
  )

# carregar dados filtrados por unidade de conservacao, especie e validacao ao nivel de especie
dados_filtrados_uc_sp_validacao <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota",
  validacao_obs = "especie"
)

dplyr::glimpse(dados_filtrados_uc_sp_validacao)

monitora_aves_masto_florestal |>
  dplyr::filter(
    nome_uc == "resex_tapajos_arapiuns",
    nome_sp == "dasyprocta_croconota",
    validacao == "especie"
  )

dados_filtrados_uc2 <- gerar_dados_filtrados(
  nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
  validacao_obs = "genero"
)

dplyr::glimpse(dados_filtrados_uc2)

monitora_aves_masto_florestal |>
  dplyr::filter(
    nome_uc %in% c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
    validacao %in% c("genero")
  )

dados_filtrados_uc1 <- gerar_dados_filtrados(
  nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
  validacao_obs = c("genero", "especie")
)

dplyr::glimpse(dados_filtrados_uc1)

monitora_aves_masto_florestal |>
  dplyr::filter(
    nome_uc %in% c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
    validacao %in% c("genero", "especie")
  )

# .keep_all = TRUE

gerar_dados_filtrados(validacao_obs = "especie")
