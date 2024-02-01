## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distanceMonitoraflorestal)

## ----obter dados para especie, eval=FALSE-------------------------------------
#  cutias_year <- filtrar_dados(
#    nome_ucs == "resex_tapajos_arapiuns",
#    nome_sps == "dasyprocta_croconota",
#    validacao_obs = "especie"
#  )
#  
#  cutias_year_distance <- transformar_dados_formato_Distance(
#    cutias_year,
#    amostras_repetidas = FALSE)
#  
#  cutias_year_distance

## ----plotar as distancias, eval=FALSE-----------------------------------------
#  cutias_year_distance |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2014, eval=FALSE------------------------------------
#  cutia_2014 <- cutias_year_distance |>
#    dplyr::filter(year == 2014)
#  
#  cutia_2014 |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2015, eval=FALSE------------------------------------
#  cutia_2015 <- cutias_year_distance |>
#    dplyr::filter(year == 2015)
#  
#  cutia_2015 |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2016, eval=FALSE------------------------------------
#  cutia_2016 <- cutias_year_distance |>
#    dplyr::filter(year == 2016)
#  
#  cutia_2016 |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2017, eval=FALSE------------------------------------
#  cutia_2017 <- cutias_year_distance |>
#    dplyr::filter(year == 2017)
#  
#  cutia_2017 |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2018, eval=FALSE------------------------------------
#  cutia_2018 <- cutias_year_distance |>
#    dplyr::filter(year == 2018)
#  
#  cutia_2018 |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2019, eval=FALSE------------------------------------
#  cutia_2019 <- cutias_year_distance |>
#    dplyr::filter(year == 2019)
#  
#  cutia_2019 |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2020, eval=FALSE------------------------------------
#  cutia_2020 <- cutias_year_distance |>
#    dplyr::filter(year == 2020)
#  
#  cutia_2020|>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----plotar as distancias 2021, eval=FALSE------------------------------------
#  cutia_2021 <- cutias_year_distance |>
#    dplyr::filter(year == 2021)
#  
#  cutia_2021 |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção uniforme para um truncamento de 10% dos dados
#  cutias_distance_unif_year <- cutias_year_distance |>
#    dplyr::filter(year != 2014) |>
#    dplyr::mutate(Region.Label = year) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_distance_hn_year <- cutias_year_distance |>
#    dplyr::filter(year != 2014) |>
#    dplyr::mutate(Region.Label = year) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate para um truncamento de 10% dos dados
#  cutias_distance_hr_year <- cutias_year_distance |>
#    dplyr::filter(year != 2014) |>
#    dplyr::mutate(Region.Label = year) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  melhor_modelo_cutias_year <- selecionar_funcao_deteccao_termo_ajuste(
#    cutias_distance_unif_year$Cosseno,
#    cutias_distance_unif_year$`Polinomial simples`,
#    cutias_distance_hn_year$`Sem termo`,
#    cutias_distance_hn_year$Cosseno,
#    cutias_distance_hn_year$`Hermite polinomial`,
#    cutias_distance_hr_year$`Sem termo`,
#    cutias_distance_hr_year$Cosseno,
#    cutias_distance_hr_year$`Polinomial simples`
#  )
#  
#  melhor_modelo_cutias_year

## ----eval=FALSE---------------------------------------------------------------
#  modelos_cutias_year <- gerar_lista_modelos_selecionados(
#    cutias_distance_hn_year$Cosseno,
#    cutias_distance_hr_year$`Sem termo`,
#    cutias_distance_unif_year$Cosseno,
#    cutias_distance_unif_year$`Polinomial simples`,
#    cutias_distance_hn_year$`Sem termo`,
#    nome_modelos_selecionados = melhor_modelo_cutias_year
#  )
#  
#  plotar_funcao_deteccao_modelos_selecionados(modelos_cutias_year)

## ----eval=FALSE---------------------------------------------------------------
#  #criar uma lista com os modelos selecionados, na ordem de seleção
#  testar_bondade_ajuste(
#    modelos_cutias_year,
#    plot = TRUE,
#    chisq = FALSE,
#  )
#  

## ----eval=FALSE---------------------------------------------------------------
#  gerar_resultados_Distance(
#    dados = modelos_cutias_year,
#    resultado_selecao_modelos = melhor_modelo_cutias_year,
#    tipo_de_resultado = "area_estudo",
#    estratificacao = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  resultado_abundancia <-
#  gerar_resultados_Distance(
#    dados = modelos_cutias_year,
#    resultado_selecao_modelos = melhor_modelo_cutias_year,
#    tipo_de_resultado = "abundancia",
#    estratificacao = TRUE
#  )
#  
#  resultado_abundancia

## ----eval=FALSE---------------------------------------------------------------
#  resultado_abundancia |>
#    dplyr::mutate(ano = as.integer(Regiao)) |>
#    dplyr::group_by(ano) |>
#    dplyr::summarise(n = sum(`Abundancia estimada`)) |>
#    ggplot2::ggplot() +
#    ggplot2::aes(
#      x = ano,
#      y = n
#    ) +
#    ggplot2::geom_line() +
#    ggplot2::theme_minimal()

## ----eval=FALSE---------------------------------------------------------------
#  resultados_densidade <-
#  gerar_resultados_Distance(
#    dados = modelos_cutias_year,
#    resultado_selecao_modelos = melhor_modelo_cutias_year,
#    tipo_de_resultado = "densidade",
#    estratificacao = TRUE
#  )
#  
#  resultados_densidade

## ----eval=FALSE---------------------------------------------------------------
#  resultados_densidade |>
#    dplyr::filter(Rotulo != "Total") |>
#    dplyr::mutate(ano = as.integer(Rotulo)) |>
#    ggplot2::ggplot() +
#    ggplot2::aes(
#      x = ano,
#      y = `Estimativa de densidade`
#    ) +
#    ggplot2::geom_line() +
#    ggplot2::facet_wrap(facets = ggplot2::vars(Modelo)) +
#    ggplot2::theme_minimal()

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_distance_year_hn <- cutias_year_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2015_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2015) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2016_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2016) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2017_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2017) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2018_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2018) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2019_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2019) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2020_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2020) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_2021_distance_hn <- cutias_year_distance |>
#    dplyr::filter(year == 2021) |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  modelo_cutias_estrat_year <- comparar_aic_modelo_estratificado(
#    cutias_distance_year_hn,
#    cutias_2015_distance_hn,
#    cutias_2016_distance_hn,
#    cutias_2017_distance_hn,
#    cutias_2018_distance_hn,
#    cutias_2019_distance_hn,
#    cutias_2020_distance_hn,
#    cutias_2021_distance_hn,
#    nome_modelos = c(
#   "Global",
#   "2015",
#   "2016",
#   "2017",
#   "2018",
#   "2019",
#   "2020",
#   "2021"
#    )
#  )
#  
#  
#  modelo_cutias_estrat_year

## ----eval=FALSE---------------------------------------------------------------
#  modelos_cutias_estrat <- gerar_lista_modelos_selecionados(
#    cutias_distance_year_hn,
#    cutias_2015_distance_hn,
#    cutias_2016_distance_hn,
#    cutias_2017_distance_hn,
#    cutias_2018_distance_hn,
#    cutias_2019_distance_hn,
#    cutias_2020_distance_hn,
#    cutias_2021_distance_hn,
#    nome_modelos_selecionados = modelo_cutias_estrat
#  )
#  
#  plotar_funcao_deteccao_modelos_selecionados(modelos_cutias_estrat)

