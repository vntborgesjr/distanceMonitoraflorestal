## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distanceMonitoraflorestal)

## ----eval=FALSE---------------------------------------------------------------
#  # filtrar dados das cutias para as quatro UCS onde ocorre:
#  cutias <- filtrar_dados(
#    nome_ucs %in% c(
#      "resex_tapajos_arapiuns",
#      "parna_da_serra_do_pardo",
#      "esec_da_terra_do_meio",
#      "resex_riozinho_do_anfrisio"
#    ),
#    nome_sps == "dasyprocta_croconota",
#    validacao_obs = "especie"
#  )
#  
#  cutias_distance <- transformar_dados_formato_Distance(
#    cutias,
#    amostras_repetidas = TRUE)
#  
#  cutias_distance

## ----eval=FALSE---------------------------------------------------------------
#  cutias_distance |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  cutias_tap_distance <- filtrar_dados(
#    nome_ucs = "resex_tapajos_arapiuns",
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  ) |>
#    transformar_dados_formato_Distance(amostras_repetidas = TRUE)
#  
#  cutias_tap_distance |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  cutias_par_distance <- filtrar_dados(
#    nome_ucs = "parna_da_serra_do_pardo",
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  )|>
#    transformar_dados_formato_Distance(amostras_repetidas = TRUE)
#  
#  cutias_par_distance |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  cutias_ter_distance <- filtrar_dados(
#    nome_ucs = "esec_da_terra_do_meio",
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  ) |>
#    transformar_dados_formato_Distance(amostras_repetidas = TRUE)
#  
#  cutias_ter_distance |>
#    tidyr::drop_na(distance) |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  cutias_anf_distance <- filtrar_dados(
#    nome_ucs = "resex_riozinho_do_anfrisio",
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  ) |>
#    transformar_dados_formato_Distance(amostras_repetidas = TRUE)
#  
#  cutias_anf_distance |>
#    plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção uniforme para um truncamento de 10% dos dados
#  cutias_distance_unif <- cutias_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_distance_hn <- cutias_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate para um truncamento de 10% dos dados
#  cutias_distance_hr <- cutias_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  melhor_modelo_cutias <- selecionar_funcao_deteccao_termo_ajuste(
#    cutias_distance_unif$Cosseno,
#    cutias_distance_unif$`Polinomial simples`,
#    cutias_distance_hn$`Sem termo`,
#    cutias_distance_hn$Cosseno,
#    cutias_distance_hn$`Hermite polinomial`,
#    cutias_distance_hr$`Sem termo`,
#    cutias_distance_hr$Cosseno,
#    cutias_distance_hr$`Polinomial simples`
#  )
#  
#  melhor_modelo_cutias

## ----eval=FALSE---------------------------------------------------------------
#  modelos_cutias <- gerar_lista_modelos_selecionados(
#    cutias_distance_hr$`Sem termo`,
#    cutias_distance_hn$Cosseno,
#    cutias_distance_unif$Cosseno,
#    cutias_distance_unif$`Polinomial simples`,
#    cutias_distance_hn$`Sem termo`,
#    nome_modelos_selecionados = melhor_modelo_cutias
#  )
#  
#  plotar_funcao_deteccao_modelos_selecionados(modelos_cutias)

## ----eval=FALSE---------------------------------------------------------------
#  #criar uma lista com os modelos selecionados, na ordem de seleção
#  testar_bondade_ajuste(
#    modelos_cutias,
#    plot = TRUE,
#    chisq = FALSE,
#  )
#  

## ----eval=FALSE---------------------------------------------------------------
#  gerar_resultados_Distance(
#    dados = modelos_cutias,
#    resultado_selecao_modelos = melhor_modelo_cutias,
#    tipo_de_resultado = "area_estudo",
#    estratificacao = TRUE
#  )

## ----eval=FALSE---------------------------------------------------------------
#  
#  gerar_resultados_Distance(
#    dados = modelos_cutias,
#    resultado_selecao_modelos = melhor_modelo_cutias,
#    tipo_de_resultado = "abundancia",
#    estratificacao = TRUE
#  )
#  

## ----eval=FALSE---------------------------------------------------------------
#  
#  gerar_resultados_Distance(
#    dados = modelos_cutias,
#    resultado_selecao_modelos = melhor_modelo_cutias,
#    tipo_de_resultado = "densidade",
#    estratificacao = TRUE
#  )
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_distance_hn <- cutias_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_tap_distance_hn <- cutias_tap_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_tap_distance_hn <- cutias_tap_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_par_distance_hn <- cutias_par_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_ter_distance_hn <- cutias_ter_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutias_anf_distance_hn <- cutias_anf_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      termos_ajuste = "cos",
#      truncamento = "10%")

## ----eval=FALSE---------------------------------------------------------------
#  modelo_cutias_estrat <- comparar_aic_modelo_estratificado(
#    cutias_distance_hn$Cosseno,
#    cutias_tap_distance_hn,
#    cutias_par_distance_hn,
#    cutias_ter_distance_hn,
#    cutias_anf_distance_hn,
#    nome_modelos = c(
#   "Global",
#   "Resex Tapajós-Arapiuns",
#   "Parna Serra do Prado",
#   "Esec da Terra do Meio",
#   "Resex do Riozinho do Anfrísio"
#    )
#  )
#  
#  
#  modelo_cutias_estrat

## ----eval=FALSE---------------------------------------------------------------
#  modelos_cutias_estrat <- gerar_lista_modelos_selecionados(
#    cutias_distance_hn$Cosseno,
#    cutias_tap_distance_hn,
#    cutias_par_distance_hn,
#    cutias_ter_distance_hn,
#    cutias_anf_distance_hn,
#    nome_modelos_selecionados = modelo_cutias_estrat
#  )
#  
#  plotar_funcao_deteccao_modelos_selecionados(modelos_cutias_estrat)

