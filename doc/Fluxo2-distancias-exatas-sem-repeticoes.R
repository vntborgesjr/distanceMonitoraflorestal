## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distanceMonitoraflorestal)

## ----eval=FALSE---------------------------------------------------------------
#  cutia_tap_arap_sem_repeticao <- filtrar_dados(
#    nome_ucs == "resex_tapajos_arapiuns",
#    nome_sps == "dasyprocta_croconota",
#    validacao_obs = "especie"
#  ) |>
#    transformar_dados_formato_Distance(amostras_repetidas = FALSE)
#  
#  cutia_tap_arap_sem_repeticao

## ----eval=FALSE---------------------------------------------------------------
#  cutia_tap_arap_sem_repeticao |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ----eval=FALSE---------------------------------------------------------------
#  # conduz a selecao da melhor distancia de truncamento a partir do ajsute de modelos com funcao de deteccao half-normal sem termos de ajuste
#  cutia_tap_arap_sem_repeticao_dist_trunc <- cutia_tap_arap_sem_repeticao |>
#    selecionar_distancia_truncamento()
#  
#  cutia_tap_arap_sem_repeticao_dist_trunc$selecao

## ----eval=FALSE---------------------------------------------------------------
#  plotar_funcao_deteccao_selecao_distancia_truncamento(cutia_tap_arap_sem_repeticao_dist_trunc)
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção uniforme para um truncamento de 10% dos dados
#  cutia_tap_arap_sem_repeticao_unif <- cutia_tap_arap_sem_repeticao |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      termos_ajuste = "cos",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutia_tap_arap_sem_repeticao_hn <- cutia_tap_arap_sem_repeticao |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate para um truncamento de 10% dos dados
#  cutia_tap_arap_sem_repeticao_hr <- cutia_tap_arap_sem_repeticao |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%")
#  

## ----eval=FALSE---------------------------------------------------------------
#  fluxo2_lista_modelos_ajustados <- list(
#    cutia_tap_arap_sem_repeticao_hn,
#    cutia_tap_arap_sem_repeticao_hr,
#    cutia_tap_arap_sem_repeticao_unif
#  )
#  
#  fluxo2_selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    cutia_tap_arap_sem_repeticao_hn$`Sem termo`,
#    cutia_tap_arap_sem_repeticao_hn$Cosseno,
#    cutia_tap_arap_sem_repeticao_hn$`Hermite polinomial`,
#    cutia_tap_arap_sem_repeticao_hr$`Sem termo`,
#    cutia_tap_arap_sem_repeticao_hr$Cosseno,
#    cutia_tap_arap_sem_repeticao_hr$`Polinomial simples`,
#    cutia_tap_arap_sem_repeticao_unif
#  )
#  
#  fluxo2_selecao_funcao_deteccao_termo_ajuste

## ----eval=FALSE---------------------------------------------------------------
#  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
#  fluxo2_modelos_selecionados <- gerar_lista_modelos_selecionados(
#    cutia_tap_arap_sem_repeticao_unif,
#    cutia_tap_arap_sem_repeticao_hn$`Sem termo`,
#    cutia_tap_arap_sem_repeticao_hr$`Sem termo`,
#    nome_modelos_selecionados = fluxo2_selecao_funcao_deteccao_termo_ajuste
#  )
#  
#  # plotar a probabilidade de detecção observada (barras) e a esperada (linhas e pontos)
#  plotar_funcao_deteccao_modelos_selecionados(fluxo2_modelos_selecionados)

## ----eval=FALSE---------------------------------------------------------------
#  bondade_ajuste_fluxo2 <- testar_bondade_ajuste(
#    fluxo2_modelos_selecionados,
#    plot = TRUE,
#    chisq = TRUE,
#    intervalos_distancia  =  NULL
#  )
#  
#  bondade_ajuste_fluxo2

## ----eval=FALSE---------------------------------------------------------------
#  # gera resultados área de estudo e taxa de encontro
#  fluxo2_caracteristicas_area_estudo_taxa_encontro <- gerar_resultados_Distance(
#    dados = fluxo2_modelos_selecionados,
#    resultado_selecao_modelos = fluxo2_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "area_estudo",
#    estratificacao = FALSE
#  )
#  
#  fluxo2_caracteristicas_area_estudo_taxa_encontro

## ----eval=FALSE---------------------------------------------------------------
#  # gera resultados abundância e detecção
#  fluxo2_caracteristicas_abundancia <- gerar_resultados_Distance(
#    dados = fluxo2_modelos_selecionados,
#    resultado_selecao_modelos = fluxo2_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "abundancia",
#    estratificacao = FALSE
#  )
#  
#  fluxo2_caracteristicas_abundancia

## ----eval=FALSE---------------------------------------------------------------
#  # gera resultados densidade e coeficiente de variação
#  fluxo2_caracteristicas_densidade <- gerar_resultados_Distance(
#    dados = fluxo2_modelos_selecionados,
#    resultado_selecao_modelos = fluxo2_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "densidade",
#    estratificacao = FALSE
#  )
#  
#  fluxo2_caracteristicas_densidade

