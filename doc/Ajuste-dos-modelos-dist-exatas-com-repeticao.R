## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup, eval=FALSE--------------------------------------------------------
#  library(distanceMonitoraflorestal)

## ---- eval=FALSE--------------------------------------------------------------
#  # carregar dados
#  cutia_tap_arap <- filtrar_dados(
#    nome_ucs = "resex_tapajos_arapiuns",
#    nome_sps = "dasyprocta_croconota"
#  ) |>
#    transformar_dados_formato_Distance()
#  
#  cutia_tap_arap

## ---- fig.height=15, fig.width=10, warning=FALSE------------------------------
#  cutia_tap_arap |>
#    tidyr::drop_na(distance) |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)

## ---- eval=FALSE--------------------------------------------------------------
#  # conduz a selecao da melhor distancia de truncamento a partir do ajsute de modelos com funcao de deteccao half-normal sem termos de ajuste
#  cutia_tap_arap_dist_trunc <- cutia_tap_arap |>
#    selecionar_distancia_truncamento()
#  
#  cutia_tap_arap_dist_trunc$selecao

## ---- eval=FALSE--------------------------------------------------------------
#  plotar_funcao_deteccao_selecao_distancia_truncamento(cutia_tap_arap_dist_trunc)

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção uniforme para um truncamento de 10% dos dados
#  cutia_tap_arap_unif <- cutia_tap_arap |>
#    ajustar_modelos_Distance(
#      funcao_chave = "unif",
#      truncamento = "10%")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção half-normal para um truncamento de 10% dos dados
#  cutia_tap_arap_hn <- cutia_tap_arap |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%")

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate para um truncamento de 10% dos dados
#  cutia_tap_arap_hr <- cutia_tap_arap |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%")

## ---- eval=FALSE--------------------------------------------------------------
#  fluxo1_selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    cutia_tap_arap_hn$`Sem termo`,
#    cutia_tap_arap_hn$Cosseno,
#    cutia_tap_arap_hn$`Hermite polinomial`,
#    cutia_tap_arap_hr$`Sem termo`,
#    cutia_tap_arap_hr$Cosseno,
#    cutia_tap_arap_hr$`Polinomial simples`,
#    cutia_tap_arap_unif$Cosseno,
#    cutia_tap_arap_unif$`Polinomial simples`
#  )
#  
#  fluxo1_selecao_funcao_deteccao_termo_ajuste

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
#  fluxo1_modelos_selecionados <- gerar_lista_modelos_selecionados(
#    cutia_tap_arap_hr$`Sem termo`,
#    cutia_tap_arap_unif$`Cosseno`,
#    cutia_tap_arap_hn$`Cosseno`,
#    cutia_tap_arap_unif$`Polinomial simples`,
#    cutia_tap_arap_hn$`Sem termo`,
#    nome_modelos_selecionados = fluxo1_selecao_funcao_deteccao_termo_ajuste
#  )
#  
#  # plotar a probabilidade de detecção observada (barras) e a esperada (linhas e pontos)
#  plotar_funcao_deteccao_modelos_selecionados(fluxo1_modelos_selecionados)

## ---- eval=FALSE--------------------------------------------------------------
#  bondade_ajuste_fluxo1 <- testar_bondade_ajuste(
#    fluxo1_modelos_selecionados,
#    plot = TRUE,
#    chisq = TRUE,
#    intervalos_distancia  =  NULL
#  )
#  
#  bondade_ajuste_fluxo1

## ---- eval=FALSE--------------------------------------------------------------
#  # gera resultados área de estudo e taxa de encontro
#  fluxo1_caracteristicas_area_estudo_taxa_encontro <- gerar_resultados_Distance(
#    dados = fluxo1_modelos_selecionados,
#    resultado_selecao_modelos = fluxo1_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "area_estudo",
#    estratificacao = FALSE
#  )
#  
#  fluxo1_caracteristicas_area_estudo_taxa_encontro

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados abundância e detecção
#  fluxo1_caracteristicas_esforco_abundancia_deteccao <- gerar_resultados_Distance(
#    dados = fluxo1_modelos_selecionados,
#    resultado_selecao_modelos = fluxo1_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "abundancia",
#    estratificacao = FALSE
#  )
#  
#  fluxo1_caracteristicas_esforco_abundancia_deteccao

## ---- eval=FALSE--------------------------------------------------------------
#  # gerar resultados densidade e coeficiente de variação
#  fluxo1_caracteristicas_densidade <- gerar_resultados_Distance(
#    dados = fluxo1_modelos_selecionados,
#    resultado_selecao_modelos = fluxo1_selecao_funcao_deteccao_termo_ajuste,
#    tipo_de_resultado = "densidade",
#    estratificacao = FALSE
#  )
#  
#  fluxo1_caracteristicas_densidade

