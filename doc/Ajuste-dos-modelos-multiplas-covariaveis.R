## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(distanceMonitoraflorestal)

## ----obter dados pra especie--------------------------------------------------
#  cutias_cov <- filtrar_dados(
#    nome_ucs = "resex_tapajos_arapiuns",
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  )
#  
#  cutias_cov_distance <- transformar_dados_formato_Distance(
#    cutias_cov,
#    amostras_repetidas = FALSE)
#  
#  cutias_cov_distance

## ----plotar histograma--------------------------------------------------------
#  cutias_cov_distance |>
#  plotar_distribuicao_distancia_interativo(largura_caixa = 1)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção half-normal, sem covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hn <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%"
#      )
#  
#  cutias_cov_distance_hn$`Sem termo`

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção half-normal, com tamanho do grupo como covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hn_size <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%",
#      formula = ~ size
#      )
#  
#  cutias_cov_distance_hn_size

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção half-normal, com tamanho do grupo como covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hn_size_time <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hn",
#      truncamento = "10%",
#      formula = ~ size + cense_time
#      )
#  
#  cutias_cov_distance_hn_size_time

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate, sem covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hr <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%"
#      )
#  
#  cutias_cov_distance_hr$`Sem termo`

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate, com tamanho do grupo como covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hr_size <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%",
#      formula = ~ size
#      )
#  
#  cutias_cov_distance_hr_size

## ---- eval=FALSE--------------------------------------------------------------
#  # ajustando a função de detecção hazard-rate, com tamanho do grupo como covariável, para um truncamento de 10% dos dados
#  cutias_cov_distance_hr_size_time <- cutias_cov_distance |>
#    ajustar_modelos_Distance(
#      funcao_chave = "hr",
#      truncamento = "10%",
#      formula = ~ size + cense_time
#      )
#  
#  cutias_cov_distance_hr_size_time

## ---- eval=FALSE--------------------------------------------------------------
#  cutias_cov_distance_melhor_modelo <-
#    selecionar_funcao_deteccao_termo_ajuste(
#    cutias_cov_distance_hn$`Sem termo`,
#    cutias_cov_distance_hn_size,
#    cutias_cov_distance_hn_size_time,
#    cutias_cov_distance_hr$`Sem termo`,
#    cutias_cov_distance_hr_size,
#    cutias_cov_distance_hr_size_time
#  )
#  
#  cutias_cov_distance_melhor_modelo
#  

## ---- eval=FALSE--------------------------------------------------------------
#  modelos_cutias_cov_distance <-
#    gerar_lista_modelos_selecionados(
#      cutias_cov_distance_hr_size_time,
#      cutias_cov_distance_hn_size_time,
#      cutias_cov_distance_hn$`Sem termo`,
#      cutias_cov_distance_hn_size,
#      cutias_cov_distance_hr_size,
#      cutias_cov_distance_hr$`Sem termo`,
#      nome_modelos_selecionados = cutias_cov_distance_melhor_modelo
#    )
#  
#  plotar_funcao_deteccao_modelos_selecionados(modelos_cutias_cov_distance)
#  

## ---- eval=FALSE--------------------------------------------------------------
#  testar_bondade_ajuste(
#    modelos_cutias_cov_distance,
#    plot = TRUE,
#    nboot = 100
#  )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  gerar_resultados_Distance(
#    modelos_cutias_cov_distance,
#    cutias_cov_distance_melhor_modelo,
#    tipo_de_resultado = "area_estudo"
#    )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  gerar_resultados_Distance(
#    modelos_cutias_cov_distance,
#    cutias_cov_distance_melhor_modelo,
#    tipo_de_resultado = "abundancia"
#    )
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  gerar_resultados_Distance(
#    modelos_cutias_cov_distance,
#    cutias_cov_distance_melhor_modelo,
#    tipo_de_resultado = "densidade"
#    )
#  

