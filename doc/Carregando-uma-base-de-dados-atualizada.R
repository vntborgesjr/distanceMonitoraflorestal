## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distanceMonitoraflorestal)

## ----eval=FALSE---------------------------------------------------------------
#  # inspecionar a base de dados
#  dplyr::glimpse(monitora_aves_masto_florestal)

## ----eval=FALSE---------------------------------------------------------------
#  # carregar a base de dados do Monitora
#  dados_brutos <- carregar_dados_brutos_xlsx(
#    dados = "data-raw/monitora_masto_aves_2023_04_04.xlsx",
#    sheet = "dados brutos"
#  )
#  
#  head(dados_brutos)
#  dplyr::glimpse(dados_brutos)

## ----eval=FALSE---------------------------------------------------------------
#  # gerar dados completos
#  dados_completos <- gerar_dados_completos(dados_brutos)
#  
#  head(dados_completos)
#  dplyr::glimpse(dados_completos)

## ----eval=FALSE---------------------------------------------------------------
#  # contar observações validadas ao nível de espécie
#  n_obs_validadas <- contar_n_obs_validadas(dados_completos)
#  n_obs_validadas

## ----eval=FALSE---------------------------------------------------------------
#  # filtrar dados por Unidade de Conservação e por espécie
#  dados_filtrados <- filtrar_dados(
#    dados = dados_completos,
#    nome_ucs = "resex_tapajos_arapiuns",
#    nome_sps = "dasyprocta_croconota"
#  )
#  
#  head(dados_filtrados)
#  str(dados_filtrados)

## ----eval=FALSE---------------------------------------------------------------
#  # transformar dados para o formato do pacote Distance
#  dados_transformados <- transformar_dados_formato_Distance(dados_filtrados)
#  
#  head(dados_transformados)
#  dplyr::glimpse(dados_transformados)

