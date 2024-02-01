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
#  # retornar a base de dados completa
#  filtrar_dados()

## ----eval=FALSE---------------------------------------------------------------
#  # consultar o nome das UCs
#  nomes_ucs <- monitora_aves_masto_florestal |>
#    dplyr::distinct(nome_uc)
#  
#  nomes_ucs

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma UC fornecendo o nome
#  filtrar_dados(nome_ucs = "resex_tapajos_arapiuns")

## ----eval=FALSE---------------------------------------------------------------
#  # gerar um vetor contendo os nomes das UCs
#  nomes_ucs <- dplyr::pull(nomes_ucs)
#  nomes_ucs

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma UC fornecendo o nome
#  filtrar_dados(nome_ucs = nomes_ucs[1])

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de mais de uma UC
#  filtrar_dados(nome_ucs = nomes_ucs[2:3])

## ----eval=FALSE---------------------------------------------------------------
#  # consultar o nome das UCs
#  nomes_sps <- monitora_aves_masto_florestal |>
#    dplyr::distinct(nome_sp)
#  
#  nomes_sps

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma espécie fornecendo o nome
#  filtrar_dados(nome_sps = "dasyprocta_croconota")

## ----eval=FALSE---------------------------------------------------------------
#  # gerar um vetor contendo os nomes das UCs
#  nomes_sps <- dplyr::pull(nomes_sps)
#  nomes_sps

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma UC fornecendo o nome
#  filtrar_dados(nome_sps = nomes_sps[1])

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de mais de uma UC
#  filtrar_dados(nome_sps = nomes_sps[c(33, 34)])

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma espécie fornecendo o nome
#  filtrar_dados(
#    nome_sps = nomes_sps[c(33, 34)],
#    validacao_obs = "especie"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # retornar as observações de uma espécie fornecendo o nome
#  filtrar_dados(
#    nome_ucs = "flona_do_jamari",
#    nome_sps = "callicebus_brunneus",
#    validacao_obs = "especie"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  dasy_croc <- filtrar_dados(
#    nome_sps = "dasyprocta_croconota",
#    validacao_obs = "especie"
#  )

## ----eval=FALSE---------------------------------------------------------------
#  # transformar os dados para o formato do Distance
#  dasy_croc_com_rep <- transformar_dados_formato_Distance(dasy_croc)
#  
#  dasy_croc_com_rep

## ----eval=FALSE---------------------------------------------------------------
#  dasy_croc_sem_rep <- transformar_dados_formato_Distance(
#    dados = dasy_croc,
#    amostras_repetidas = FALSE
#  )
#  
#  dasy_croc_sem_rep

