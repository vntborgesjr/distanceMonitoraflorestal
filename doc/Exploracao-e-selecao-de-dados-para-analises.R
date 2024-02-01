## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(distanceMonitoraflorestal)

## -----------------------------------------------------------------------------
# contar número total de UC's 
n_ucs <- contar_n_uc()
n_ucs

## ----eval=FALSE---------------------------------------------------------------
#  # gerar tabela com o número de unidades de conservação amostradas em cada ano
#  n_ucs_ano <- contar_n_uc_ano()
#  n_ucs_ano

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_uc_ano()

## ----eval=FALSE---------------------------------------------------------------
#  n_ano_uc <- contar_n_ano_uc()
#  n_ano_uc

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_ano_uc()

## ----eval=FALSE---------------------------------------------------------------
#  # contar número de observações por UC
#  n_obs_uc <- contar_n_obs_uc()
#  
#  # gerar tabdin
#  gerar_tabdin_n_obs_uc()

## ----eval=FALSE---------------------------------------------------------------
#  # plotar o número de observações por UC
#  plotar_n_obs_uc_interativo()

## ----eval=FALSE---------------------------------------------------------------
#  n_obs_uc_ano <- contar_n_obs_uc_ano()
#  n_obs_uc_ano

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_uc_ano()

## -----------------------------------------------------------------------------
# contar observações validadas ao nível de espécie
n_obs_validadas <- contar_n_obs_validadas()
n_obs_validadas

## ----eval=FALSE---------------------------------------------------------------
#  # gerar gráfico com número observações validadas para cada nível taxonômico
#  plotar_n_obs_validadas_interativo()

## ----eval=FALSE---------------------------------------------------------------
#  # gerar tabela de dados selecionados
#  dados_selecionados <- carregar_dados_selecionados()
#  
#  # gerar tabdin dados_selecionados
#  gerar_tabdin_dados_selecionados()

## -----------------------------------------------------------------------------
n_sp <- contar_n_sp()
n_sp

## ----eval=FALSE---------------------------------------------------------------
#  # contar total sp
#  n_obs_sp <- contar_n_obs_sp()
#  n_obs_sp

## ----eval=FALSE---------------------------------------------------------------
#  # gerar tabela dinâmica com o número total de obsevações por espécie
#  gerar_tabdin_n_obs_sp()

## ----eval=FALSE---------------------------------------------------------------
#  # plotar o o número de observações por UC
#  plotar_n_obs_sp_interativo()

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_sp()

## ----eval=FALSE---------------------------------------------------------------
#  # gerar tabela com o número de observações por espécie e por UC
#  n_obs_sp_uc <- contar_n_obs_sp_uc()
#  n_obs_sp_uc

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_sp_uc()

## ----eval=FALSE---------------------------------------------------------------
#  n_obs_sp_ano <- contar_n_obs_sp_ano()
#  n_obs_sp_ano

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_sp_ano()

## ----eval=FALSE---------------------------------------------------------------
#  n_obs_sp_uc_ano <- contar_n_obs_sp_uc_ano()
#  n_obs_sp_uc_ano

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_sp_uc_ano()

## ----eval=FALSE---------------------------------------------------------------
#  n_obs_sp_uc_estacao_ano <- contar_n_obs_sp_uc_estacao_ano()
#  n_obs_sp_uc_estacao_ano

## ----eval=FALSE---------------------------------------------------------------
#  gerar_tabdin_n_obs_sp_uc_estacao_ano()

