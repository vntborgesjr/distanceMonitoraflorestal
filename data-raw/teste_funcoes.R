#############################################################
################# Teste local do pacote #####################
#############################################################

# remover versao anterior
# remove.packages("distanceMonitoraflorestal")

# instalar o pacotes remotamente
# devtools::install_github(
#   "vntborgesjr/distanceMonitoraflorestal",
#   force = TRUE
# )

# instalar o pacote localmente
# devtools::install()

# carregar o pacote
library(distanceMonitoraflorestal)

# testar a base de dados do pacote monitora_aves_masto_florestal ----------------------------------------

dplyr::glimpse(monitora_aves_masto_florestal)

# testar funcao carregar_dados_brutos() --------------------------------

# carregar os dados brutos
dados_brutos <- carregar_dados_brutos()

# explorar o conteudo dos dados
dplyr::glimpse(dados_brutos)

# conferir a documentacao da funcao carregar_dados_brutos()
# ?carregar_dados_brutos

# testar funcao de gerar_dados_filtrados() -------------------------------

# carregar dados filtrados por uma unidade de conservacao
dados_filtrados_uc1 <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

dplyr::glimpse(dados_filtrados_uc1)

# carregar dados filtrados por mais de uma unidade de conservacao
dados_filtrados_uc2 <- gerar_dados_filtrados(
  nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
)

dplyr::glimpse(dados_filtrados_uc2)

# carregar dados filtrados por uma especie
dados_filtrados_sp1 <- gerar_dados_filtrados(
  nome_sps = "dasyprocta_croconota"
)

dplyr::glimpse(dados_filtrados_sp1)

# carregar dados filtrados por mais de uma especie
dados_filtrados_sp2 <- gerar_dados_filtrados(
  nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
)
dplyr::glimpse(dados_filtrados_sp2)

# carregar dados filtrados por unidade de conservacao e especie
dados_filtrados_uc_sp <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota"
)

dplyr::glimpse(dados_filtrados_uc_sp)

# conferir a documentacao da funcao gerar_dados_filtrados()
# ?gerar_dados_filtrados

# testar funcao gerar_dados_selecionados() --------------------------------------
# carregar dados selecionados
dados_selecionados <- gerar_dados_selecionados()

dplyr::glimpse(dados_selecionados)

# conferir a documentacao da funcao gerar_dados_selecionados()
# ?gerar_dados_selecionados

# testar a funcao contar_n_ano_uc() ---------------------------------------

# gerar o numero de anos em uma Unidade de Conservacao foi amostrada a partir dos dados brutos monitora_ave_masto_florestal
contar_n_ano_uc()

# gerar o numero de anos em uma Unidade de Conservacao foi amostrada a partir dos dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_ano_uc(dados_filtrados)

# gerar o numero de anos em uma Unidade de Conservacao foi amostrada a partir dos dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_ano_uc(dados_selecionados)

# conferir a documentacao da funcao contar_n_ano_uc()
# ?contar_n_ano_uc

# testar a funcao contar_n_obs_sp_ano() -----------------------------------

# numero de observacoes para cada especie por ano - dados brutos
contar_n_obs_sp_ano()

# numero de observacoes para cada especie por ano - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_ano(dados_filtrados)

# numero de observacoes para cada especie por ano - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_sp_ano(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_sp_ano()
# ?contar_n_obs_sp_ano

# testar a funcao contar_n_obs_sp_uc_ano() -----------------------------------

# numero de observacoes para cada especie, em cada UC e por ano - dados brutos
contar_n_obs_sp_uc_ano()

# numero de observacoes para cada especie, em cada UC e por ano - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc_ano(dados_filtrados)

# numero de observacoes para cada especie, em cada UC e por ano - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_sp_uc_ano(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_sp_uc_ano()
# ?contar_n_obs_sp_uc_ano

# testar a funcao contar_n_obs_sp_uc_estacao_ano() -----------------------------------

# numero de observacoes para cada especie, em cada UC, estacao e ano - dados brutos
contar_n_obs_sp_uc_estacao_ano()

# numero de observacoes para cada especie, em cada UC, estacao e ano - dados filtrados
 dados_filtrados <- gerar_dados_filtrados(
 nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc_estacao_ano(dados_filtrados)

# numero de observacoes para cada especie, em cada UC, estacao e ano - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_sp_uc_estacao_ano(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_sp_uc_estacao_ano()
# ?contar_n_obs_sp_uc_estacao_ano

# testar a funcao contar_n_obs_sp_uc() -----------------------------------

# numero de observacoes para cada especie e em cada UC - dados brutos
contar_n_obs_sp_uc()

# numero de observacoes para cada especie e em cada UC - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc(dados_filtrados)

# numero de observacoes para cada especie e em cada UC - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_sp_uc(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_sp_uc()
# ?contar_n_obs_sp_uc

# testar a funcao contar_n_obs_sp() -----------------------------------

# numero de observacoes para cada especie - dados brutos
contar_n_obs_sp()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp(dados_filtrados)

# numero de observacoes para cada especie - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_sp(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_sp()
# ?contar_n_obs_sp

# testar a funcao contar_n_obs_uc_ano() -----------------------------------

# numero de observacoes para cada especie - dados brutos
contar_n_obs_uc_ano()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_uc_ano(dados_filtrados)

# numero de observacoes para cada especie - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_uc_ano(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_uc_ano()
# ?contar_n_obs_uc_ano

# testar a funcao contar_n_obs_uc() -----------------------------------

# numero de observacoes para cada especie - dados brutos
contar_n_obs_uc()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_uc(dados_filtrados)

# numero de observacoes para cada especie - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_uc(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_uc()
# ?contar_n_obs_uc

# testar a funcao contar_n_obs_validadas() -----------------------------------

# tabela de observacoes validadas para cada nivel taxonomico - dados brutos
contar_n_obs_validadas()

# vetor de observacoes validadas para cada nivel taxonomico - dados brutos
contar_n_obs_validadas(retorna_tabela = FALSE)

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_validadas(dados_filtrados)

# numero de observacoes para cada especie - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_obs_validadas(dados_selecionados)

# conferir a documentacao da funcao contar_n_obs_validadas()
# ?contar_n_obs_validadas

# testar a funcao contar_n_obs_validadas() -----------------------------------

# numero total de especies - dados brutos
contar_n_sp()

# numero total de especies para uma UC - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_sp(dados_filtrados)

# numero total de especies - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_sp(dados_selecionados)

# testar a funcao contar_n_uc_ano() -----------------------------------

# numero total de UCs em cada ano - dados brutos
contar_n_uc_ano()

# numero total de UCs em cada ano - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
 nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_uc_ano(dados_filtrados)

# numero total de UCs em cada ano - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_uc_ano(dados_selecionados)

# testar a funcao contar_n_uc_ano() -----------------------------------

# numero total de UCs - dados brutos
contar_n_uc()

# numero total de UCs - dados filtrados
dados_filtrados <- gerar_dados_filtrados(
  nome_sps = "dasyprocta_croconota"
)

contar_n_uc(dados_filtrados)

# numero total de UCs - dados selecionados
dados_selecionados <- gerar_dados_selecionados()

contar_n_uc(dados_selecionados)

# rm(list = ls())

