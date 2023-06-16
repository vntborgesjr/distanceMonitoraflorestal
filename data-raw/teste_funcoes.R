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

# testar a funcao contar_n_ano_uc() ---------------------------------------

# gerar o numero de anos em uma Unidade de Conservacao foi amostrada a partir dos dados brutos monitora_ave_masto_florestal
contar_n_ano_uc()

# gerar o numero de anos em uma Unidade de Conservacao foi amostrada a partir dos dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota"
)

contar_n_ano_uc(dados_filtrados)

# conferir a documentacao da funcao contar_n_ano_uc()
# ?contar_n_ano_uc

# testar a funcao contar_n_obs_sp_ano() -----------------------------------

# numero de observacoes para cada especie por ano - dados brutos
contar_n_obs_sp_ano()

# numero de observacoes para cada especie por ano - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_ano(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_sp_ano()
# ?contar_n_obs_sp_ano

# testar a funcao contar_n_obs_sp_uc_ano() -----------------------------------

# numero de observacoes para cada especie, em cada UC e por ano - dados brutos
contar_n_obs_sp_uc_ano()

# numero de observacoes para cada especie, em cada UC e por ano - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc_ano(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_sp_uc_ano()
# ?contar_n_obs_sp_uc_ano

# testar a funcao contar_n_obs_sp_uc_estacao_ano() -----------------------------------
# numero de observacoes para cada especie, em cada UC, estacao e ano - dados brutos
contar_n_obs_sp_uc_estacao_ano()

# numero de observacoes para cada especie, em cada UC, estacao e ano - dados filtrados
dados_filtrados <- filtrar_dados(
 nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc_estacao_ano(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_sp_uc_estacao_ano()
# ?contar_n_obs_sp_uc_estacao_ano

# testar a funcao contar_n_obs_sp_uc() -----------------------------------

# numero de observacoes para cada especie e em cada UC - dados brutos
contar_n_obs_sp_uc()

# numero de observacoes para cada especie e em cada UC - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp_uc(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_sp_uc()
# ?contar_n_obs_sp_uc

# testar a funcao contar_n_obs_sp() -----------------------------------
# numero de observacoes para cada especie - dados brutos
contar_n_obs_sp()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_sp(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_sp()
# ?contar_n_obs_sp

# testar a funcao contar_n_obs_uc_ano() -----------------------------------
# numero de observacoes para cada especie - dados brutos
contar_n_obs_uc_ano()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_uc_ano(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_uc_ano()
# ?contar_n_obs_uc_ano

# testar a funcao contar_n_obs_uc() -----------------------------------
# numero de observacoes para cada especie - dados brutos
contar_n_obs_uc()

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_uc(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_uc()
# ?contar_n_obs_uc

# testar a funcao contar_n_obs_validadas() -----------------------------------
# tabela de observacoes validadas para cada nivel taxonomico - dados brutos
contar_n_obs_validadas()

# vetor de observacoes validadas para cada nivel taxonomico - dados brutos
contar_n_obs_validadas(retorna_tabela = FALSE)

# numero de observacoes para cada especie - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_obs_validadas(dados_filtrados)

# conferir a documentacao da funcao contar_n_obs_validadas()
# ?contar_n_obs_validadas

# testar a funcao contar_n_obs_validadas() -----------------------------------
# numero total de especies - dados brutos
contar_n_sp()

# numero total de especies para uma UC - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_sp(dados_filtrados)

# testar a funcao contar_n_uc_ano() -----------------------------------
# numero total de UCs em cada ano - dados brutos
contar_n_uc_ano()

# numero total de UCs em cada ano - dados filtrados
dados_filtrados <- filtrar_dados(
 nome_ucs = "resex_tapajos_arapiuns"
)

contar_n_uc_ano(dados_filtrados)

# testar a funcao contar_n_uc_ano() -----------------------------------
# numero total de UCs - dados brutos
contar_n_uc()

# numero total de UCs - dados filtrados
dados_filtrados <- filtrar_dados(
  nome_sps = "dasyprocta_croconota"
)

contar_n_uc(dados_filtrados)

# testar função transformar_dados_formato_Distance() ----------------------------
# gerar os dados transformados com repeticao
dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)

dplyr::glimpse(dados_distance_com_repeticao)

# gerar os dados transformados sem repeticao
dados_distance_sem_repeticao <- transformar_dados_formato_Distance(dados_filtrados, amostras_repetidas = FALSE)

dplyr::glimpse(dados_distance_sem_repeticao)

# testar função ajustar_modelos_distance() --------------------------------
# gerar dados filtrados para a uma espécie e uma UC e transformar para o formato para a análise no pacote Distance
dados_dasyp_croco_sem_repeticao <- filtrar_dados(
  dados = monitora_aves_masto_florestal,
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota",
  validacao_obs = "especie"
) |>
  transformar_dados_formato_Distance(amostras_repetidas = FALSE)

# ajustar modelo com função chave Half-normal e todas os termos de ajsute possíveis
modelo_hn_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hn",
    termos_ajuste = NULL,
    truncamento = "10%"
  )

modelo_hn_dasyp_croco_resex_tap_arap

# ajustar modelo com função chave Hazard-rate e todas os termos de ajsute possíveis
modelo_hr_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    termos_ajuste = NULL,
    truncamento = "10%"
  )

# ajustar modelo com função chave Uniform e todas os termos de ajsute possíveis
modelo_unif_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "unif",
    termos_ajuste = NULL,
    truncamento = "10%"
  )

# ajustar modelo com função chave Hazard-rate e o termo de ajuste Cosseno
modelo_hr_dasyp_croco_resex_tap_arap <- dados_dasyp_croco_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    termos_ajuste = "cos",
    truncamento = "10%"
  )

# Exemplo função definir_intervalos_distancia() ---------------------------
# carregar pacote
library(dplyr)

# gerar dados filtrados
dados_filtrados <- filtrar_dados(
  nome_uc = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota",
  validacao_obs = "especie"
)

# gerar os dados transformados com repetição
dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)

# definir intervalos de distância regulares (binagem)
dados_dasy_croc_tap_arap_com_repeticao_binados <- dados_dasy_croc_tap_arap_com_repeticao |>
  definir_intervalos_distancia(intervalos_distancia = seq(
    from = 0,
    to  = 55,
    by = 1.5
  ))

glimpse(dados_dasy_croc_tap_arap_com_repeticao_binados)

# definir intervalos de distância diferentes

dados_dasy_croc_tap_arap_com_repeticao_binados_diferentes <- dados_dasy_croc_tap_arap_com_repeticao |>
  definir_intervalos_distancia(
    intervalos_distancia = c(0, seq(
      from = 1,
      to = 55,
      by = 1.4
    ))
  )

dplyr::glimpse(dados_dasy_croc_tap_arap_com_repeticao_binados)

# Exemplo função selecionar_funcao_deteccao_termo_ajuste() -----------------------------------------------
# ajustar modelos com funções chave diferentes
# modelo Half-normal
modelo_hn <- dados_dasy_croc_tap_arap_com_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hn",
    truncamento = 15
  )

# modelo Hazard-rate
modelo_hr <- dados_dasy_croc_tap_arap_com_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    truncamento = 15
  )

# modelo Uniform
modelo_unif <- dados_dasy_croc_tap_arap_com_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "unif",
    truncamento = 15
  )

# gerar a tabela de seleção com o resumo comparativo dos modelos
selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
  modelo_hn$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_hn$`Hermite polinomial`,
  modelo_hr$`Sem termo`,
  modelo_hr$Cosseno,
  modelo_hr$`Polinomial simples`,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`
)

# Exemplo da função testar_bondade_ajuste() ----------------------------------
# gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hr$`Sem termo`,
  modelo_hn$Cosseno,
  modelo_unif$Cosseno,
  modelo_unif$`Polinomial simples`,
  modelo_hn$`Sem termo`,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
)

# teste de bondade de ajuste dos modelos e Q-Q plots
bondade_ajuste <- testar_bondade_ajuste(
  lista_modelos_selecionados,
  plot = TRUE,
  chisq = FALSE,
  intervalos_distancia =  NULL
)

bondade_ajuste

# rm(list = ls())
# modelos com covariáveis
# gerar dados sem repetição
cutia_resex_sem_repeticao <- filtrar_dados(
  nome_ucs = "resex_tapajos_arapiuns",
  nome_sps = "dasyprocta_croconota",
  validacao_obs = "especie"
) |>
  transformar_dados_formato_Distance(amostras_repetidas = FALSE)

# ajustar modelos sem covariáveis
# Half-normal
modelo_hn_sem_covariavel <- cutia_resex_sem_repeticao |>
  ajustar_modelos_Distance(
  funcao_chave = "hn",
  termos_ajuste = NULL,
  truncamento = "10%",
  formula = ~ 1
)

# Hazard-rate
modelo_hr_sem_covariavel <- cutia_resex_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    termos_ajuste = NULL,
    truncamento = "10%",
    formula = ~ 1
  )

# Uniforme
# modelo_unif_sem_covariavel <- cutia_resex_sem_repeticao |>
#   ajustar_modelos_Distance(
#     funcao_chave = "unif",
#     termos_ajuste = NULL,
#     truncamento = "10%",
#     formula = ~ 1
#   )

# ajustar modelos com covariáveis
# Half-normal
modelo_hn_com_covariavel <- cutia_resex_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hn",
    termos_ajuste = NULL,
    truncamento = "10%",
    formula = ~ size
  )

# Hazard-rate
modelo_hr_com_covariavel <- cutia_resex_sem_repeticao |>
  ajustar_modelos_Distance(
    funcao_chave = "hr",
    termos_ajuste = NULL,
    truncamento = "10%",
    formula = ~ size
  )

# comparação entre os modelos
selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
  modelo_hn_sem_covariavel$`Sem termo`,
  modelo_hn_sem_covariavel$Cosseno,
  modelo_hn_sem_covariavel$`Hermite polinomial`,
  modelo_hr_sem_covariavel$`Sem termo`,
  modelo_hr_sem_covariavel$Cosseno,
  modelo_hr_sem_covariavel$`Polinomial simples`,
  modelo_hn_com_covariavel,
  modelo_hr_com_covariavel
)

lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
  modelo_hn_sem_covariavel$`Sem termo`,
  modelo_hr_sem_covariavel$`Sem termo`,
  modelo_hn_com_covariavel,
  modelo_hr_com_covariavel,
  nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
)

# teste de bondade de ajuste dos modelos e Q-Q plots
bondade_ajuste <- testar_bondade_ajuste(
  lista_modelos_selecionados,
  plot = TRUE,
  chisq = FALSE,
  intervalos_distancia =  NULL
)

bondade_ajuste
