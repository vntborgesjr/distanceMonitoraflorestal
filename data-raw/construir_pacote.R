
# Carregar pacotes necessarios --------------------------------------------

# install.packages(c("devtools", "roxygen2","usethis", "testthat"))

# Carregar pacotes --------------------------------------------------------
#library(devtools)
#library(roxygen2)
#library(usethis)
#library(testthat)
#library(qpdf)

# Inicializando o pacote --------------------------------------------------

# usethis::create_package("distanceMonitoraflorestal")

# Adicionar descricao ao pacote -------------------------------------------

# devtools::create(getwd())

# Adicionar a versao do pacote --------------------------------------------

# usethis::use_version()

# Adicionar licenca ao pacote ---------------------------------------------

# usethis::use_mit_license(copyright_holder = "Vitor Borges-Junior")

# Adicionar dataset disponivel ao usuario ---------------------------------
# usethis::use_data_raw("monitora_masto_aves_2023_04_04.xlsx")
#
# Adicionar datasets ao pacote --------------------------------------------
# essa etapa deve ser executada no script que e aberto automaticamente
# apos a execucao do comando usethis::use_data_raw()
# monitora_aves_masto_florestal <- readxl::read_excel(
#   path = "data-raw/monitora_masto_aves_2023_04_04.xlsx",
#   sheet = "dados brutos"
# )
# usethis::use_data(
#   monitora_aves_masto_florestal,
#   overwrite = TRUE
# )
#
# Adicionar funcoes e respectivos testes individuais ao pacote ---------------------------------------------
# configurar o pacote para usar o testthat
# usethis::use_testthat(3)
#
# usethis::use_r("ajustar_modelos_Distance.R")
# usethis::use_test("ajustar_modelos_Distance.R")
# testthat::test_file("tests/testthat/test-ajustar_modelos_Distance.R")
#
# usethis::use_r("contar_n_ano_uc.R")
# usethis::use_test("contar_n_ano_uc.R")
# testthat::test_file("tests/testthat/test-contar_n_ano_uc.R")
#
# usethis::use_r("contar_n_obs_sp.R")
# usethis::use_test("contar_n_obs_sp.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp.R")
#
# usethis::use_r("contar_n_obs_sp_ano.R")
# usethis::use_test("contar_n_obs_sp_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp_ano.R")
#
# usethis::use_r("contar_n_obs_sp_uc.R")
# usethis::use_test("contar_n_obs_sp_uc.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp_uc.R")
#
# usethis::use_r("contar_n_obs_sp_uc_ano.R")
# usethis::use_test("contar_n_obs_sp_uc_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp_uc_ano.R")
#
# usethis::use_r("contar_n_obs_sp_uc_estacao_ano.R")
# usethis::use_test("contar_n_obs_sp_uc_estacao_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp_uc_estacao_ano.R")
#
# usethis::use_r("contar_n_obs_uc.R")
# usethis::use_test("contar_n_obs_uc.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_uc.R")
#
# usethis::use_r("contar_n_obs_uc_ano.R")
# usethis::use_test("contar_n_obs_uc_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_uc_ano.R")
#
# usethis::use_r("contar_n_obs_validadas.R")
# usethis::use_test("contar_n_obs_validadas.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_validadas.R")
#
# usethis::use_r("contar_n_sp.R")
# usethis::use_test("contar_n_sp.R")
# testthat::test_file("tests/testthat/test-contar_n_sp.R")
#
# usethis::use_r("contar_n_uc.R")
# usethis::use_test("contar_n_uc.R")
# testthat::test_file("tests/testthat/test-contar_n_uc.R")
#
# usethis::use_r("contar_n_uc_ano.R")
# usethis::use_test("contar_n_uc_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_uc_ano.R")
#
# usethis::use_r("definir_intervalo_distancia.R")
# usethis::use_test("definir_intervalo_distancia.R")
# testthat::test_file("tests/testthat/test-definir_intervalo_distancia.R")
#
# usethis::use_r("filtrar_dados.R")
# usethis::use_test("filtrar_dados.R")
# testthat::test_file("tests/testthat/test-filtrar_dados.R")
#
# usethis::use_r("gerar_lista_modelos_selecionados.R")
# usethis::use_test("gerar_lista_modelos_selecionados.R")
# testthat::test_file("tests/testthat/test-gerar_lista_modelos_selecionados.R")
#
# usethis::use_r("gerar_resultados_Distance.R")
# usethis::use_test("gerar_resultados_Distance.R")
# testthat::test_file("tests/testthat/test-gerar_resultados_Distance.R")
#
# usethis::use_r("gerar_tabdin.R")
# usethis::use_test("gerar_tabdin.R")
# testthat::test_file("tests/testthat/test-gerar_tabdin.R")
#
# usethis::use_r("monitora_aves_masto_florestal.R")
# usethis::use_test("monitora_aves_masto_florestal.R")
# testthat::test_file("tests/testthat/test-monitora_aves_masto_florestal.R")
#
# usethis::use_r("plotar_distribuicao_distancia_interativo.R")
# usethis::use_test("plotar_distribuicao_distancia_interativo.R")
# testthat::test_file("tests/testthat/test-plotar_distribuicao_distancia_interativo.R")
#
# usethis::use_r("plotar_funcao_deteccao_modelos_selecionados.R")
# usethis::use_test("plotar_funcao_deteccao_modelos_selecionados.R")
# testthat::test_file("tests/testthat/test-plotar_funcao_deteccao_modelos_selecionados.R")
#
# usethis::use_r("plotar_funcao_deteccao_selecao_distancia_truncamento.R")
# usethis::use_test("plotar_funcao_deteccao_selecao_distancia_truncamento.R")
# testthat::test_file("tests/testthat/test-plotar_funcao_deteccao_selecao_distancia_truncamento.R")
#
# usethis::use_r("selecionar_distancia_truncamento.R")
# usethis::use_test("selecionar_distancia_truncamento.R")
# testthat::test_file("tests/testthat/test-selecionar_distancia_truncamento.R")
#
# usethis::use_r("selecionar_funcao_deteccao_termo_ajuste.R")
# usethis::use_test("selecionar_funcao_deteccao_termo_ajuste.R")
# testthat::test_file("tests/testthat/test-selecionar_funcao_deteccao_termo_ajuste.R")
#
# usethis::use_r("testar_bondade_ajuste.R")
# usethis::use_test("testar_bondade_ajuste.R")
# testthat::test_file("tests/testthat/test-testar_bondade_ajuste.R")
#
# usethis::use_r("transformar_dados_formato_Distance.R")
# usethis::use_test("transformar_dados_formato_Distance.R")
# testthat::test_file("tests/testthat/test-transformar_dados_formato_Distance.R")
#
# rodar todos os testes de uma so vez
# devtools::test()
#
# Carergar todas as funcoes -----------------------------------------------
#
# devtools::load_all()
#
# Checar o funcionamento do pacote ----------------------------------------
# nessa etapa os arquivos de documentacao e NAMESPACE sao criados/recriados
# devtools::check()
#
# Controle de versao com Git a GitHub -------------------------------------

# usethis::use_git()
# usethis::create_github_token() # criar um novo token de acesso ao github
# usethis::edit_r_environ() # contem o último token de acesso ao github
# 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
# usethis::use_github() # cria um novo repositório no github

# Criar documentação ------------------------------------------------------

# devtools::document()

# criar testes individuais de cada funcao ---------------------------------
# rodar na primeira vez ou sempre que inserir uma nova funcao
# minhas_funcoes <- c(
#  "carregar_dados_brutos_xlsx"
# )
# usethis::use_test()
#
# rodar quando quiser repetir os testes existentes. para adicionar novos testest, rodar as linha anteriores
# devtools::test()
#
# Adicionar um arquivo README ---------------------------------------------
# usethis::use_readme_md()
#
# adicionar estagio do pacote: "epxerimental"
# usethis::use_lifecycle_badge("experimental")
#
# Adicionar vignette -------------------------------------------------------------------------
# usethis::use_vignette("Checagem-filtragem-e-transformação-dos-dados")
# usethis::use_vignette("Exploracao-e-selecao-de-dados-para-analises")
# usethis::use_vignette("Ajuste-dos-modelos-fluxo1-distancias-exatas-com-repeticoes")
# usethis::use_vignette("Ajuste-dos-modelos-fluxo2-distancias-exatas-sem-repeticoes")
# usethis::use_vignette("Ajuste-dos-modelos-fluxo3-distancias-agrupadas-com-repeticoes")
#
# construir o html para ser chamado usando vignettes()
# devtools::build_vignettes()
#
# Instalar localmente o pacote didstanceMonitorafloresta ------------------

# devtools::install_github("vntborgesjr/distanceMonitoraflorestal")
#

# resolvendo variaveis globais --------------------------------------------
# adicionar todas as variaveis
# utils::globalVariables()
#
# informar que o pacote e escrito em portugues ----------------------------


# e-mail Caio Lente
# clente@curso-r.com


# coisas que faltam ser feitas --------------------------------------------

# parte de exploração de dados --------------------------------------------
# COVARIÁVEIS
# 1 - gráficos distância (y) x covariável (x)

#RÉPLICAS E REPETIÇÕES
# 2 - Número de UCs para cada espécie
# 3 - Número de réplicas (estações amostrais) para cada espécie
# 4 - Número de réplicas (estações amostrais) para cada espécie por UC
# 5 - Número de repetições por estação amostral para cada espécie
# 6 - Número de repetições por estação amostral por UC para cada espécie
# 7 - Número de repetições por estação amostral por UC para cada espécie por
# ano
# 8 - Número total de repetições por estação amostral por UC para cada
# espécie por períod amostral (estações do ano/ano)

# Documentação e testes individuais das funções ------------------------------------------------------------
# 9 - funções gerar_lista_modelos(), selecionar_funcao_deteccao_termo_ajuste()
# estão com problemas nos testes e exemplos
# 10 - salvar como arquivos de dados objetos que levam muito tempo de execução
# para serem gerados e usá-los nos exemplos
# 12 - funções de gráfico plotar_distribuicao_distancia_interativo(),
#  gerar_resultados_Distance(), selecionar_distancia_truncamento(),
# plotar_funcao_deteccao_modelos_selecionados()... sem
# teste e documentacao
# 13 - função plotar_funcao_deteccao_modelos_selecionados() sem teste
# 14 - gerar resultados distance sem documenação e sem teste

# Aspectos importantes para revisão final do texto do tutorial_fluxo_analises.Rmd
# 1 - padronizar Unidade de Conservação ou UC
# 2 - evitar estações amostrais com o mesmo nome
