
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
# usethis::use_r("contar_n_obs_uc_ano.R")
# usethis::use_test("contar_n_obs_uc_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_uc_ano.R")
#
# usethis::use_r("contar_n_obs_uc.R")
# usethis::use_test("contar_n_obs_uc.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_uc.R")
#
# usethis::use_r("contar_n_obs_sp_uc_estacao_ano.R")
# usethis::use_test("contar_n_obs_sp_uc_estacao_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_sp_uc_estacao_ano.R")
#
# usethis::use_r("contar_n_obs_validadas.R")
# usethis::use_test("contar_n_obs_validadas.R")
# testthat::test_file("tests/testthat/test-contar_n_obs_validadas.R")
#
# usethis::use_r("contar_n_sp.R")
# usethis::use_test("contar_n_sp.R")
# testthat::test_file("tests/testthat/test-contar_n_sp.R")
#
# usethis::use_r("contar_n_uc_ano.R")
# usethis::use_test("contar_n_uc_ano.R")
# testthat::test_file("tests/testthat/test-contar_n_uc_ano.R")
#
# usethis::use_r("filtrar_dados.R")
# usethis::use_test("filtrar_dados.R")
# testthat::test_file("tests/testthat/test-filtrar_dados.R")
#
# usethis::use_r("gera_dados_Distance.R")
# usethis::use_test("gera_dados_Distance.R")
# testthat::test_file("tests/testthat/test-gera_dados_Distance.R")
#
# usethis::use_r("gerar_tabdin.R")
# usethis::use_test("gerar_tabdin.R")
# testthat::test_file("tests/testthat/test-gerar_tabdin.R")
#
# usethis::use_r("monitora_aves_masto_florestal.R")
# usethis::use_test("monitora_aves_masto_florestal.R")
# testthat::test_file("tests/testthat/test-monitora_aves_masto_florestal.R")
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
# usethis::create_github_token()
# usethis::edit_r_environ()
# usethis::use_github()

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
# usethis::use_vignette("distanceMonitoraflorestal")
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
