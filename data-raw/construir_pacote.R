
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

# usethis:use_mit_license()

# Adicionar dataset disponivel ao usuario ---------------------------------
# usethis::use_data_raw("monitora_masto_aves_2023_04_04.xlsx")
#
# Adicionar datasets ao pacote --------------------------------------------
# usethis::use_data(monitora_masto_aves_2023_04_04.xlsx)
#
# Adicionar funcoes ao pacote ---------------------------------------------

# usethis::use_r("minhas_funcoes.R")

# Carergar todas as funcoes -----------------------------------------------

# devtools::load_all()

# Checar o funcionamento do pacote ----------------------------------------
# nessa etapa o arquivo NAMESPACE e recriado
# devtools::check()

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
# usethis::use_test(minhas_funcoes)
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
