## code to prepare `monitora_masto_aves_2023_04_04.xlsx` dataset goes here
# carregar funcao para gerar os dados brutos
source("data-raw/gerar_dados_completos.R")

# carregar os dados brutos
monitora_aves_masto_florestal <- readxl::read_excel(
  path = "data-raw/monitora_masto_aves_2023_04_04.xlsx",
  sheet = "dados brutos"
) |>
  janitor::clean_names()

# gerar as transformacoes necessarias nos dados brutos
monitora_aves_masto_florestal <- gerar_dados_completos(monitora_aves_masto_florestal)

# gerar o arquivo monitora_aves_masto_florestal.rda na pasta data
usethis::use_data(
  monitora_aves_masto_florestal,
  overwrite = TRUE
)
