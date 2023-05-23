
# Exemplo funcao carregar_dados_brutos_xlsx() -----------------------------
carregar_dados_brutos_xlsx <- function(
    dados = readxl::read_excel(
      path = paste0(
        here::here(),
        "/inst/extdata/monitora_masto_aves_2023_04_04.xlsx"
      ),
      sheet = "dados brutos"
    )
) {
  # grava uma versao dados_brutos.rds no diretorio inst/extdata
  readr::write_rds(
    dados,
    file = paste0(
      here::here(),
      "/inst/extdata/dados_brutos.rds"
    )
  )

  # retorna os dados butos
  return(dados)
}

#dados_brutos <- carregar_dados_brutos_xlsx(caminho = "/home/usuario/Documentos/Vitor/Piper3D/WWF/Monitora/Dados/Monitora/Monitora/piper3d_monitora_florestal/data-raw/dados-brutos.xlsx")
#dplyr::glimpse(dados_brutos)


