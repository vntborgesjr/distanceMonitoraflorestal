# Documentacao da funcao gerar_tabdin_dados_brutos() --------------------
#' Gera uma tabela dinamica
#'
#' @description
#' A funcao \code{} gera uma tabela dinamica a partir de uma base de dados de entrada
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], [gerar_dados_filtrados()], ou [gerar_dados_selecionados()]
#' @param n_linhas Descricao
#'
#' @details
#' Additional details...
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples \dontrun{gerar_tabdin_dados_brutos()}
gerar_tabdin_dados_brutos <- function(
    dados = readxl::read_excel(
      path = paste0(
        here::here(),
        "/data-raw/monitora_masto_aves_2023_04_04.xlsx"
      ),
      sheet = "dados brutos"
    ),
    n_linhas = 1:1000
) {
  # gerar tabela dinamica dos dados brutos
  dados_brutos <- dados |>
    dplyr::slice(n_linhas) |>
    DT::datatable(filter = list(position = "top"))

  # retornar a tabela dinamica dos dados brutos
  return(dados_brutos)
}

utils::globalVariables(
  c(
    "n_ucs",
    "dados",
    "nome_uc"
  )
)
