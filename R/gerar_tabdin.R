# Documentação da função gerar_tabdin() --------------------
#' Gera uma tabela dinâmica
#'
#' @description
#' A função \code{gerar_tabdin()} gera uma tabela dinâmica a partir de uma base de dados de entrada.
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burto de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal, `monitora_aves_mamiferos_florestal`.
#' @param n_linhas fornece o intervalo de linhas a ser gerado. Por configuração, é gerado o intervalo entre as linhas 1 e 1000.
#'
#' @details
#' A velociadade de geração da tabela dinâmica e o tamanho do intervalo de linhas que pode ser gerado vairam de acordo com a configuração da máquina que executa a função.
#'
#' @return Retorna um objeto do tipo \code{}...
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # gerar tabela dinâmica dos dados brutos
#' gerar_tabdin()
#'
#' # gerar tabela dinâmica a partir dos dados filtrados
#' dados_filtrados <- filtrar_dados(
#' nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
#' )
#' gerar_tabdin(dados_filtrados)
gerar_tabdin <- function(
    dados = monitora_aves_masto_florestal,
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
    "monitora_aves_masto_florestal",
    "dados_brutos",
    "dados",
    "n_linhas"
  )
)
