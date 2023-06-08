# Documentação da função contar_n_uc() --------------------
#' Número de Unidades de Conservação amostradas
#'
#' @description
#' A função \code{contar_n_uc()} gera o número total de Unidades de Conservação amostradas contidas na base de dados fornecida.
#'
#' @usage contar_n_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados brutos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo o número total de Unidades de Conservação contido na base de dados fornecida.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número total de UCs - dados brutos
#' contar_n_uc()
#'
#' # número total de UCs - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_sps = "dasyprocta_corconota"
#' )
#'
#' contar_n_uc(dados_filtrados)
contar_n_uc <- function(dados = monitora_aves_masto_florestal) {

  # número total de Uc's
  n_ucs <- dados |>
    dplyr::distinct(nome_uc) |>
    nrow()

  # retornos n número total de Uc's
  return(n_ucs)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_ucs",
    "dados",
    "nome_uc"
  )
)
