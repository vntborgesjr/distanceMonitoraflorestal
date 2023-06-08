# Documentacao da funcao carregar_dados_brutos() -----------------------
#' Carrega os dados brutos do Projeto Monitora Componente Florestal
#'
#' @description
#' A funcao \code{carregar_dados_brutos()} carrega os dados brutos de aves e de mamíferos de médio e grande porte do Projeto Monitora Componente Florestal.
#'
#' @usage carregar_dados_brutos()
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados brutos do Projeto Monitora Componente Florestal a partir do formato .xlsx.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto

#' @export
#'
#' @examples
#' # carregar os dados brutos
#' dados_brutos <- carregar_dados_brutos()
#'
#' # explorar o conteudo dos dados
#' dplyr::glimpse(dados_brutos)
#'
carregar_dados_brutos <- function() {

  # # carregar dados brutos
  # utils::data(
  #   monitora_aves_masto_florestal,
  #   envir = environment()
  # )
  #
  # # retorna os dados butos
  return(monitora_aves_masto_florestal)
}

utils::globalVariables("monitora_aves_masto_florestal")

