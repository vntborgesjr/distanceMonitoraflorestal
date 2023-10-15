# Documentação da função contar_n_sp() --------------------
#' Número total de espécies
#'
#' @description
#' A função \code{contar_n_sp()} gera o número total de espécies contidos na base de dados fornecida.
#'
#' @usage contar_n_sp(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{vetor} contendo o número total de espécies contido na base de dados fornecida.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número total de espécies - dados brutos
#' contar_n_sp()
#'
#' # número total de espécies para uma UC - dados filtrados
#' dados_filtrados <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#'   nome_uc == "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_sp(dados_filtrados)
contar_n_sp <- function(dados = monitora_aves_masto_florestal) {

  # número total de espécies
  n_sp <- dados |>
    dplyr::distinct(nome_sp) |>
    nrow()

  # retorna o número total de espécies
  return(n_sp)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_sp",
    "dados",
    "nome_sp"
  )
)
