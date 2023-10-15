# Documentação da função contar_n_uc_ano() --------------------
#' Número de Unidades de Conservação amostradas por ano
#'
#' @description
#' A função \code{contar_n_uc_ano()} gera o número total de Unidades de Conservação amostradas por ano contidos na base de dados fornecida.
#'
#' @usage contar_n_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuraçãoo, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo o número total de Unidades de Conservação amostradas por ano contido na base de dados fornecida.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número total de UCs em cada ano - dados brutos
#' contar_n_uc_ano()
#'
#' # número total de UCs em cada ano - dados filtrados
#' dados_filtrados <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#'   nome_uc == "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_uc_ano(dados_filtrados)
contar_n_uc_ano <- function(dados = monitora_aves_masto_florestal) {

  # gera o número de Ucs amostradas por ano
  n_uc_ano <- dados |>
    dplyr::count(
      ano,
      nome_uc
    ) |>
    dplyr::group_by(
      ano
    ) |>
    dplyr::count(
      nome_uc
    ) |>
    dplyr::summarise(
      n_ucs = sum(n)
    )

  # retorna uma tabela com o número de UCs amostradas por ano
  return(n_uc_ano)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_uc_ano",
    "dados",
    "nome_uc",
    "ano",
    "n_ucs",
    "n"
  )
)
