# Documentação da função contar_n_ano_uc() --------------------

#' Número total de anos em que cada Unidade de Consevarção foi amostrada
#'
#' @description
#' A função \code{contar_n_ano_uc()} gera uma tabela contendo o número total de anos em que cada Unidade de Conservação foi amostrada.
#'
#' @usage contar_n_ano_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_floresta`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo três colunas, \code{nome_uc} que contem os nomes completos das Unidades de Conservação, \code{nome_uc_abv} que contem os nomes abreviados das Unidades de Conservação e \code{n_anos} que contem o número de anos em que cada Unidade de Conservação foi amostrada.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de anos em que uma UC foi amostrada - dados brutos
#' contar_n_ano_uc()
#'
#' # número de anos em que uma UC foi amostrada - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'  nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_ano_uc(dados_filtrados)
contar_n_ano_uc <- function(
    dados = monitora_aves_masto_florestal
) {
  # gera o numero de anos em que cada UC foi amostrada
  n_ano_uc <- dados |>
    dplyr::count(
      ano,
      nome_uc,
      nome_uc_abv
    ) |>
    dplyr::group_by(
      nome_uc,
      nome_uc_abv
    ) |>
    dplyr::count(
      ano
    ) |>
    dplyr::summarise(
      n_anos = sum(n)
    )

  # retorna o numero de UCs por ano
  return(n_ano_uc)
}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_ano_uc",
    "dados",
    "ano",
    "nome_uc",
    "nome_uc_abv",
    "n_anos",
    "n"
  )
)

