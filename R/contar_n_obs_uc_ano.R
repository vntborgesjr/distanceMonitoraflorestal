# Documentação da função contar_n_obs_uc_ano() --------------------
#' Número total de observações em cada Unidade de Conservação por ano
#'
#' @description
#' A função \code{contar_n_obs_uc_ano()} gera uma tabela contendo o número total de observações para cada Unidade de Conservação em cada ano.
#'
#' @usage contar_n_obs_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo quatro colunas, \code{nome_uc} contendo o nome da Unidade de Conservação, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservação, \code{ano} que contem o ano da amostragem e \code{n} contendo o número de observações realizadas para cada Unidade de Conservação.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada UC - dados brutos
#' contar_n_obs_uc_ano()
#'
#' # número de observações para cada UC - dados filtrados
#' dados_filtrados <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#'   nome_uc == "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_uc_ano(dados_filtrados)
contar_n_obs_uc_ano <- function(
    dados = monitora_aves_masto_florestal
) {

  # gerar a tabela com o número de observações por UC em cada ano
  n_obs_uc_ano <- dados  |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      ano
    )

  # retorna a tabela com o número de observações por UC por ano
  return(n_obs_uc_ano)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_obs_uc_ano",
    "dados",
    "nome_uc",
    "nome_uc_abv",
    "ano"
  )
)
