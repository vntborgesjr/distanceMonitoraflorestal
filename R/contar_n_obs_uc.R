# Documentação da função contar_n_obs_uc() --------------------
#' Número total de observações em cada Unidade de Conservação
#'
#' @description
#' A função \code{contar_n_obs_uc()} gera uma tabela contendo o número total de observações para cada Unidade de Conservação.
#'
#' @usage contar_n_obs_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela funções [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo três colunas, \code{nome_uc} contendo o nome da Unidade de Conservação, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservação e \code{n} contendo o número de observações realizadas para cada Unidade de Conservação.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada UC - dados brutos
#' contar_n_obs_uc()
#'
#' # número de observações para cada UC - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_uc(dados_filtrados)
contar_n_obs_uc <- function(
    dados = monitora_aves_masto_florestal
) {

  # gerar a tabela com o número de observações por UC
  n_obs_uc <- dados  |>
    dplyr::count(
      nome_uc,
      nome_uc_abv
    )

  # retorna a tabela com o número de observações por UC
  return(n_obs_uc)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_obs_uc",
    "dados",
    "nome_uc",
    "nome_uc_abv"
  )
)
