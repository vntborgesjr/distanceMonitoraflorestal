# Documentação da função contar_n_obs_sp_uc() --------------------
#' Número total de observações para cada espécie e em cada Unidade de Conservação
#'
#' @description
#' A função \code{contar_n_obs_sp_uc()} gera uma tabela contendo o número total de observações para cada espécie e em cada Unidade de Conservação.
#'
#' @usage contar_n_obs_sp_uc(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo cinco colunas, \code{nome_uc} que contem o nome da Unidade de Conservação, \code{nome_uc_abv} que contem o nome  abreviado da Unidadade de Conservação, \code{nome_sp} contendo o nome da espécie, \code{nome_sp_abv} que contem o nome abreviado da espécie e \code{n} contendo o número de observações realizadas para espécie em cada ano.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada espécie e em cada UC - dados brutos
#' contar_n_obs_sp_uc()
#'
#' # número de observações para cada espécie e em cada UC - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_uc(dados_filtrados)
contar_n_obs_sp_uc <- function(
    dados = monitora_aves_masto_florestal
) {

  # gera o número de observações por espécie
  n_obs_sp_uc <- dados |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      nome_sp,
      nome_sp_abv
    )

  # retorna o número de observações por espécie
  return(n_obs_sp_uc)

}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_obs_sp_uc",
    "dados",
    "nome_sp",
    "nome_sp_abv",
    "nome_uc",
    "nome_uc_abv"
  )
)
