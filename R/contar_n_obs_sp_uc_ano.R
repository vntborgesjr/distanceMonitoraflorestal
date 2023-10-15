# Documentação da função contar_n_obs_sp_uc_ano() --------------------
#' Número total de observações para cada espécie, em cada Unidade de Conservação e em cada ano
#'
#' @description
#' A função \code{contar_n_obs_sp_uc_ano()} gera uma tabela contendo o número total de observações para cada espécie, em cada Unidade de Conservação e em cada ano.
#'
#' @usage contar_n_obs_sp_uc_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo seis colunas, \code{nome_uc} que contem o nome da Unidade de Conservação, \code{nome_uc_abv} que contem o nome abreviado da Unidade de Conservação, \code{nome_sp} contendo o nome da espécie, \code{nome_sp_abv} que contem o nome abreviado da espécie, \code{ano} que contem o ano em que a espécie foi amostrada e \code{n} contendo o número de observações realizadas para espécie em cada ano.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada espécie, em cada UC e por ano - dados brutos
#' contar_n_obs_sp_uc_ano()
#'
#  # número de observações para cada espécie, em cada UC e por ano - dados filtrados
#' dados_filtrados <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#'   nome_uc == "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_uc_ano(dados_filtrados)
contar_n_obs_sp_uc_ano <- function(
    dados = monitora_aves_masto_florestal
) {
  # gera o número de observações por espécie, por Uc e por ano
  n_obs_sp_uc_ano <- dados |>
    dplyr::count(
      nome_uc,
      nome_uc_abv,
      nome_sp,
      nome_sp_abv,
      ano
    )

  # retorna o número de observações por espécie, por Uc e por ano
  return(n_obs_sp_uc_ano)
}

utils::globalVariables(
  c(
    "monitora_aves_masto_florestal",
    "n_obs_sp_uc_ano",
    "dados",
    "ano",
    "nome_sp",
    "nome_sp_abv",
    "nome_uc",
    "nome_uc_abv"
  )
)
