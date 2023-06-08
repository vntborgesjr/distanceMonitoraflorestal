# Documentação da função contar_n_obs_sp_ano() --------------------
#' Número total de observações para cada espécie e em cada ano
#'
#' @description
#' A função \code{contar_n_obs_sp_ano()} gera uma tabela contendo o número total de observações para cada espécie e em cada ano.
#'
#' @usage contar_n_obs_sp_ano(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pela função [filtrar_dados()]. Por configuração, carrega a base de dados burtos de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal `monitora_aves_masto_florestal`.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo quatro colunas, ano que contem o ano em que a espécie foi amostrada, `nome_sp` contendo o nome da espécie, `nome_sp_abv` que contem o nome abreviado da espécie e `n` contendo o número de observações realizadas para espécie em cada ano.
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # número de observações para cada espécie por ano - dados brutos
#' contar_n_obs_sp_ano()
#'
#' # número de observações para cada espécie por ano - dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' contar_n_obs_sp_ano(dados_filtrados)
contar_n_obs_sp_ano <- function(
    dados = monitora_aves_masto_florestal
) {
  # gerar a tabela com o número de observações por UC em cada ano
  n_obs_sp_ano <- dados  |>
    dplyr::count(
      ano,
      nome_sp,
      nome_sp_abv
    )

  # retorna a tabela com o número de observações por UC
  return(n_obs_sp_ano)
}

utils::globalVariables(
  c(
    "n_obs_sp_ano",
    "dados",
    "ano",
    "nome_sp",
    "nome_sp_abv"
  )
)
