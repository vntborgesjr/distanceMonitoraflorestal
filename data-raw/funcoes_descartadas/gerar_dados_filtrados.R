# Documentacao funcao gerar_dados_filtrados() --------------------------

#' Gera dados filtrados a partir das Unidades de Conservacao e das especies
#'
#' @description
#' A funcao \code{gerar_dados_filtrados()} filtra a base de dados brutos a partir das colunas \code{nome_uc} e \code{nome_sp} que correspondem a(s) Unidade(s) de Conservacao e especie(s) desejadas, respectivamente.
#'
#' @usage gerar_dados_filtrados(
#'          dados = monitora_aves_masto_florestal,
#'          nome_ucs = NULL,
#'          nome_sps = NULL
#'        )
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], ou [gerar_dados_selecionados()]. Por configuracao, carrega a base de dados burto de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal
#' @param nome_ucs recebe um vetor do tipo caracter contendo o nome de uma ou mais Unidades de Conservacao.
#' @param nome_sps recebe um vetor do tipo caracter contendo o nome de uma ou mais especies.
#'
#' @details
#' A filtragem da base de dados pode ser feita a partir do nome das Unidades de Conservacao, a partir do nome das especies, ou a partir da combinacao de nomes de diferentes Unidades de Conservacao e diferentes especies. Para maiores detalhes consulte os exemplos.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados do Projeto Monitora Componente Florestal, filtrados a partir da(s) Unidade(s) de Conservacao ou da(s) especie(s).
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples
#' # carregar dados filtrados por uma unidade de conservacao
#' dados_filtrados_uc1 <- gerar_dados_filtrados(
#' nome_ucs = "resex_tapajos_arapiuns"
#' )
#'
#' dplyr::glimpse(dados_filtrados_uc1)
#'
#' # carregar dados filtrados por mais de uma unidade de conservacao
#' dados_filtrados_uc2 <- gerar_dados_filtrados(
#' nome_ucs = c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
#' )
#'
#' dplyr::glimpse(dados_filtrados_uc2)
#'
#' # carregar dados filtrados por uma especie
#' dados_filtrados_sp1 <- gerar_dados_filtrados(
#' nome_sps = "dasyprocta_croconota"
#' )
#'
#' dplyr::glimpse(dados_filtrados_sp1)
#'
#' # carregar dados filtrados por mais de uma especie
#' dados_filtrados_sp2 <- gerar_dados_filtrados(
#' nome_sps = c("dasyprocta_croconota", "dasyprocta_iacki")
#' )
#'
#' dplyr::glimpse(dados_filtrados_sp2)
#'
#' # carregar dados filtrados por unidade de conservacao e especie
#' dados_filtrados_uc_sp <- gerar_dados_filtrados(
#' nome_ucs = "resex_tapajos_arapiuns",
#' nome_sps = c("dasyprocta_croconota")
#' )
#'
#' dplyr::glimpse(dados_filtrados_uc_sp)
#'
gerar_dados_filtrados <- function(
    dados = monitora_aves_masto_florestal,
    nome_ucs = NULL,
    nome_sps = NULL
) {

  # controlar a origem dos dados
  if (dplyr::is.tbl(dados)) {

    # se for uma tibble gerada por outra funcao
    dados <- dados

  } else {

    # se nao for, utilizar o default
    dados <- dados

  }

  # definir possiveis nomes de ucs
  todas_ucs <- dados |>
    dplyr::distinct(nome_uc) |>
    dplyr::pull() |>
    as.character()

  # definir possiveis nomes de sps
  todas_sps <- dados |>
    dplyr::distinct(nome_sp) |>
    dplyr::pull() |>
    as.character()

  # controlar as possibilidades de filtragem
  if (is.null(nome_ucs)) {

    # gerar o tibble filtrado por UC e especie
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_uc %in% todas_ucs,
        nome_sp %in% nome_sps
      )

  } else if (is.null(nome_sps)) {

    # gerar o tibble filtrado por UC e especie
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        nome_sp %in% todas_sps
      )

  } else {
    # gerar o tibble filtrado por UC e especie
    dados_filtrados <- dados |>
      dplyr::filter(
        nome_uc %in% nome_ucs,
        nome_sp %in% nome_sps
      )

  }

  # retornar o tibble com os dados filtrados
  return(dados_filtrados)
}

utils::globalVariables(
  c(
    "dados",
    "monitora_aves_masto_florestal",
    "todas_ucs",
    "todas_sps",
    "nome_ucs",
    "nome_uc",
    "nome_sps",
    "nome_sp"
  )
)

