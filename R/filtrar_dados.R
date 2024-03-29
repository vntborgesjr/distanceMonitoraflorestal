# Documentacao funcao filtrar_dados() --------------------------
#' Filtra os dados a partir do nível taxonomico de validação e demais colunas
#'
#' @description
#' A função \code{filtrar_dados()} filtra a base de dados brutos a partir da coluna \code{validacao} e colunas desejadas.
#'
#' @usage filtrar_dados(
#'          dados = monitora_aves_masto_florestal,
#'          ...,
#'          validacao_obs = c("ordem", "familia", "genero", "especie", "na")
#'        )
#'
#' @param dados recebe uma \code{tibble} que contenha as colunas `nome_uc`, `nome_sp` e `validacao`. Por configuração, carrega a base de dados burto de aves e médios e grandes mamíferos do Pojeto Monitora Componente Florestal, `monitora_aves_mamiferos_florestal`.
#' @param ... recebe as colunas a sere filtradas ex. \code{nome_uc}, \code{nome_sp}, \code{nome_genero}.
#' @param validacao_obs recebe um vetor do tipo caracter contendo o nome de um ou mais níveis taxonômicos de validação.
#'
#' @details
#' A filtragem da base de dados pode ser feita a partir do nome das Unidades de Conservação, a partir do nome das espécies, a partir do nível taxonômico de validação, ou a partir de suas combinações. Para maiores detalhes consulte os exemplos.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados do Projeto Monitora Componente Florestal, filtrados a partir da(s) Unidade(s) de Conservação, da(s) espécie(s), ou dos nível(is) taxonômico(s) de validação.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#' @export
#'
#' @examples
#' # carregar pacote
#' library(dplyr)
#'
#' # carregar dados filtrados por uma Unidade de Conservacão
#' dados_filtrados_uc1 <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_uc == "resex_tapajos_arapiuns"
#' )
#'
#' glimpse(dados_filtrados_uc1)
#'
#' # carregar dados filtrados por mais de uma Unidade de Conservação
#' dados_filtrados_uc2 <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_uc %in% c("resex_tapajos_arapiuns", "resex_barreiro_das_antas")
#' )
#'
#' glimpse(dados_filtrados_uc2)
#'
#' # carregar dados filtrados por mais de uma Unidade de Conservação e níveis taxonômicos de validação
#' dados_filtrados_uc_validacao1 <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_uc == "resex_tapajos_arapiuns",
#' validacao_obs = c("genero", "especie")
#' )
#'
#' glimpse(dados_filtrados_uc_validacao1)
#'
#' # carregar dados filtrados por uma espécie
#' dados_filtrados_sp1 <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_sp == "dasyprocta_croconota"
#' )
#'
#' glimpse(dados_filtrados_sp1)
#'
#' # carregar dados filtrados por mais de uma especie
#' dados_filtrados_sp2 <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_sp %in% c("dasyprocta_croconota", "dasyprocta_iacki")
#' )
#'
#' glimpse(dados_filtrados_sp2)
#'
#' # carregar dados filtrados por unidade de conservacao e especie
#' dados_filtrados_uc_sp <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#' nome_uc == "resex_tapajos_arapiuns",
#' nome_sp == c("dasyprocta_croconota")
#' )
#'
#' glimpse(dados_filtrados_uc_sp)
#'
#' # carregar dados filtrados por Unidades de Conservação, espécie e nível taxonômico de validação
#' dados_filtrados_uc_sp_validacao <- filtrar_dados(
#' dados = monitora_aves_masto_florestal,
#'   nome_uc %in% c("resex_tapajos_arapiuns", "resex_barreiro_das_antas"),
#'   nome_sp %in% c("dasyprocta_croconota", "guerlinguetus_aestuans"),
#'   validacao_obs = "especie"
#' )
#'
#' glimpse(dados_filtrados_uc_sp_validacao)
filtrar_dados <- function(
    dados = monitora_aves_masto_florestal,
    ...,
    validacao_obs = c("ordem", "familia", "genero", "especie", "na")
) {

  # gerar o tibble filtrado pelas colunas desejadas e nivel taxonomico de validacao
  dados_filtrados <- dados |>
    dplyr::filter(
      ...,
      validacao %in% validacao_obs
    )

  # gerar nomes das ucs e sps filtradas
  nome_uc1 <- unique(as.character(dados_filtrados$nome_uc))

  # gerar o tibble filtrado pelas uc, nivel taxonomico de validacao
  dados_filtrados_uc <- dados |>
    dplyr::filter(
      nome_uc %in% nome_uc1
    )

  # incluir data amostradas e sem observação
  dados_filtrados <- dados_filtrados_uc |>
    # seleciona combinacoes unicas de nome_uc, nome_ea, data_amostragem,
    # ano, estacao, esforco_dia
    dplyr::distinct(
      nome_uc,
      nome_ea,
      data_amostragem,
      ano,
      estacao,
      esforco_dia
    ) |>
    dplyr::left_join(
      y = dados_filtrados,
      by = dplyr::join_by(
        nome_uc,
        nome_ea,
        data_amostragem,
        ano,
        estacao,
        esforco_dia
      )
    )

  # gerar o tibble filtrado pelas colunas desejadas e nivel taxonomico de validacao
  dados_filtrados <- dados |>
    dplyr::filter(
      ...
    )

  # retornar dados filtrados
  return(dados_filtrados)

}

utils::globalVariables(
  c(
    "dados_filtrados",
    "nome_uc",
    "nome_sp",
    "nome_ucs",
    "nome_sps",
    "validacao",
    "validacao_obs"
  )
)
