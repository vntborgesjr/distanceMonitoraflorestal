# Documentacao da funcao gerar_dados_selecionados() --------------------

#' Carrega os dados selecionados a partir das observacoes validadas ao nivel de especie
#'
#' @description
#' A funcao \code{gerar_dados_selecionados()} seleciona as observacoes da base de dados brutos validadas ao nivel de especie.
#'
#' @usage gerar_dados_selecionados(dados = monitora_aves_masto_florestal)
#'
#' @param dados recebe a \code{tibble} gerada pelas funcoes [carregar_dados_brutos()], ou [gerar_dados_filtrados()]. Por configuracao, carrega a base de dados burto de aves e medios e grandes mamiferos do Pojeto Monitora Componente Florestal, selecionando apenas as os observacoes cujos validacoes foram realizadas ao nivel de especie
#'
#' @details
#' A selecao da base de dados e feita a partir das observacoes validadas ao nivel de especie.
#'
#' @return Retorna um objeto do tipo \code{tibble} contendo os dados do Projeto Monitora Componente Florestal, validados ao nivel de especie.
#'
#' @author
#' Vitor N. T. Borges-Junior
#' Luciana A. Fusinatto
#'
#' @export
#'
#' @examples
#' # carregar dados selecionados
#' dados_selecionados <- gerar_dados_selecionados()
#'
#' dplyr::glimpse(dados_selecionados)
#'
gerar_dados_selecionados <- function(dados = monitora_aves_masto_florestal) {

  # controlar a origem dos dados
  if (dplyr::is.tbl(dados)) {

    # se for uma tibble gerada por outra funcao
    dados <- dados

  } else {

    # se nao for, utilizar o default
    dados <- utils::data(
      monitora_aves_masto_florestal,
      envir = environment()
    )

  }

  # gerar o tibble com os dados selecioandos
  dados_selecionados <- dados |>
    dplyr::filter(validacao == "especie")

  # retornar o tibble com os dados selecionados
  return(dados_selecionados)
}

utils::globalVariables(
  c(
    "dados",
    "monitora_aves_masto_florestal",
    "dados_selecionados",
    "validacao"
  )
)

