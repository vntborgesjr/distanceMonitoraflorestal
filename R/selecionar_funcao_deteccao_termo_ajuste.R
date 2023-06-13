# Documentacao da funcao selecionar_funcao_deteccao_termo_ajuste() ------------------
#' Seleção de modelos a partir do valor de AIC
#'
#' @description
#' A função `selecionar_funcao_deteccao_termo_ajuste()` gera uma tabela de
#' seleção de modelos para avaliar qual combinação entre função chave e termo
#' de ajuste melhor se ajustam aos dados.Os modelos são ordenados pelo valor de
#' AAIC e, portanto não permitem modelos com diferentes distâncias de truncagem
#' nem diferentes categorização de distâncias.
#'
#' @usage selecionar_funcao_deteccao_termo_ajuste(
#'          ...,
#'          distancia_categorizada = FALSE
#'        )
#'
#' @param ... recebe os objetos que aramazenam os modelos a serem sumarizados
#' @param distancia_categorizada lógico, se `TRUE` informa que as distâncias
#' foram categorizadas. Por configuração, assume que as distâncias são contínuas
#'
#' @return retorna um `data.frame` contendo os modelos ordenados de acordo com o
#' valor de AIC e outras informações adicionais como a presença ou não de
#' covariáveis, p-valor da estatística utilizada (Chi-quadrado, ou Cramer-von
#' Mises).
#'
#' @export
#'
#' @examples
#' # gerar os dados transformados com repetição
#' dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#' transformar_dados_formato_Distance()
#'
#' # ajustar modelos com funções chave diferentes
#' # modelo Half-normal
#' modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     truncamento = 15
#'   )
#'
#' # modelo Hazard-rate
#' modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hr",
#'     truncamento = 15
#'   )
#'
#' # modelo Uniform
#' modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "unif",
#'     truncamento = 15
#'   )
#'
#' # gerar a tabela de seleção com o resumo comparativo dos modelos
#' selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#'   modelo_hn$`Sem termo`,
#'   modelo_hn$Cosseno,
#'   modelo_hn$`Hermite polinomial`,
#'   modelo_hr$`Sem termo`,
#'   modelo_hr$Cosseno,
#'   modelo_hr$`Polinomial simples`,
#'   modelo_unif$Cosseno,
#'   modelo_unif$`Polinomial simples`
#' )
#'
#' selecao_funcao_deteccao_termo_ajuste
selecionar_funcao_deteccao_termo_ajuste <- function(
    ...,
    distancia_categorizada = FALSE
) {

  # gerar rank das funcoes de deteccao e termos de ajuste
  selecao_funcao_deteccao_termo_ajuste <- Distance::summarize_ds_models(
    ...,
    delta_only = FALSE
  )

  # controlar para a presença de dados categorizados
  if (distancia_categorizada == FALSE) {

    # gera a tabela de seleção de modelos para dados de distância contínuos
    tabela_selecao_funcao_deteccao_termo_ajuste <- selecao_funcao_deteccao_termo_ajuste |>
      dplyr::select(!1) |>
      dplyr::rename(
        Modelo = `Key function`,
        `C-vM p-valor` = `C-vM p-value`
      ) |>
      dplyr::distinct()

  } else {

    # gera a tabela de seleção de modelos para dados de distância categorizados
    tabela_selecao_funcao_deteccao_termo_ajuste <- selecao_funcao_deteccao_termo_ajuste |>
      dplyr::select(!1) |>
      dplyr::rename(
        Modelo = `Key function`,
      ) |>
      dplyr::distinct()

  }

  # retorna um data.frame com a selecao das funcoes de deteccao e termos
  # de ajuste
  return(tabela_selecao_funcao_deteccao_termo_ajuste)

}

utils::globalVariables(
  c(
    "...",
    "selecao_funcao_deteccao_termo_ajuste",
    "dados",
    "delta_only",
    "distancia_categorizada",
    "tabela_selecao_funcao_deteccao_termo_ajuste",
    "Modelo",
    "Key function",
    "C-vM p-valor",
    "C-vM p-value"
  )
)
