# Documentacao da funcao selecionar_funcao_deteccao_termo_ajuste() ------------------
#' Title
#'
#' @description
#' A short description...
#'
#' @usage selecionar_funcao_deteccao_termo_ajuste(
#'          dados,
#'          distancia_categorizada = FALSE
#'        )
#'
#' @param dados breve descrição sobre o argumento
#' @param distancia_categorizada breve descrição sobre o argumento
#'
#' @details
#' Additional details...
#'
#' @return breve descrição sobre o objeto de saída
#' @export
#'
#' @examples
#' # gerar os dados transformados com repetição
#' dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#' transformar_dados_formato_Distance(dados_filtrados)
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
#' # gerar lista contendo todos os modelos ajsutados aos dados
#' lista_modelos_ajustados <- list(
#'   `half-normal` = modelo_hn,
#'   `hazard-rate` = modelo_hr,
#'   `uniforme` = modelo_unif
#' )
#'
#' # gerar a tabela de seleção com o resumo comparativo dos modelos
#' selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#'   lista_modelos_ajustados
#' )
#'
#' selecao_funcao_deteccao_termo_ajuste
selecionar_funcao_deteccao_termo_ajuste <- function(
    dados,
    distancia_categorizada = FALSE
) {

  # gerar rank das funcoes de deteccao e termos de ajuste
  selecao_funcao_deteccao_termo_ajuste <- Distance::summarize_ds_models(
    dados$`half-normal`$`Sem termo`,
    dados$`half-normal`$Cosseno,
    dados$`half-normal`$`Hermite polinomial`,
    dados$`hazard-rate`$`Sem termo`,
    dados$`hazard-rate`$Cosseno,
    dados$`hazard-rate`$`Polinomial simples`,
    dados$uniforme$Cosseno,
    dados$uniforme$`Polinomial simples`,
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
