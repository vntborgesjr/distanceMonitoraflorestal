# Documentação da função comparar_modelo_estratificado() ------------------
#' Compara AIC de modelos não estratificados com modelo estratificado
#'
#' @description
#' A função `comparar_aic_modelo_estratificado()` compara o valor de AIC de um
#' modelo global com estratificação com o valor de AIC de modelos ajustados
#' separadamente para caada uma das regiões.
#'
#' @param ... recebe os objetos que contém os resultados dos modelos *distance sampling*
#'  ajustados aos dados a partir da função [ajustar_modelos_Distance()].
#' @param nome_modelos recebe um vetor do tipo caracter contendo os nomes dos
#' modelos.
#'
#' @return Retorna uma lista contendo uma `tibble` contendo o nome dos modelos, graus
#' de liberdade e valores de AIC e uma lista contendo o valor de AIC do modelo global
#' (estratificado) e a soma dos valores de AIC dos modelos ajustados individualmente
#' para cada região.
#' @export
#'
#' @examples \dontrun{
#' # comparar modelo global (com estratificação) e modelos ajustados para cada
#' # região
#' resultado_comparacao <- comparar_aic_modelo_estratificado(
#' cutias_distance_hn$Cosseno,
#' cutias_tap_distance_hn,
#' cutias_par_distance_hn,
#' cutias_ter_distance_hn,
#' cutias_anf_distance_hn,
#' nome_modelos = c(
#'   "Global",
#'   "Resex Tapajós-Arapiuns",
#'   "Parna Serra do Prado",
#'   "Esec da Terra do Meio",
#'   "Resex do Riozinho do Anfrísio"
#'  )
#' )
#'
#' resultado_comparacao
#' }
comparar_aic_modelo_estratificado <- function(
    ...,
    nome_modelos
) {

  # gera a lista contento o modelo global e os modelos de cada região
  lista <- list(
    ...
  )

  # gera a tabela contendo o nome dos modelos, graus de liberdade e valores de AIC
  tabela_aic_brutos <- lista |>
    purrr::map(
      \(.x) AIC(.x)
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(Modelo = nome_modelos) |>
    dplyr::relocate(Modelo, .before = df) |>
    tibble::tibble()

  # lista com resultados: tabela e comparação entre o AIC global e a soma dos
  # AICs dos modelos globais
  resultado <- list(
    tabela_aics = tabela_aic_brutos,
    comparacao_aics = list(
      AIC_global = tabela_aic_brutos$AIC[1],
      AIC_locais = sum(tabela_aic_brutos$AIC[2:length(tabela_aic_brutos$AIC)])
    )
  )

  # retornar tabela com valores de AIC dos modelos
  return(resultado)

}







