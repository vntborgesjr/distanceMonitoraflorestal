# Documentacao da funcao selecionar_distancia_truncamento() ------------------
#' Seleção da melhor distância de truncamento dos dados
#'
#' @description
#' Breve descrição da função...
#'
#' @param dados recebe uma `tibble` gerada a partir da função
#' [transformar_dados_formato_Distance()]
#' @param dist_truncamento recebe uma lista contendo as distâncias de truncagem
#' de forma numérica (ex. 20), ou como porcentagem (como um caracter, ex. 25%).
#' Também pode ser alimentada no formato de lista, com os elementos `left` e
#' `right` (ex. list(left = 1, right =20)) se a truncagem a esquerda for
#' necessária. Por configuração, a distância máxima é utilizada como valor de
#' truncagem a direita. Quando os dados estão categorizados, a truncagem a
#' direita é o valor final da última coluna. O valor de truncagem a esquerda
#' é zero, por configuração.
#'
#' @return retorna uma lista contendo...
#' @export
#'
#' @examples \dontrun{
#' # gerar dados filtrados
#' dados_filtrados <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' )
#'
#' # gerar os dados transformados com repeticao
#' dados_distance_com_repeticao <- dados_filtrados |>
#'   transformar_dados_formato_Distance()
#'
#'
#' dados_distance_com_repeticao |>
#'   selecionar_ditancia_truncamento()
#' }
selecionar_distancia_truncamento <- function(
    dados,
    dist_truncamento = list(
      `25%` = "25%",
      `20%` = "20%",
      `15%` = "15%",
      `10%` = "10%",
      `5%` = "5%"
    )
) {

  # ajustar modelos com funcao de deteccao do tipo Half-normal e sem termos
  # de ajuste a diferentes distancias de truncamento
  modelos_hn_diferentes_dist_truncamento <- purrr::map(
    dist_truncamento,
    \(.x) Distance::ds(
      dados,
      key = NULL,
      adjustment = NULL,
      truncation = .x
    )
  )

  # performa a selecao de modelos e gera um data.frame com os resultados
  tabela_selecao_dist_truncamento <- modelos_hn_diferentes_dist_truncamento |>
    purrr::map(
      \(x) Distance::summarize_ds_models(x, delta_only = FALSE)
    ) |>
    purrr::list_rbind() |>
    dplyr::mutate(AIC = round(AIC, 3))

  # gerar uma tabela com os valores da AIC e as distancias de truncamento
  # correspondentes
  aic <- modelos_hn_diferentes_dist_truncamento |>
    purrr::map(\(x) stats::AIC(x)) |>
    purrr::list_rbind() |>
    dplyr::mutate(dist_truncamento = names(dist_truncamento),
                  `AIC` = round(`AIC`, 3))

  # corrigir a coluna com as distancias de truncamento adequadas
  tabela_selecao_dist_truncamento <- tabela_selecao_dist_truncamento |>
    dplyr::left_join(aic, dplyr::join_by(`AIC`)) |>
    dplyr::mutate(Model = dist_truncamento) |>
    dplyr::select(!df:dist_truncamento)

  # retorna um data.frame com a selecao da melhor distancia de truncamento
  return(list(
    modelos = modelos_hn_diferentes_dist_truncamento,
    selecao = tabela_selecao_dist_truncamento
  ))

}

utils::globalVariables(
  c(
    "AIC"
  )
)
