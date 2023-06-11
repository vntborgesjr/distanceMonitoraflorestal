
# Documentação da função definir_intervalos_distancia() -------------------
#' Agrupas as distâncias em intervalos de tamanhos homogêneos ou diferentes
#'
#' @description
#' A função `definir_intervalos_distancia()` agrupa as distâncias perpendiculares
#' nas quais os indivíduos foram observados em intervalos regulares (ex. mesmo
#'  tamanho), ou irregulares (ex. tamanhos diferentes).
#'
#' @usage definir_intervalos_distancia(
#'          dados,
#'          intervalos_distancia
#'        )
#'
#' @param dados recebe uma `tibble` gerada pela função
#' [transformar_dados_formato_Distance()].
#' @param intervalos_distancia recebe um vetor numérico contendo os intervalos
#' a partir dos quais as distâncias serão agrupadas.
#'
#' @return Retorna a `tibble` de entrada contendo duas novas colunas, `distbegin`
#' e `distend` que definem o intervalo de agrupamento das distâncias perpendiculares
#'
#' @export
#'
#' @examples
#' # carregar pacote
#' library(dplyr)
#'
#' # gerar dados filtrados
#' dasy_croc_tap_arap_repeticao <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#' transformar_dados_formato_Distance()
#'
#' # definir intervalos de distância (binagem)
#' dasy_croc_tap_arap_repeticao_binados <- dasy_croc_tap_arap_repeticao |>
#'   definir_intervalos_distancia(intervalos_distancia = seq(
#'     from = 0,
#'     to  = 50,
#'     by = 1.5
#'   ))
#'
#' glimpse(dasy_croc_tap_arap_repeticao_binados)
#'
#' # definir intervalos de distância diferentes
#'
#' dasy_croc_tap_arap_repeticao_binados2 <- dasy_croc_tap_arap_repeticao |>
#' definir_intervalos_distancia(
#'     intervalos_distancia = c(0, seq(
#'       from = 1,
#'       to = 55,
#'       by = 1.4
#'     ))
#'   )
#'
#' glimpse(dasy_croc_tap_arap_repeticao_binados2)
definir_intervalos_distancia <- function(
    dados,
    intervalos_distancia
) {

  # definir intervalos de distância
  dados_distance_com_repeticao_binados <- dados |>
    tidyr::drop_na(distance) |>
    Distance::create_bins(
      cutpoints = intervalos_distancia
    )

  # retornar
  return(dados_distance_com_repeticao_binados)

}

utils::globalVariables(
  c(
    "dados_distance_com_repeticao_binados",
    "intervalos_distancia"
  )
)
