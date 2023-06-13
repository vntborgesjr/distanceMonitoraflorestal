# Documentação da função ajustar_modelo_Distance() -----------------------
#' Ajusta modelos de detecção com diferentes funções chave e termos de ajuste
#'
#' @description
#' A função `ajustar_modelos_Distance()` ajusta aos dados modelos com diferentes
#' funções chave e termos de ajuste.
#'
#' @usage ajustar_modelos_Distance(
#'          dados,
#'          funcao_chave = c("hn", "hr", "unif"),
#'          termos_ajuste = NULL,
#'          truncamento = NULL,
#'          formula = ~ 1
#'        )
#'
#' @param dados recebe a \code{tibble} gerada pela função [transformar_dados_formato_Distance()].
#' @param funcao_chave recebe uma dentre três funções utilizadas para modelar a
#' detectabilidade da espécie: "hn" - Half-normal; "hr" - Hazard-rate; e
#' "unif" - Uniform
#' @param termos_ajuste recebe um dentre n termos de ajuste: `"cos"` - cosseno;
#' `"herm"` - Hermet polynomial; e `"poly"` - Polinomial simples. Quando `NULL`
#' diferentes modelos são ajustados para função chave escolhida utilizando
#' diferentes termos de ajuste. Veja Details.
#' @param truncamento breve descrição do argumento
#' @param formula breve descrição do argumento
#'
#' @details
#' Quando `termo_ajuste = NULL` modelos com diferente termos de ajuste serão
#' ajustados aos dados, a depender da função chave utilizada. Para
#' `funcao_chave = "hn"`, são utilizados os termos de ajuste `NULL` (sem termos),
#'  `"cos"` e `"herm"`; para `funcao_chave = "hr"`, são utilizados os termos de
#'  ajuste `NULL` (sem termos), `"cos"` e `"poly"`; e para `funcao_chave  = "unif"`,
#'  são utilizados so termos de ajuste `"cos"` e `"poly"`
#'
#' @return Retorna uma lista contendo a estimativa de abundância para região
#' estudada
#'
#' @export
#'
#' @examples \dontrun{
#' # gerar dados filtrados para a uma espécie e uma UC e transformar para o
#' # formato para a análise no pacote Distance
#' dados_dasy_croc_tap_arap <- filtrar_dados(
#'   dados = monitora_aves_masto_florestal,
#'   nome_ucs = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#'   transformar_dados_formato_Distance(amostras_repetidas = FALSE)
#'
#' # ajustar modelo com função chave Half-normal e todas os termos de ajuste
#' # possíveis
#' dasyp_croc_resex_tap_arap_hn <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     termos_ajuste = NULL,
#'     truncamento = "10%"
#'   )
#'
#' dasyp_croc_resex_tap_arap_hn
#'
#' # ajustando a função de detecção half-normal, sem termo de ajuste para um
#' # truncamento de 10% dos dados e uma covariável
#' dasy_croc_tap_arap_hn_cov1 <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     termos_ajuste = NULL,
#'     truncamento = "10%",
#'     formula = ~ size
#'   )
#'
#' dasy_croc_tap_arap_hn_cov1
#'
#' # ajustando a função de detecção half-normal, sem termo de ajuste para um
#' # truncamento de 10% dos dados e duas covariáveis
#' dasy_croc_tap_arap_hn_cov2 <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     termos_ajuste = NULL,
#'     truncamento = "10%",
#'     formula = ~ size + cense_time
#'   )
#'
#' dasy_croc_tap_arap_hn_cov2
#' }
ajustar_modelos_Distance <- function(
    dados,
    funcao_chave = c("hn", "hr", "unif"),
    termos_ajuste = NULL,
    truncamento = NULL,
    formula = ~ 1
) {

  # definir a função chave
  funcao_chave <- match.arg(funcao_chave)

  # contorlar a escolha da função chave e dos  termos de ajuste
  if (formula != ~ 1) {

    # definir o termo de ajuste como nulo
    termos_ajuste <- NULL

    # ajusta o modelo de detecção com a função chave definida pelo usuário
    modelos_ajustados <- Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = funcao_chave,
      adjustment = termos_ajuste
    )

  } else  if (is.null(termos_ajuste) == FALSE) {

    # definir termos de ajuste
    termos_ajuste <- termos_ajuste

    # ajusta o modelo de detecção
    modelos_ajustados <- Distance::ds(
      data = dados,
      truncation = truncamento,
      formula = formula,
      key = funcao_chave,
      adjustment = termos_ajuste
    )

  } else if (funcao_chave == "hn") {

    # definir termos de ajuste para função chave Half-normal
    termos_ajuste <- list(
      `Sem termo` = NULL,
      Cosseno = "cos",
      `Hermite polinomial` = "herm"
    )

    # ajusta o modelo de detecção com a função chave Half-normal
    modelos_ajustados <- purrr::map(
      termos_ajuste,
      \(.x) Distance::ds(
        data = dados,
        truncation = truncamento,
        formula = formula,
        key = "hn",
        adjustment = .x
      )
    )

  } else if (funcao_chave == "hr") {

    # definir termos de ajuste para função chave Hazard-rate
    termos_ajuste <- list(
      `Sem termo` = NULL,
      Cosseno = "cos",
      `Polinomial simples` = "poly"
    )

    # ajusta o modelo de detecção com a função chave Hazard-rate
    modelos_ajustados <- purrr::map(
      termos_ajuste,
      \(.x) Distance::ds(
        data = dados,
        truncation = truncamento,
        formula = formula,
        key = "hr",
        adjustment = .x
      )
    )

  } else if (funcao_chave == "unif") {

    # definir termos de ajuste para função chave Uniform
    termos_ajuste <- list(
      Cosseno = "cos",
      `Polinomial simples` = "poly"
    )

    # ajusta o modelo de detecção com a função chave Uniform
    modelos_ajustados <- purrr::map(
      termos_ajuste,
      \(.x) Distance::ds(
        data = dados,
        truncation = truncamento,
        formula = formula,
        key = "unif",
        adjustment = .x
      )
    )

  }

  # retorna o output dos modelos
  return(modelos_ajustados)

}

utils::globalVariables(
  c(
    "funcao_chave",
    "termos_ajuste",
    "data",
    "truncation",
    "formula",
    "key",
    "adjustment",
    "Sem termo",
    "Cosseno",
    "Hermite polinomial",
    "Polinomial simples",
    "modelos_ajustados"
  )
)
