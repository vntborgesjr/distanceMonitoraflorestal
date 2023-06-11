# Documentacao da funcao testar_bondade_ajuste() --------------------------
#' Title
#'
#' @description
#' A short description...
#'
#' @param dados breve descrição sobre o argumento
#' @param plot breve descrição sobre o argumento
#' @param chisq breve descrição sobre o argumento
#' @param nc breve descrição sobre o argumento
#' @param intervalos_distancia breve descrição sobre o argumento
#'
#' @details
#' Additional details...
#'
#' @return breve descrição sobre o objeto de saída
#' @export
#'
#' @examples
# gerar dados filtrados para a uma espécie e uma UC e transformar para o
#' # formato para a análise no pacote Distance
#' dados_dasy_croc_tap_arap <- filtrar_dados(
#'   dados = monitora_aves_masto_florestal,
#'   nome_ucs = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#'   transformar_dados_formato_Distance(amostras_repetidas = FALSE)
#'
#' # ajustar modelos com funções chave diferentes
#' # modelo Half-normal
#' modelo_hn <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     truncamento = 15
#'   )
#'
#' # modelo Hazard-rate
#' modelo_hr <- dados_dasy_croc_tap_arap |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hr",
#'     truncamento = 15
#'   )
#'
#' # modelo Uniform
#' modelo_unif <- dados_dasy_croc_tap_arap |>
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
#' # teste de bondade de ajuste dos modelos e Q-Q plots
#' bondade_ajuste <- testar_bondade_ajuste(
#'   modelos_selecionados,
#'   plot = TRUE,
#'   chisq = TRUE,
#'   intervalos_distancia =  NULL
#' )
#'
#' bondade_ajuste
testar_bondade_ajuste <- function(
    dados,
    plot = FALSE,
    chisq = FALSE,
    nc = NULL,
    intervalos_distancia = NULL
) {

  # controlar para a presença de dados binados
  if (is.null(intervalos_distancia)) {

    # teste de bondade de ajuste com dados de distância contínuos
    # gera uma lista com os resultados dos testes de bondade de ajuste para dados
    # não categorizados
    bondade_ajuste <- dados |>
      purrr::map(
        \(x) Distance::gof_ds(
          x,
          plot = plot,
          chisq = chisq,
          nc = nc,
          breaks = intervalos_distancia
        )
      ) |>
      # gerar o data.frame com os resultados dos testes de bondade de ajuste
      purrr::map(
        \(x) data.frame(x$dsgof$CvM)
      ) |>
      purrr::list_rbind() |>
      dplyr::mutate(Modelo = names(dados)) |>
      dplyr::relocate(Modelo, .before = W)

  } else{

    # teste de bondade de ajuste com dados de distância categorizados
    # gera uma lista com os resultados dos testes de bondade de ajuste para dados
    # categorizados
    bondade_ajuste <- dados |>
      purrr::map(
        \(x) Distance::gof_ds(
          x,
          plot = plot,
          chisq = chisq,
          nc = nc,
          breaks = intervalos_distancia
        )
      )
    # atribuir nomes aos itens da lista com os resultados dos testes de bondade de ajuste
    names(bondade_ajuste) <- names(dados)

  }
  # retornar o data.frame com o resultado dos testes de bondade de ajuste
  return(bondade_ajuste)
}

utils::globalVariables(
  c(
    "intervalos_distancia",
    "bondade_ajuste",
    "dados",
    "x",
    "plot",
    "chisq",
    "nc",
    "breaks",
    "intervalos_distancia",
    "W"
  )
)
