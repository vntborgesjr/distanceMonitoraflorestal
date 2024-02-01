# Documentacao da funcao plotar_funcao_deteccao_modelos_selecionad --------
#' Plot das curvas ajustadas das funções de detecção e histogramas dos modelos
#' de amostragem por distância
#'
#' @description
#' Plot da(s) função(ões) de detecção com um histograma das distâncias
#' observadas para comparar visualmente o(s) modelo(s) ajustado e os dados.
#'
#' @usage plotar_funcao_deteccao_modelos_selecionados(
#'          dados,
#'          nc,
#'          intervalos_distancia
#'        )
#'
#' @param dados recebe uma lista nomeada gerada pela função
#' [gerar_lista_modelos_selecionados()] contendo os modelos de funções de detecção
#' @param nc número de colunas com o mesmo intervalo de distância
#' @param intervalos_distancia  intervalos de distância definidos pelo usuário
#'
#' @details
#' A estrutura do histograma pode ser controlada pelos argumentos `nc` e
#' `ìntervalos_distancia`.
#'
#' @return Gráficos de linha e histograma
#' @export
#'
#' @examples \dontrun{
#' # gerar os dados transformados com repetição
#' dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#'   nome_uc = "resex_tapajos_arapiuns",
#'   nome_sps = "dasyprocta_croconota",
#'   validacao_obs = "especie"
#' ) |>
#'   transformar_dados_formato_Distance()
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
#' modelos_selecionados <- selecionar_funcao_deteccao_termo_ajuste(
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
#' # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
#' lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#'   modelo_hr$`Sem termo`,
#'   modelo_hn$Cosseno,
#'   modelo_unif$Cosseno,
#'   modelo_unif$`Polinomial simples`,
#'   modelo_hn$`Sem termo`,
#'   nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#' )
#'
#' plotar_funcao_deteccao_modelos_selecionados(lista_modelos_selecionados)
#' }
plotar_funcao_deteccao_modelos_selecionados <- function(
    dados,
    nc = NULL,
    intervalos_distancia = NULL
  ) {

  if (inherits(dados, "list") == TRUE) {
    # plot das curvas ajustadas das funções de detecção e histogramas dos modelos
    # de amostragem por distãncia
    dados |>
      purrr::map(
        \(.x) plot(
          .x,
          nc = nc,
          breaks = intervalos_distancia,
          xlab = "Distância (m)",
          ylab = "Probabilidade de detecção",
          pl.col = "chartreuse4"
        )
      )
  } else {
    # plot das curva ajustada das função de detecção e histogramas do modelo
    # de amostragem por distãncia
    plot(
      dados,
      nc = nc,
      breaks = intervalos_distancia,
      xlab = "Distância (m)",
      ylab = "Probabilidade de detecção",
      pl.col = "chartreuse4"
    )
  }

}

utils::globalVariables(
  c(
    "dados",
    ".x",
    "nc",
    "intervalos_distancia"
  )
)
