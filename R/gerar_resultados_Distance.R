# Documentacao da funcao gerar_resultados_Distance() --------
#' Resultados sobre a área, abundância e densidade estimada, detecção e
#' coeficiente de variação
#'
#' @param dados recebe uma lista nomeada gerada pela função
#' [gerar_lista_modelos_selecionados()] contendo os modelos de função de
#' detecção
#' @param resultado_selecao_modelos recebe a tibble gerada pela função
#' [selecionar_funcao_deteccao_termo_ajuste()]
#' @param tipo_de_resultado caracter que define o tipo do resultado desejado
#' pelo usuário
#' @param estratificacao lógico, indica se os modelos informados contém ou não
#' estratificação
#'
#' @description
#' Descrever o que cada opção retorna
#'
#' @return Retorna uma `tibble`.
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
#' dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#'
#' # modelo Half-normal
#' modelo_hn <- dados_distance_com_repeticao |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hn",
#'     truncamento = 15
#'   )
#'
#' # modelo Hazard-rate
#' modelo_hr <- dados_distance_com_repeticao |>
#'   ajustar_modelos_Distance(
#'     funcao_chave = "hr",
#'     truncamento = 15
#'   )
#'
#' # modelo Uniform
#' modelo_unif <- dados_distance_com_repeticao |>
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
#' # gerar uma lista nomeada com os modelos selecionados ordenados do melhor para o pior modelo
#' lista_modelos_selecionados <- gerar_lista_modelos_selecionados(
#'   modelo_hr$`Sem termo`,
#'   modelo_hn$Cosseno,
#'   modelo_unif$Cosseno,
#'   modelo_unif$`Polinomial simples`,
#'   modelo_hn$`Sem termo`,
#'   nome_modelos_selecionados = selecao_funcao_deteccao_termo_ajuste
#' )
#'
#' resultados_distance_area_estudo <-gerar_resultados_Distance(
#'   dados = lista_modelos_selecionados,
#'   resultado_selecao_modelos = selecao_funcao_deteccao_termo_ajuste,
#'   tipo_de_resultado = "area_estudo"
#'   )
#'
#' resultados_distance_area_estudo
#'
#' resultados_distance_abundancia <-gerar_resultados_Distance(
#'   dados = lista_modelos_selecionados,
#'   resultado_selecao_modelos = selecao_funcao_deteccao_termo_ajuste,
#'   tipo_de_resultado = "abundancia"
#' )
#'
#' resultados_distance_abundancia
#'
#' resultados_distance_densidade <-gerar_resultados_Distance(
#'   dados = lista_modelos_selecionados,
#'   resultado_selecao_modelos = selecao_funcao_deteccao_termo_ajuste,
#'   tipo_de_resultado = "densidade"
#' )
#'
#' resultados_distance_densidade
#' }
gerar_resultados_Distance <- function(
    dados,
    resultado_selecao_modelos,
    tipo_de_resultado = c("area_estudo", "abundancia", "densidade"),
    estratificacao = FALSE
) {

  # definir o tipo de resultado fornecido pelo usuário
  tipo_de_resultado <- match.arg(tipo_de_resultado)

  if (estratificacao == TRUE) {

    # controlar o tipo de resultado selecionado pelo usuário para modelos com
    # estratificacao
    if(tipo_de_resultado == "area_estudo") {

      # gerar resultados sobre a área total, densidade estimada e coeficiente
      # de variação para modelos estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$summary[1:9]
        ) |>
        purrr::list_rbind() |>
        dplyr::mutate(
          Modelo = rep(
            resultado_selecao_modelos$Model,
            each = length(resultado_selecao_modelos$Model)
          )
        ) |>
        dplyr::relocate(Modelo, .before = Region) |>
        dplyr::rename(
          Região = Region,
          `Area coberta` = CoveredArea,
          Esforco = Effort,
          `Taxa de encontro` = ER,
          `ep da Taxa de encontro` = se.ER,
          `cv. da Taxa de encontro` = cv.ER
        )

    } else if (tipo_de_resultado == "abundancia") {

      # gerar resultados sobre abundância estimada e detecção para modelos
      # estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$Nhat.by.sample[1:8]
        ) |>
        purrr::list_rbind() |>
        dplyr::select(!c(Label, CoveredArea)) |>
        dplyr::rename(
          Região = Region.Label,
          `Estação amostral` = Sample.Label,
          Esforço = Effort.x,
          `Abundância estimada` = Nhat,
          `N de detecções` = n
        )

      resultados_Distance <- resultados_Distance |>
        dplyr::mutate(
          Modelo = rep(
            resultado_selecao_modelos$Model,
            each = length(
              resultados_Distance$`Estação amostral`[1:18]
            )
          )
        ) |> # pode ser um argumento da função
        dplyr::relocate(Modelo, .before = Região)

    } else {

      # gerar reesultados sobre densidade estimada e detecção para modelos
      # estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$D
        ) |>
        purrr::list_rbind() |>
        dplyr::mutate(Modelo = rep(
          resultado_selecao_modelos$Model,
          each = length(resultado_selecao_modelos$Model)
        )
        ) |> # pode ser um argumento da função
        dplyr::relocate(Modelo, .before = Label) |>
        dplyr::rename(
          Rotulo = Label,
          `Estimativa de densidade` = Estimate,
          `Erro padrão` = se,
          `Coeficiente de variação` = cv,
          `Intervalo de confiança inferior` = lcl,
          `Intervalo de confiança superior` = ucl,
          `Graus de liberdade` = df
        )

    }

  } else {

    if (tipo_de_resultado == "area_estudo") {

      # gerar resultados sobre a área total, densidade estimada e coeficiente
      # de variação para modelos não estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$summary[1:9]
        ) |>
        purrr::list_rbind() |>
        dplyr::mutate(Modelo = resultado_selecao_modelos$Model) |>
        dplyr::relocate(Modelo, .before = Region) |>
        dplyr::rename(
          Região = Region,
          `Area coberta` = CoveredArea,
          Esforco = Effort,
          `Taxa de encontro` = ER,
          `ep da Taxa de encontro` = se.ER,
          `cv. da Taxa de encontro` = cv.ER
        )

    } else if (tipo_de_resultado == "abundancia") {

      # gerar resultados sobre abundância estimada e detecção para modelos não
      # estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$Nhat.by.sample[1:8]
        ) |>
        purrr::list_rbind() |>
        dplyr::select(!c(Label, CoveredArea)) |>
        dplyr::rename(
          Região = Region.Label,
          `Estação amostral` = Sample.Label,
          Esforço = Effort.x,
          `Abundância estimada` = Nhat,
          `N de detecções` = n
        )

      resultados_Distance <- resultados_Distance |>
        dplyr::mutate(
          Modelo = rep(
            resultado_selecao_modelos$Model,
            each = length(
              unique(resultados_Distance$`Estação amostral`)
            )
          )
        ) |>
        dplyr::relocate(Modelo, .before = Região)

    } else {

      # gerar resultados sobre densidade estimada para modelos não estratificados
      resultados_Distance <- dados |>
        purrr::map(
          \(.x) .x$dht$individuals$D
        ) |>
        purrr::list_rbind() |>
        dplyr::mutate(Modelo = resultado_selecao_modelos$Model) |> # pode ser um argumento da função
        dplyr::relocate(Modelo, .before = Label) |>
        dplyr::rename(
          Rotulo = Label,
          `Estimativa de densidade` = Estimate,
          `Erro padrão` = se,
          `Coeficiente de variação` = cv,
          `Intervalo de confiança inferior` = lcl,
          `Intervalo de confiança superior` = ucl,
          `Graus de liberdade` = df
        )

    }

  }


  # retorna o resultado selecionado pelo usuário
  return(resultados_Distance)
}

utils::globalVariables(
  c(
    "tipo_de_resultado",
    "resultados_Distance",
    "dados",
    ".x",
    "Modelo",
    "Region",
    "Região",
    "Área",
    "CoveredArea",
    "Esforço",
    "Effort",
    "Taxa de encontro",
    "ER",
    "ep da Taxa de encontro",
    "se.ER",
    "cv. da Taxa de encontro",
    "cv.ER",
    "Label",
    "Region.Label",
    "Estação amostral",
    "Sample.Label",
    "Effort.x",
    "Abundância estimada",
    "Nhat",
    "N de detecções",
    "n",
    "Rotulo",
    "Estimativa de densidade",
    "Estimate",
    "Erro padrão",
    "se",
    "Coeficiente de variação",
    "cv",
    "Intervalo de confiança inferior",
    "lcl",
    "Intervalo de confiança superior",
    "ucl",
    "Graus de liberdade",
    "df"
  )
)
