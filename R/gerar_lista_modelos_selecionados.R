# Documentação da função gerar_lista_modelos_selecionados() ---------------
#' Lista nomeada das funções selecionadas
#'
#' @description
#' A função `gerar_lista_modelos_selecionados()` gera uma lista contendo os
#' modelos selecionados e seus respectivos nomes.
#'
#' @param ... recebe os objetos contendo os modelos ajustados aos dados
#' @param nome_modelos_selecionados recebe a `tibble` gerada pela função
#' [selecionar_funcao_deteccao_termo_ajuste()]
#'
#' @return retorna uma `tibble` contendo os nomes dos modelos, os valores de
#' W e de p do teste de Von Cramer.
#' @export
#'
#' @examples
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
#' lista_modelos_selecionados
gerar_lista_modelos_selecionados <- function(
    ...,
    nome_modelos_selecionados
) {

  # gerar uma lista com os modelos selecionados ordenados do melhor para o pior modelo
  lista_modelos_selecionados <- list(
    ...
  )

  # atribuir nome à lista
  names(lista_modelos_selecionados) <- nome_modelos_selecionados$Modelo

  # retornar a lista nomeada contendo os modelos selecionados
  return(lista_modelos_selecionados)

}

