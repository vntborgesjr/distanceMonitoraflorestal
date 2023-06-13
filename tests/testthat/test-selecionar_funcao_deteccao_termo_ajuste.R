# test_that("retorna um data.frame", {
#
#   # gerar os dados transformados com repetição
#   dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#     transformar_dados_formato_Distance()
#
#   # ajustar modelos com funções chave diferentes
#   # modelo Half-normal
#   modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   expect_equal(class(selecao_funcao_deteccao_termo_ajuste), "data.frame")
# })
#
#
# test_that("retorna 2 colunas", {
#
#   # gerar os dados transformados com repetição
#   dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#     transformar_dados_formato_Distance()
#
#   # ajustar modelos com funções chave diferentes
#   # modelo Half-normal
#   modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   expect_equal(selecao_funcao_deteccao_termo_ajuste |>
#                  dplyr::select(where(is.character)) |>
#                  ncol(),
#                2)
# })
#
# test_that("retorna 7 colunas", {
#
#   # gerar os dados transformados com repetição
#   dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#     transformar_dados_formato_Distance()
#
#   # ajustar modelos com funções chave diferentes
#   # modelo Half-normal
#   modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#    modelo_hn$`Sem termo`,
#    modelo_hn$Cosseno,
#    modelo_hn$`Hermite polinomial`,
#    modelo_hr$`Sem termo`,
#    modelo_hr$Cosseno,
#    modelo_hr$`Polinomial simples`,
#    modelo_unif$Cosseno,
#    modelo_unif$`Polinomial simples`
#  )
#
#   expect_equal(selecao_funcao_deteccao_termo_ajuste |>
#                  ncol(),
#                7)
# })
#
# test_that("retorna 2 colunas caracter", {
#
#   # gerar os dados transformados com repetição
#   dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#   transformar_dados_formato_Distance()
#
#   # ajustar modelos com funções chave diferentes
#   # modelo Half-normal
#   modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   expect_equal(selecao_funcao_deteccao_termo_ajuste |>
#                  dplyr::select(where(is.character)) |>
#                  ncol(),
#                2)
# })
#
# test_that("retorna 5 colunas numéricas", {
#
#   # gerar os dados transformados com repetição
#   dasy_croc_tap_arap_com_repeticao <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   ) |>
#   transformar_dados_formato_Distance()
#
#   # ajustar modelos com funções chave diferentes
#   # modelo Half-normal
#   modelo_hn <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hn",
#       truncamento = 15
#     )
#
#   # modelo Hazard-rate
#   modelo_hr <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "hr",
#       truncamento = 15
#     )
#
#   # modelo Uniform
#   modelo_unif <- dasy_croc_tap_arap_com_repeticao |>
#     ajustar_modelos_Distance(
#       funcao_chave = "unif",
#       truncamento = 15
#     )
#
#   # gerar a tabela de seleção com o resumo comparativo dos modelos
#   selecao_funcao_deteccao_termo_ajuste <- selecionar_funcao_deteccao_termo_ajuste(
#     modelo_hn$`Sem termo`,
#     modelo_hn$Cosseno,
#     modelo_hn$`Hermite polinomial`,
#     modelo_hr$`Sem termo`,
#     modelo_hr$Cosseno,
#     modelo_hr$`Polinomial simples`,
#     modelo_unif$Cosseno,
#     modelo_unif$`Polinomial simples`
#   )
#
#   expect_equal(selecao_funcao_deteccao_termo_ajuste |>
#                  dplyr::select(where(is.double)) |>
#                  ncol(),
#                5)
# })
#
