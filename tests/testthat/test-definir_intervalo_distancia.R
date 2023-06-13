# test_that("retorna uma tibble", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(dplyr::is.tbl(dados_dasy_croc_tap_arap_com_repeticao), FALSE)
#
# })
#
# test_that("retorna 13 colunas", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(ncol(dados_dasy_croc_tap_arap_com_repeticao), 13)
#
# })
#
# test_that("retorna 1335 linhas", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(nrow(dados_dasy_croc_tap_arap_com_repeticao), 1335)
#
# })
#
# test_that("retorna 9 doubles", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(dados_dasy_croc_tap_arap_com_repeticao |>
#                            dplyr::select(dplyr::where(is.double)) |>
#                            ncol(), 9)
#
# })
#
# test_that("retorna 3 fatores", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(dados_dasy_croc_tap_arap_com_repeticao |>
#                            dplyr::select(dplyr::where(is.factor)) |>
#                            ncol(), 3)
#
# })
#
# test_that("retorna 1 inteiro", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repetição
#   dados_dasy_croc_tap_arap_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   # definir intervalos de distância (binagem)
#   dados_dasy_croc_tap_arap_com_repeticao <- dados_dasy_croc_tap_arap_com_repeticao |>
#     definir_intervalos_distancia(intervalos_distancia = seq(
#       from = 0,
#       to  = max(
#         dados_dasy_croc_tap_arap_com_repeticao$distance,
#         na.rm = TRUE
#       ) + 1,
#       by = 1.5
#     ))
#
#   testthat::expect_equal(dados_dasy_croc_tap_arap_com_repeticao |>
#                            dplyr::select(dplyr::where(is.integer)) |>
#                            ncol(), 1)
#
# })
#
