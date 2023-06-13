# testthat::test_that("retorna tibble", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(dplyr::is.tbl(dados_distance_com_repeticao), TRUE)
#
# })
#
# testthat::test_that("retorna 11 colunas", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(ncol(dados_distance_com_repeticao), 11)
# })
#
# testthat::test_that("retorna 1352 linhas", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(nrow(dados_distance_com_repeticao), 1352)
# })
#
# testthat::test_that("retorna 55 linhas", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados sem repeticao
#   dados_distance_sem_repeticao <- transformar_dados_formato_Distance(dados_filtrados, amostras_repetidas = FALSE)
#
#   testthat::expect_equal(nrow(dados_distance_sem_repeticao), 55)
# })
#
# testthat::test_that("retorna 3 fatores", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(dados_distance_com_repeticao |>
#                            dplyr::select(dplyr::where(is.factor)) |>
#                            ncol(),
#                          3
#   )
# })
#
# testthat::test_that("retorna 7 double", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(dados_distance_com_repeticao |>
#                            dplyr::select(dplyr::where(is.double)) |>
#                            ncol(),
#                          7
#   )
# })
#
# testthat::test_that("retorna 1 integer", {
#
#   # gerar dados filtrados
#   dados_filtrados <- filtrar_dados(
#     nome_uc = "resex_tapajos_arapiuns",
#     nome_sps = "dasyprocta_croconota",
#     validacao_obs = "especie"
#   )
#
#   # gerar os dados transformados com repeticao
#   dados_distance_com_repeticao <- transformar_dados_formato_Distance(dados_filtrados)
#
#   testthat::expect_equal(dados_distance_com_repeticao |>
#                            dplyr::select(dplyr::where(is.integer)) |>
#                            ncol(),
#                          1
#   )
# })
