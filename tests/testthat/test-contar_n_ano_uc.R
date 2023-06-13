# test_that("retornar tibble", {
#   expect_equal(dplyr::is.tbl(contar_n_ano_uc()), TRUE)
# })
#
# test_that("retornar 40 linhas", {
#   expect_equal(nrow(contar_n_ano_uc()), 40)
# })
#
# test_that("retornar 3 colunas", {
#   expect_equal(ncol(contar_n_ano_uc()), 3)
# })
#
# test_that("retornar 1 linhas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(nrow(contar_n_ano_uc(dados_filtrados)), 1)
# })
#
# test_that("retornar 3 colunas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos-arapiuns",
#     nome_sps = "dasyprocta_croconota"
#   )
#   expect_equal(ncol(contar_n_ano_uc(dados_filtrados)), 3)
# })
#
