# test_that("retorna uma tibble", {
#   expect_equal(dplyr::is.tbl(contar_n_obs_uc()), TRUE)
# })
#
# test_that("retorna 40 linhas", {
#   expect_equal(nrow(contar_n_obs_uc()), 40)
# })
#
# test_that("retorna 3 colunas", {
#   expect_equal(ncol(contar_n_obs_uc()), 3)
# })
#
# test_that("retorna 1 linhas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(nrow(contar_n_obs_uc(dados_filtrados)), 1)
# })
#
