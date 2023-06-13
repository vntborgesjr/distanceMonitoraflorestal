# test_that("retorna uma tibble", {
#   expect_equal(dplyr::is.tbl(contar_n_obs_sp()), TRUE)
# })
#
# test_that("retorna 196 linhas", {
#   expect_equal(nrow(contar_n_obs_sp()), 196)
# })
#
# test_that("retorna 3 colunas", {
#   expect_equal(ncol(contar_n_obs_sp()), 3)
# })
#
# test_that("retorna 60 linhas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(nrow(contar_n_obs_sp(dados_filtrados)), 60)
# })
#
#
