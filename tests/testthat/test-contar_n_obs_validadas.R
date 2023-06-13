# test_that("retorna uma tibble", {
#   expect_equal(dplyr::is.tbl(contar_n_obs_validadas()), TRUE)
# })
#
# test_that("retorna uma tibble", {
#   expect_equal(is.vector(contar_n_obs_validadas(retorna_tabela = FALSE)), TRUE)
# })
#
# test_that("retorna 5 linhas", {
#   expect_equal(nrow(contar_n_obs_validadas()), 5)
# })
#
# test_that("retorna 2 colunas", {
#   expect_equal(ncol(contar_n_obs_validadas()), 2)
# })
#
# test_that("retorna 5 linhas", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(nrow(contar_n_obs_validadas(dados_filtrados)), 5)
# })
