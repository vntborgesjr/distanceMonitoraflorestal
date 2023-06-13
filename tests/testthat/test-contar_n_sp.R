# test_that("retorna um vetor", {
#   expect_equal(is.vector(contar_n_sp()), TRUE)
# })
#
# test_that("retorna um vetor de comprimento 1", {
#   expect_equal(length(contar_n_sp()), 1)
# })
#
# test_that("retorna 196 especies", {
#   expect_equal(contar_n_sp(), 196)
# })
#
# test_that("retorna 60 especies", {
#   dados_filtrados <- filtrar_dados(
#     nome_ucs = "resex_tapajos_arapiuns"
#   )
#   expect_equal(contar_n_sp(dados_filtrados), 60)
# })
