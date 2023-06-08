test_that("retorna uma tibble", {
  expect_equal(dplyr::is.tbl(contar_n_obs_sp_uc()), TRUE)
})

test_that("retorna 1079 linhas", {
  expect_equal(nrow(contar_n_obs_sp_uc()), 1079)
})

test_that("retorna 5 colunas", {
  expect_equal(ncol(contar_n_obs_sp_uc()), 5)
})

test_that("retorna 60 linhas", {
  dados_filtrados <- filtrar_dados(
    nome_ucs = "resex_tapajos_arapiuns"
  )
  expect_equal(nrow(contar_n_obs_sp_uc(dados_filtrados)), 60)
})
