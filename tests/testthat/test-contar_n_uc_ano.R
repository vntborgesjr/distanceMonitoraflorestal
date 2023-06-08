test_that("retorna um tibble", {
  expect_equal(dplyr::is.tbl(contar_n_uc_ano()), TRUE)
})

test_that("retorna 8 colunas", {
  expect_equal(ncol(contar_n_uc_ano()), 2)
})

