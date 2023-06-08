testthat::test_that("retorna tibble", {
  testthat::expect_equal(dplyr::is.tbl(carregar_dados_brutos()), TRUE)
})

testthat::test_that("retorna 19 colunas", {
  testthat::expect_equal(ncol(carregar_dados_brutos()), 19)
})

testthat::test_that("retorna 26420 linhas", {
  testthat::expect_equal(nrow(carregar_dados_brutos()), 26420)
})

testthat::test_that("retorna 8 fatores", {
  testthat::expect_equal(carregar_dados_brutos() |>
                           dplyr::select(dplyr::where(is.factor)) |>
                           ncol(),
                         8
  )
})

testthat::test_that("retorna 10 double", {
  testthat::expect_equal(carregar_dados_brutos() |>
                           dplyr::select(dplyr::where(is.double)) |>
                           ncol(),
                         10
  )
})

testthat::test_that("retorna 1 integer", {
  testthat::expect_equal(carregar_dados_brutos() |>
                           dplyr::select(dplyr::where(is.integer)) |>
                           ncol(),
                         1
  )
})
