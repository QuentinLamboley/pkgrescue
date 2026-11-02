test_that("INLA is recognised as a dedicated official source", {
  candidate <- pkgrescue:::.ir_special_candidate("INLA")
  expect_equal(candidate$type, "inla")
  expect_equal(candidate$target, "INLA")
})

test_that("GitHub-style references are recognised automatically", {
  expect_equal(pkgrescue:::.ir_source_from_reference("nathoze/Rsero"), "github")
  expect_equal(pkgrescue:::.ir_source_from_reference("github::nathoze/Rsero"), "github")
})

test_that("ordinary names remain automatic", {
  expect_equal(pkgrescue:::.ir_source_from_reference("ggplot2"), "auto")
  expect_true(pkgrescue:::.ir_is_simple_package("RcppEigen"))
})

test_that("explicit CRAN references are resolved without repository discovery", {
  expect_equal(pkgrescue:::.ir_source_from_reference("cran::ggplot2"), "cran")
})

test_that("Quarto detection has a safe no-install result", {
  candidate <- pkgrescue:::.ir_find_quarto()
  expect_type(candidate, "character")
  expect_length(candidate, 1L)
  expect_true(nchar(candidate) == 0L || file.exists(candidate))
})

test_that("install_any requires a non-empty reference", {
  expect_error(pkgrescue::install_any("   "), "non-empty")
})
