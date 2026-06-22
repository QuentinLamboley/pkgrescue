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
