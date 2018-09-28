context("Classifier Build")

num <- sboost(malware[-1], malware[1], 5, 1)
cat <- sboost(mushrooms[-1], mushrooms[1], 5, "p")

test_that("Arguments are properly used.", {
  expect_equal(num$outcomes[["positive"]], 1)
  expect_equal(cat$outcomes[["positive"]], "p")

  expect_equal(nrow(num$classifier), 5)
  expect_equal(nrow(cat$classifier), 5)

  expect_equal(num$training$stumps, 5)
  expect_equal(cat$training$stumps, 5)

  expect_equal(num$training$features, 360)
  expect_equal(cat$training$features, 22)

  expect_equal(num$training$instances, 7597)
  expect_equal(cat$training$instances, 8124)

  expect_equal(round(num$training$positive_prevalence, digits = 5), 0.33882)
  expect_equal(round(cat$training$positive_prevalence, digits = 5), 0.48203)
})

test_that("Algorithm is classifying properly.", {
  expect_equal(num$classifier$feature, c("X195", "X356", "X148", "X290", "X250"))
  expect_equal(cat$classifier$feature, c("odor", "spore_print_color", "population", "odor", "population"))

  expect_equal(round(num$classifier$vote, digits = 5), c(0.52443, 0.37820, 0.38180, 0.25555, 0.23200))
  expect_equal(round(cat$classifier$vote, digits = 5), c(2.10010, 1.31712, 0.60860, 0.60788, 0.63077))

  expect_equal(num$classifier$orientation, c("1|0", "0|1", "1|0", "0|1", "1|0"))
  expect_equal(cat$classifier$orientation, c("p|e", "p|e", "p|e", "p|e", "p|e"))

  expect_equal(num$classifier$split, c(0.5, 2.5, 79.5, 7.5, 0.5))
  expect_true(all(is.na(cat$classifier$split)))

  expect_true(all(is.na(num$classifier$left_categories)))
  expect_equal(cat$classifier$left_categories, c("c; f; m; p; s; y", "h; r; w", "s; v", "c; f; m; p; s; y", "c; v; y"))
})




