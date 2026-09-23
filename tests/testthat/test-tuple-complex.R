set_cmdstan_path()

tuple_complex_data <- list(
  d_pair = list(3, c(1, 2)),
  d_arr = list(list(1, 1), list(2, 2)),
  d_zv = c(5 + 6i, 7 + 8i)
)

mod <- testing_model("tuple_complex")
utils::capture.output(
  fit <- mod$sample(data = tuple_complex_data, chains = 2, iter_warmup = 100,
                    iter_sampling = 50, refresh = 0, seed = 1)
)

test_that("metadata reports tuple and complex names and sizes", {
  expect_no_warning(meta <- fit$metadata())
  expect_equal(
    meta$stan_variables,
    c("lp__", "a_scalar", "b_tuple", "c_matrix", "z", "d_tuple", "mu", "m",
      "threeD", "zv", "zm", "z3D", "base", "base_i", "pair", "nested",
      "arr_pair", "d_sum")
  )
  sizes <- meta$stan_variable_sizes
  expect_equal(sizes$z, 2)
  expect_equal(sizes$zv, c(2, 2))
  expect_equal(sizes$zm, c(2, 3, 2))
  expect_equal(sizes$z3D, c(4, 2, 3, 2))
  expect_equal(sizes$b_tuple, 1)
  expect_equal(sizes$nested, 1)
  expect_equal(sizes$arr_pair, 2)
})

test_that("a tuple's name selects all of its columns", {
  expect_equal(
    posterior::variables(fit$draws("b_tuple")),
    c("b_tuple:1:1[1]", "b_tuple:1:1[2]", "b_tuple:1:2[1]", "b_tuple:1:2[2]",
      "b_tuple:2[1,1]", "b_tuple:2[2,1]", "b_tuple:2[1,2]", "b_tuple:2[2,2]")
  )
  expect_equal(
    posterior::variables(fit$draws("arr_pair[1]")),
    c("arr_pair[1]:1", "arr_pair[1]:2")
  )
  expect_equal(fit$summary("pair")$variable, c("pair:1", "pair:2"))
  x <- read_cmdstan_csv(fit$output_files(), variables = "nested")
  expect_equal(
    posterior::variables(x$post_warmup_draws),
    c("nested:1", "nested:2:1", "nested:2:2[real]", "nested:2:2[imag]")
  )
})

test_that("data with tuple and complex values round trips by value", {
  expect_true(all(as.numeric(fit$draws("d_sum")) == 14))
})

test_that("init = fit carries tuple and complex parameters over", {
  utils::capture.output(
    fit2 <- mod$sample(data = tuple_complex_data, init = fit, chains = 2,
                       iter_warmup = 10, iter_sampling = 10, refresh = 0,
                       seed = 2)
  )
  init1 <- jsonlite::read_json(fit2$runset$args$init[1])
  expect_equal(names(init1$b_tuple), c("1", "2"))
  expect_length(init1$b_tuple[["1"]][["2"]], 2)
  expect_length(init1$z, 2)
})

test_that("a tuple and complex value can be given as a list init", {
  utils::capture.output(
    fit3 <- mod$sample(
      data = tuple_complex_data,
      init = list(list(
        b_tuple = list(list(c(0.1, 0.2), c(0.3, 0.4)), matrix(0.1, 2, 2)),
        z = 0.1 + 0.2i
      )),
      chains = 1, iter_warmup = 10, iter_sampling = 10, refresh = 0, seed = 3
    )
  )
  expect_equal(fit3$init()[[1]]$z, c(0.1, 0.2))
})

test_that("model methods work with tuple and complex variables", {
  skip_if(os_is_wsl())

  expect_equal(posterior::nvariables(fit$unconstrain_draws()), 15)

  x <- fit$constrain_variables(rep(0.1, 15), generated_quantities = FALSE)
  expect_equal(names(x), c("a_scalar", "b_tuple", "c_matrix", "z"))
  expect_true(is.complex(x$z))
  expect_equal(x$b_tuple[[1]][[2]], array(0.1, dim = 2))
  expect_equal(dim(x$b_tuple[[2]]), c(2, 2))

  expect_equal(fit$unconstrain_variables(x), rep(0.1, 15))

  gq <- fit$constrain_variables(rep(0.1, 15))
  expect_length(gq$arr_pair, 2)
  expect_true(all(sapply(gq$arr_pair, is.list)))
  expect_true(is.complex(gq$zm))
  expect_equal(dim(gq$zm), c(2, 3))
  expect_equal(dim(gq$z3D), c(4, 2, 3))
  expect_true(is.complex(gq$nested[[2]][[2]]))
})
