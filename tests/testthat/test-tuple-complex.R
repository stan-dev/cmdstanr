# Tests for tuple and complex variable handling

# --- Unit tests for repair_variable_names ---

test_that("repair_variable_names handles standard array/matrix variables", {
  expect_equal(
    repair_variable_names(c("mu", "beta.1", "beta.2", "sigma.1.2")),
    c("mu", "beta[1]", "beta[2]", "sigma[1,2]")
  )
})

test_that("repair_variable_names handles complex scalar variables", {
  expect_equal(
    repair_variable_names(c("z.real", "z.imag")),
    c("z[real]", "z[imag]")
  )
})

test_that("repair_variable_names handles complex vector variables", {
  expect_equal(
    repair_variable_names(c("zv.1.real", "zv.1.imag", "zv.2.real", "zv.2.imag")),
    c("zv[1,real]", "zv[1,imag]", "zv[2,real]", "zv[2,imag]")
  )
})

test_that("repair_variable_names handles complex matrix variables", {
  expect_equal(
    repair_variable_names(c("zm.1.1.real", "zm.1.1.imag", "zm.2.3.real", "zm.2.3.imag")),
    c("zm[1,1,real]", "zm[1,1,imag]", "zm[2,3,real]", "zm[2,3,imag]")
  )
})

test_that("repair_variable_names handles array of complex matrix variables", {
  expect_equal(
    repair_variable_names(c("z3D.1.1.1.real", "z3D.1.1.1.imag", "z3D.4.2.3.real", "z3D.4.2.3.imag")),
    c("z3D[1,1,1,real]", "z3D[1,1,1,imag]", "z3D[4,2,3,real]", "z3D[4,2,3,imag]")
  )
})

test_that("repair_variable_names handles simple tuple variables", {
  expect_equal(
    repair_variable_names(c("pair:1", "pair:2")),
    c("pair:1", "pair:2")
  )
})

test_that("repair_variable_names handles nested tuple variables", {
  expect_equal(
    repair_variable_names(c("nested:1", "nested:2:1", "nested:2:2.real", "nested:2:2.imag")),
    c("nested:1", "nested:2:1", "nested:2:2[real]", "nested:2:2[imag]")
  )
})

test_that("repair_variable_names handles tuple with array indices", {
  expect_equal(
    repair_variable_names(c("b_tuple:1:1.1", "b_tuple:1:1.2", "b_tuple:2.1.1", "b_tuple:2.2.2")),
    c("b_tuple:1:1[1]", "b_tuple:1:1[2]", "b_tuple:2[1,1]", "b_tuple:2[2,2]")
  )
})

test_that("repair_variable_names handles array of tuples", {
  expect_equal(
    repair_variable_names(c("arr_pair.1:1", "arr_pair.1:2", "arr_pair.2:1", "arr_pair.2:2")),
    c("arr_pair[1:1]", "arr_pair[1:2]", "arr_pair[2:1]", "arr_pair[2:2]")
  )
})

# --- Unit tests for unrepair_variable_names (inverse) ---

test_that("unrepair_variable_names is inverse of repair_variable_names", {
  raw_names <- c(
    "mu", "beta.1.2",
    "z.real", "z.imag",
    "zv.1.real", "zv.2.imag",
    "zm.1.1.real", "zm.2.3.imag",
    "z3D.1.1.1.real", "z3D.4.2.3.imag",
    "pair:1", "pair:2",
    "nested:1", "nested:2:1", "nested:2:2.real", "nested:2:2.imag",
    "b_tuple:1:1.1", "b_tuple:2.1.1",
    "arr_pair.1:1", "arr_pair.2:2"
  )
  expect_equal(unrepair_variable_names(repair_variable_names(raw_names)), raw_names)
})

# --- Unit tests for variable_dims ---

test_that("variable_dims handles complex scalar (no NA, no warning)", {
  expect_silent(result <- variable_dims(c("z[real]", "z[imag]")))
  expect_equal(result, list(z = 2L))
})

test_that("variable_dims handles complex vector", {
  expect_silent(
    result <- variable_dims(c("zv[1,real]", "zv[1,imag]", "zv[2,real]", "zv[2,imag]"))
  )
  expect_equal(result, list(zv = c(2L, 2L)))
})

test_that("variable_dims handles complex matrix", {
  vars <- c(
    "zm[1,1,real]", "zm[1,1,imag]", "zm[2,1,real]", "zm[2,1,imag]",
    "zm[1,2,real]", "zm[1,2,imag]", "zm[2,2,real]", "zm[2,2,imag]",
    "zm[1,3,real]", "zm[1,3,imag]", "zm[2,3,real]", "zm[2,3,imag]"
  )
  expect_silent(result <- variable_dims(vars))
  expect_equal(result, list(zm = c(2L, 3L, 2L)))
})

test_that("variable_dims handles tuple leaf variables", {
  vars <- c("pair:1", "pair:2", "nested:1", "nested:2:1",
            "nested:2:2[real]", "nested:2:2[imag]")
  expect_silent(result <- variable_dims(vars))
  expect_equal(result$`pair:1`, 1)
  expect_equal(result$`pair:2`, 1)
  expect_equal(result$`nested:1`, 1)
  expect_equal(result$`nested:2:1`, 1)
  expect_equal(result$`nested:2:2`, 2L)
})

test_that("variable_dims handles array of tuples", {
  vars <- c("arr_pair[1:1]", "arr_pair[1:2]", "arr_pair[2:1]", "arr_pair[2:2]")
  expect_silent(result <- variable_dims(vars))
  expect_equal(result, list(arr_pair = 4L))
})

test_that("variable_dims handles mixed standard and complex/tuple variables", {
  vars <- c(
    "lp__", "a_scalar",
    "b_tuple:1:1[1]", "b_tuple:1:1[2]",
    "b_tuple:1:2[1]", "b_tuple:1:2[2]",
    "b_tuple:2[1,1]", "b_tuple:2[2,1]", "b_tuple:2[1,2]", "b_tuple:2[2,2]",
    "c_matrix[1,1]", "c_matrix[2,1]", "c_matrix[1,2]", "c_matrix[2,2]",
    "z[real]", "z[imag]"
  )
  expect_silent(result <- variable_dims(vars))
  expect_equal(result$lp__, 1)
  expect_equal(result$a_scalar, 1)
  expect_equal(result$`b_tuple:1:1`, 2L)
  expect_equal(result$`b_tuple:1:2`, 2L)
  expect_equal(result$`b_tuple:2`, c(2L, 2L))
  expect_equal(result$c_matrix, c(2L, 2L))
  expect_equal(result$z, 2L)
})

# --- Helper function tests ---

test_that("stan_param_has_leaf matches direct and tuple-expanded names", {
  leaf_names <- c("a_scalar", "b_tuple:1:1", "b_tuple:1:2", "b_tuple:2", "c_matrix", "z")
  expect_equal(
    stan_param_has_leaf(c("a_scalar", "b_tuple", "c_matrix", "z"), leaf_names),
    c(TRUE, TRUE, TRUE, TRUE)
  )
  expect_equal(
    stan_param_has_leaf(c("nonexistent", "b_tup"), leaf_names),
    c(FALSE, FALSE)
  )
})

test_that("expand_stan_params_to_leaves expands tuple names", {
  leaf_names <- c("a_scalar", "b_tuple:1:1", "b_tuple:1:2", "b_tuple:2", "c_matrix", "z")
  expect_equal(
    expand_stan_params_to_leaves(c("a_scalar", "b_tuple", "c_matrix", "z"), leaf_names),
    c("a_scalar", "b_tuple:1:1", "b_tuple:1:2", "b_tuple:2", "c_matrix", "z")
  )
  # Non-tuple names pass through
  expect_equal(
    expand_stan_params_to_leaves(c("a_scalar", "c_matrix"), leaf_names),
    c("a_scalar", "c_matrix")
  )
})

# --- End-to-end tests with Stan model ---

test_that("sampling model with tuple and complex types produces no warnings", {
  mod <- testing_model("tuple_complex")
  expect_no_warning(
    utils::capture.output(
      fit <- mod$sample(seed = 123, chains = 2, iter_sampling = 100,
                        iter_warmup = 100, refresh = 0)
    )
  )

  # Check metadata
  meta <- fit$metadata()
  expect_true("a_scalar" %in% meta$stan_variables)
  expect_true("b_tuple:1:1" %in% meta$stan_variables)
  expect_true("b_tuple:1:2" %in% meta$stan_variables)
  expect_true("b_tuple:2" %in% meta$stan_variables)
  expect_true("c_matrix" %in% meta$stan_variables)
  expect_true("z" %in% meta$stan_variables)
  expect_true("d_tuple:1" %in% meta$stan_variables)
  expect_true("pair:1" %in% meta$stan_variables)
  expect_true("pair:2" %in% meta$stan_variables)
  expect_true("nested:1" %in% meta$stan_variables)
  expect_true("nested:2:1" %in% meta$stan_variables)
  expect_true("nested:2:2" %in% meta$stan_variables)
  expect_true("arr_pair" %in% meta$stan_variables)
  expect_true("zv" %in% meta$stan_variables)
  expect_true("zm" %in% meta$stan_variables)
  expect_true("z3D" %in% meta$stan_variables)

  # Check dimensions have no NAs
  sizes <- meta$stan_variable_sizes
  for (var_name in names(sizes)) {
    expect_false(
      any(is.na(sizes[[var_name]])),
      info = paste("NA in stan_variable_sizes for", var_name)
    )
  }

  # Check specific dimensions
  expect_equal(sizes$z, 2L)
  expect_equal(sizes$zv, c(2L, 2L))
  expect_equal(sizes$zm, c(2L, 3L, 2L))
  expect_equal(sizes$z3D, c(4L, 2L, 3L, 2L))
  expect_equal(sizes$`b_tuple:1:1`, 2L)
  expect_equal(sizes$`b_tuple:2`, c(2L, 2L))
  expect_equal(sizes$`pair:1`, 1)
  expect_equal(sizes$`nested:2:2`, 2L)

  # Check draws work with posterior
  dr <- fit$draws()
  expect_true("z[real]" %in% posterior::variables(dr))
  expect_true("z[imag]" %in% posterior::variables(dr))
  expect_true("pair:1" %in% posterior::variables(dr))
  expect_true("arr_pair[1:1]" %in% posterior::variables(dr))
})

test_that("variable_skeleton works with tuple and complex parameters", {
  skip_if(os_is_wsl())
  mod <- cmdstan_model(testing_stan_file("tuple_complex"), force_recompile = TRUE)
  utils::capture.output(
    fit <- mod$sample(seed = 123, chains = 2, iter_sampling = 100,
                      iter_warmup = 100, refresh = 0)
  )

  skel <- fit$variable_skeleton()
  expect_false(any(is.na(names(skel))))
  expect_true("a_scalar" %in% names(skel))
  expect_true("c_matrix" %in% names(skel))
  expect_true("z" %in% names(skel))
  # Tuple leaves should be expanded
  expect_true("b_tuple.1.1" %in% names(skel))
  expect_true("b_tuple.1.2" %in% names(skel))
  expect_true("b_tuple.2" %in% names(skel))
  # Check dimensions
  expect_equal(dim(skel$z), 2L)
  expect_equal(dim(skel$c_matrix), c(2L, 2L))
  expect_equal(dim(skel$b_tuple.1.1), 2L)
  expect_equal(dim(skel$b_tuple.2), c(2L, 2L))
})

test_that("unconstrain_draws works with tuple and complex parameters", {
  skip_if(os_is_wsl())
  mod <- cmdstan_model(testing_stan_file("tuple_complex"), force_recompile = TRUE)
  utils::capture.output(
    fit <- mod$sample(seed = 123, chains = 2, iter_sampling = 100,
                      iter_warmup = 100, refresh = 0)
  )

  expect_no_error(udraws <- fit$unconstrain_draws())
  uvar_names <- posterior::variables(udraws)
  # Should include the tuple leaf names and complex parts
  expect_true("a_scalar" %in% uvar_names)
  expect_true("b_tuple:1:1[1]" %in% uvar_names)
  expect_true("b_tuple:1:1[2]" %in% uvar_names)
  expect_true("b_tuple:2[1,1]" %in% uvar_names)
  expect_true("c_matrix[1,1]" %in% uvar_names)
  expect_true("z[real]" %in% uvar_names)
  expect_true("z[imag]" %in% uvar_names)
})

# --- Tests for write_stan_json with tuples ---

test_that("write_stan_json handles simple tuple values", {
  f <- tempfile(fileext = ".json")
  data <- list(pair = list("1" = 1.5, "2" = 3.4))
  write_stan_json(data, f)
  json <- jsonlite::read_json(f)
  expect_equal(json$pair[["1"]], 1.5)
  expect_equal(json$pair[["2"]], 3.4)
})

test_that("write_stan_json handles nested tuple values", {
  f <- tempfile(fileext = ".json")
  data <- list(
    b_tuple = list(
      "1" = list("1" = c(1.1, 2.2), "2" = c(3.3, 4.4)),
      "2" = matrix(c(1, 2, 3, 4), 2, 2)
    )
  )
  write_stan_json(data, f)
  json_text <- paste(readLines(f), collapse = "\n")
  # Verify the JSON contains the expected nested structure
  expect_true(grepl('"b_tuple"', json_text))
  expect_true(grepl('"1":\\s*\\{', json_text))
  expect_true(grepl('"2":\\s*\\[', json_text))
  # Verify inner arrays
  expect_true(grepl("1.1,\\s*2.2", json_text))
  expect_true(grepl("3.3,\\s*4.4", json_text))
})

test_that("write_stan_json still handles array lists correctly", {
  f <- tempfile(fileext = ".json")
  data <- list(x = list(1:3, 4:6))
  write_stan_json(data, f)
  json <- jsonlite::read_json(f, simplifyVector = TRUE)
  expect_equal(json$x[1, ], c(1, 2, 3))
  expect_equal(json$x[2, ], c(4, 5, 6))
})

# --- Tests for build_tuple_init_value ---

test_that("build_tuple_init_value reconstructs nested tuple from draws", {
  mod <- cmdstan_model(testing_stan_file("tuple_complex"), force_recompile = TRUE)
  utils::capture.output(
    fit <- mod$sample(seed = 123, chains = 1, iter_sampling = 10, refresh = 0)
  )
  dr <- fit$draws()
  draws_rvar <- posterior::as_draws_rvars(dr)
  mv <- mod$variables()

  tuple_result <- build_tuple_init_value("b_tuple", mv$parameters$b_tuple,
                                         draws_rvar, 1)
  expect_true(is.list(tuple_result))
  expect_equal(length(tuple_result$bad_leaves), 0)
  result <- tuple_result$value
  # Should be a named list with keys "1" and "2"
  expect_true(is.list(result))
  expect_equal(names(result), c("1", "2"))
  # Element "1" is a nested tuple with keys "1" and "2"
  expect_true(is.list(result[["1"]]))
  expect_equal(names(result[["1"]]), c("1", "2"))
  # Element "1"."1" is array[2] real
  expect_equal(length(result[["1"]][["1"]]), 2)
  expect_true(is.numeric(result[["1"]][["1"]]))
  # Element "2" is matrix[2,2]
  expect_equal(dim(result[["2"]]), c(2, 2))
})

# --- End-to-end init = fit with tuple parameters ---

test_that("init = fit works with tuple parameters", {
  mod <- cmdstan_model(testing_stan_file("tuple_complex"), force_recompile = TRUE)
  utils::capture.output(
    fit <- mod$sample(seed = 123, chains = 2, iter_sampling = 100,
                      iter_warmup = 100, refresh = 0)
  )

  # Check that init JSON includes b_tuple
  init_files <- process_init(fit, num_procs = 1,
                             model_variables = mod$variables())
  json <- jsonlite::read_json(init_files)
  expect_true("b_tuple" %in% names(json))
  expect_true("a_scalar" %in% names(json))
  expect_true("c_matrix" %in% names(json))
  expect_true("z" %in% names(json))
  # b_tuple should be a nested object
  expect_true(is.list(json$b_tuple))
  expect_true("1" %in% names(json$b_tuple))
  expect_true("2" %in% names(json$b_tuple))

  # Second sampling with init = fit should succeed without missing-param warnings
  suppressMessages(
    utils::capture.output(
      fit2 <- mod$sample(seed = 456, chains = 2, iter_sampling = 100,
                         iter_warmup = 100, refresh = 0, init = fit)
    )
  )
  expect_true(inherits(fit2, "CmdStanMCMC"))
})

test_that("manual init list with tuple values works", {
  mod <- cmdstan_model(testing_stan_file("tuple_complex"), force_recompile = TRUE)
  init_list <- list(list(
    a_scalar = 0.5,
    b_tuple = list(
      "1" = list("1" = c(0.1, 0.2), "2" = c(0.3, 0.4)),
      "2" = matrix(c(0.5, 0.6, 0.7, 0.8), 2, 2)
    ),
    c_matrix = matrix(c(0.1, 0.2, 0.3, 0.4), 2, 2),
    z = c(0.1, 0.2)
  ))
  expect_no_error(
    utils::capture.output(
      fit <- mod$sample(seed = 123, chains = 1, iter_sampling = 100,
                        iter_warmup = 100, refresh = 0, init = init_list)
    )
  )
})
