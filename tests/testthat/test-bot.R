test_that("error paths and edge cases", {
  expect_error(read_json_str(""))
  expect_error(read_json_str("["))
  expect_error(read_json_str(NULL))
  expect_error(read_json_str(c("a", "b")))
  expect_error(read_json_raw(NULL))
  expect_error(read_json_raw("not raw"))
  expect_error(read_json_file("nonexistent.json"))
  expect_error(validate_json_str(""))
  expect_error(validate_json_str("["))
  expect_error(validate_json_file("nonexistent.json"))
  
  con <- textConnection("")
  expect_error(read_json_conn(con))
  close(con)
  
  expect_error(write_json_file(list(), ""))
  expect_error(write_ndjson_file(NULL, tempfile()))
  expect_error(write_geojson_file(list(), tempfile()))
  
  expect_error(read_ndjson_str("", type = "invalid"))
  expect_error(read_ndjson_file("nonexistent.ndjson", type = "invalid"))
  expect_error(read_ndjson_raw(raw(), type = "invalid"))
  
  expect_error(opts_read_json(str_specials = "invalid"))
  expect_error(opts_read_json(num_specials = "invalid"))  
  expect_error(opts_read_json(int64 = "invalid"))
  expect_error(opts_write_json(dataframe = "invalid"))
  expect_error(opts_write_json(factor = "invalid"))
  expect_error(opts_write_json(name_repair = "invalid"))
  expect_error(opts_write_json(str_specials = "invalid"))
  expect_error(opts_write_json(num_specials = "invalid"))
  
  expect_error(opts_read_geojson(type = "invalid"))
  expect_error(opts_read_geojson(property_promotion = "invalid"))
  expect_error(opts_read_geojson(property_promotion_lgl = "invalid"))
  
  expect_error(read_geojson_str("["))
  expect_error(read_geojson_file("nonexistent.geojson"))
})

test_that("edge cases and boundary conditions", {
  expect_identical(read_json_str("null"), NULL)
  expect_identical(read_json_str("[]"), list())
  expect_identical(read_json_str("{}"), list())  
  expect_identical(write_json_str(NULL), "null")
  expect_identical(write_json_str(list()), "[]")
  expect_identical(write_json_str(setNames(list(), character())), "{}")
  
  expect_true(validate_json_str("null"))
  expect_true(validate_json_str("[]"))
  expect_true(validate_json_str("{}"))
  
  tmp <- tempfile()
  writeLines("null", tmp)
  expect_true(validate_json_file(tmp))
  expect_identical(read_json_file(tmp), NULL)
  unlink(tmp)
  
  expect_identical(as_scalar(1), structure(1, class = c("scalar", "numeric")))
  expect_identical(as_scalar(c(1, 2)), c(1, 2))
  expect_identical(as_scalar(list(1)), list(1))
  expect_identical(as_scalar(character(0)), character(0))
  
  expect_type(yyjson_version(), "character")
  expect_length(yyjson_version(), 1)
})

test_that("option flag combinations", {
  expect_identical(read_json_str("1", digits_promote = 0), 1)
  expect_identical(read_json_str("1", digits_promote = 10), 1)
  expect_identical(read_json_str("1", length1_array_asis = TRUE), structure(1, class = "AsIs"))
  expect_identical(read_json_str("1", single_null = "custom"), 1)
  expect_identical(read_json_str("null", single_null = "custom"), "custom")
  
  expect_identical(write_json_str(1, digits = 0), "[1]")
  expect_identical(write_json_str(1, digits_secs = 3), "[1.0]")
  expect_identical(write_json_str(1, digits_signif = 2), "[1.0]")
  expect_identical(write_json_str(1, pretty = TRUE), "[\n    1.0\n]")
  expect_identical(write_json_str(list(a = 1), name_repair = "minimal"), '{"a":1.0}')
  expect_identical(write_json_str(list(1), name_repair = "minimal"), '{"1":1.0}')
  expect_identical(write_json_str(NA_character_, str_specials = "string"), '["NA"]')
  expect_identical(write_json_str(NA_real_, num_specials = "string"), '["NA"]')
  expect_identical(write_json_str(1, fast_numerics = TRUE), "[1.0]")
  expect_identical(write_json_str(structure("null", class = "json"), json_verbatim = TRUE), "[null]")
  
  flags <- c(yyjson_read_flag$YYJSON_READ_ALLOW_TRAILING_COMMAS, yyjson_read_flag$YYJSON_READ_ALLOW_COMMENTS)
  expect_identical(read_json_str("[1,2,] /* comment */", yyjson_read_flag = flags), c(1L, 2L))
  
  wflags <- c(yyjson_write_flag$YYJSON_WRITE_PRETTY, yyjson_write_flag$YYJSON_WRITE_ESCAPE_SLASHES)
  expect_match(write_json_str("a/b", yyjson_write_flag = wflags), "a\\\\/b")
})

test_that("ndjson edge cases", {
  expect_identical(read_ndjson_str("", nread = 0), data.frame())
  expect_identical(read_ndjson_str("", type = "list", nread = 0), list())
  expect_identical(read_ndjson_str("{}", nskip = 1), data.frame())
  expect_identical(read_ndjson_str("{}\n{}", nread = 1), data.frame())
  expect_identical(read_ndjson_str("{}\n{}", nprobe = 1), data.frame())
  expect_identical(read_ndjson_str("{}\n{}", nprobe = -1), data.frame())
  
  expect_identical(read_ndjson_raw(raw(), type = "df"), data.frame())
  expect_identical(read_ndjson_raw(raw(), type = "list"), list())
  
  expect_identical(write_ndjson_str(data.frame()), character(0))
  expect_identical(write_ndjson_str(list()), character(0))
  expect_identical(write_ndjson_raw(data.frame()), raw())
  expect_identical(write_ndjson_raw(list()), raw())
  
  tmp <- tempfile()
  write_ndjson_file(data.frame(), tmp)
  expect_identical(read_ndjson_file(tmp), data.frame())
  expect_identical(read_ndjson_file(tmp, type = "list"), list())
  unlink(tmp)
})

test_that("geojson edge cases", {
  expect_identical(opts_write_geojson(), structure(list(), class = "opts_write_geojson"))
  
  opts <- opts_read_geojson(type = "sfc", property_promotion = "list", property_promotion_lgl = "string")
  expect_equal(opts$type, "sfc")
  expect_equal(opts$property_promotion, "list")
  expect_equal(opts$property_promotion_lgl, "string")
})

test_that("verbose validation", {
  expect_warning(validate_json_str("[", verbose = TRUE))
  expect_silent(validate_json_str("[]", verbose = TRUE))
  
  tmp <- tempfile()
  writeLines("[", tmp)
  expect_warning(validate_json_file(tmp, verbose = TRUE))
  unlink(tmp)
})

test_that("connection edge cases", {
  con <- textConnection("null")
  expect_identical(read_json_conn(con), NULL)
  close(con)
  
  con <- textConnection(c("1", "2"))
  expect_identical(read_json_conn(con), c(1L, 2L))
  close(con)
})

test_that("modify_list utility", {
  expect_identical(yyjsonr:::modify_list(list(a = 1), list(b = 2)), list(a = 1, b = 2))
  expect_identical(yyjsonr:::modify_list(list(a = 1), list(a = 2)), list(a = 2))
  expect_identical(yyjsonr:::modify_list(list(), list(a = 1)), list(a = 1))
  expect_identical(yyjsonr:::modify_list(list(a = 1), list()), list(a = 1))
})

test_that("type conversion edge cases", {
  expect_identical(read_json_str("1", obj_of_arrs_to_df = FALSE), 1)
  expect_identical(read_json_str("1", arr_of_objs_to_df = FALSE), 1)
  expect_identical(read_json_str("1", promote_num_to_string = TRUE), 1)
  expect_identical(read_json_str("[1,\"a\"]", promote_num_to_string = TRUE), c("1.000000", "a"))
  expect_identical(read_json_str("1", str_specials = "special"), 1)
  expect_identical(read_json_str("1", num_specials = "string"), 1)
  expect_identical(read_json_str("1", int64 = "double"), 1)
  expect_identical(read_json_str("1", int64 = "bit64"), 1)
  expect_identical(read_json_str("1", df_missing_list_elem = "missing"), 1)
  
  expect_identical(write_json_str(data.frame(x = 1), dataframe = "columns"), '{"x":[1.0]}')
  expect_identical(write_json_str(factor("a"), factor = "integer"), "[1]")
  expect_identical(write_json_str(factor("a"), factor = "string"), '["a"]')
})

test_that("complex yyjson flag combinations", {
  flags <- yyjson_read_flag$YYJSON_READ_ALLOW_INF_AND_NAN
  expect_identical(read_json_str("Infinity", yyjson_read_flag = flags), Inf)
  expect_identical(read_json_str("NaN", yyjson_read_flag = flags), NaN)
  
  flags <- yyjson_read_flag$YYJSON_READ_NUMBER_AS_RAW
  expect_type(read_json_str("123", yyjson_read_flag = flags), "character")
  
  flags <- yyjson_read_flag$YYJSON_READ_BIGNUM_AS_RAW
  expect_type(read_json_str("123", yyjson_read_flag = flags), "integer")
  
  flags <- yyjson_write_flag$YYJSON_WRITE_ALLOW_INF_AND_NAN
  expect_match(write_json_str(Inf, yyjson_write_flag = flags), "Infinity")
  
  flags <- yyjson_write_flag$YYJSON_WRITE_INF_AND_NAN_AS_NULL
  expect_identical(write_json_str(Inf, yyjson_write_flag = flags), "[null]")
  
  flags <- yyjson_write_flag$YYJSON_WRITE_ESCAPE_UNICODE
  expect_match(write_json_str("α", yyjson_write_flag = flags), "\\\\u")
  
  flags <- yyjson_write_flag$YYJSON_WRITE_PRETTY_TWO_SPACES
  expect_match(write_json_str(list(1), yyjson_write_flag = flags), "  ")
  
  flags <- yyjson_write_flag$YYJSON_WRITE_NEWLINE_AT_END
  expect_match(write_json_str(1, yyjson_write_flag = flags), "\\n$")
})
