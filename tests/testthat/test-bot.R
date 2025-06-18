test_that("modify_list edge cases", {
  expect_identical(modify_list(list(), list()), list())
  expect_identical(modify_list(list(a=1), list()), list(a=1))
  expect_identical(modify_list(list(), list(a=1)), list(a=1))
  expect_identical(modify_list(list(a=1, b=2), list(b=3, c=4)), list(a=1, b=3, c=4))
})

test_that("as_scalar edge cases", {
  expect_identical(as_scalar(character(0)), character(0))
  expect_identical(as_scalar(1:2), 1:2)
  expect_identical(as_scalar(list(1)), list(1))
  expect_identical(as_scalar(NULL), NULL)
  x <- as_scalar(1)
  expect_true(inherits(x, "scalar"))
  expect_identical(as_scalar("test"), structure("test", class=c("scalar", "character")))
})

test_that("read_json_conn error cases", {
  expect_error(read_json_conn(textConnection("invalid json")))
  expect_error(read_json_conn(textConnection('{"a":}')))
})

test_that("read_json_file error cases", {
  expect_error(read_json_file("nonexistent.json"))
  tmp <- tempfile()
  writeLines("invalid json", tmp)
  expect_error(read_json_file(tmp))
  unlink(tmp)
})

test_that("read_json_raw error cases", {
  expect_error(read_json_raw(as.raw(c(123, 34, 125))))
  expect_error(read_json_raw(raw(0)))
})

test_that("read_json_str error cases", {
  expect_error(read_json_str(""))
  expect_error(read_json_str('{"a":}'))
  expect_error(read_json_str('[1,2,'))
})

test_that("write_json_file with invalid path", {
  expect_error(write_json_file(list(a=1), "/invalid/path/file.json"))
})

test_that("validate_json edge cases", {
  expect_false(validate_json_str(""))
  expect_false(validate_json_str("invalid"))
  expect_true(validate_json_str("null"))
  expect_true(validate_json_str("true"))
  expect_true(validate_json_str("false"))
  expect_true(validate_json_str("123"))
  expect_true(validate_json_str('"string"'))
})

test_that("ndjson file error cases", {
  expect_error(read_ndjson_file("nonexistent.ndjson"))
  tmp <- tempfile()
  writeLines("invalid", tmp)
  expect_error(read_ndjson_file(tmp))
  unlink(tmp)
})

test_that("ndjson string error cases", {
  expect_error(read_ndjson_str("invalid\ninvalid"))
  expect_error(read_ndjson_str(""))
})

test_that("ndjson raw error cases", {
  expect_error(read_ndjson_raw(as.raw(c(105, 110, 118))))
})

test_that("ndjson type argument validation", {
  expect_error(read_ndjson_str("1\n2", type="invalid"))
  expect_identical(read_ndjson_str("1\n2", type="list"), list(1, 2))
})

test_that("ndjson parameter edge cases", {
  tmp <- tempfile()
  write_ndjson_file(data.frame(x=1:3), tmp)
  expect_identical(nrow(read_ndjson_file(tmp, nread=1)), 1L)
  expect_identical(nrow(read_ndjson_file(tmp, nskip=1)), 2L)
  expect_identical(nrow(read_ndjson_file(tmp, nread=1, nskip=1)), 1L)
  unlink(tmp)
})

test_that("write_ndjson error cases", {
  expect_error(write_ndjson_file(list(1,2), "/invalid/path.ndjson"))
})

test_that("geojson file error cases", {
  expect_error(read_geojson_file("nonexistent.geojson"))
  tmp <- tempfile()
  writeLines("invalid", tmp)
  expect_error(read_geojson_file(tmp))
  unlink(tmp)
})

test_that("geojson string error cases", {
  expect_error(read_geojson_str("invalid"))
  expect_error(read_geojson_str("{}"))
})

test_that("write_geojson error cases", {
  expect_error(write_geojson_file(list(a=1), "/invalid/path.geojson"))
})

test_that("opts_read_json parameter validation", {
  expect_error(opts_read_json(int64="invalid"))
  expect_error(opts_read_json(str_specials="invalid"))
  expect_error(opts_read_json(num_specials="invalid"))
  expect_identical(opts_read_json(digits_promote=10)$digits_promote, 10L)
  expect_identical(opts_read_json(promote_num_to_string=TRUE)$promote_num_to_string, TRUE)
  expect_identical(opts_read_json(obj_of_arrs_to_df=FALSE)$obj_of_arrs_to_df, FALSE)
  expect_identical(opts_read_json(arr_of_objs_to_df=FALSE)$arr_of_objs_to_df, FALSE)
  expect_identical(opts_read_json(length1_array_asis=TRUE)$length1_array_asis, TRUE)
})

test_that("opts_write_json parameter validation", {
  expect_error(opts_write_json(dataframe="invalid"))
  expect_error(opts_write_json(factor="invalid"))
  expect_error(opts_write_json(name_repair="invalid"))
  expect_error(opts_write_json(num_specials="invalid"))
  expect_error(opts_write_json(str_specials="invalid"))
  expect_identical(opts_write_json(digits=5)$digits, 5L)
  expect_identical(opts_write_json(digits_secs=3)$digits_secs, 3L)
  expect_identical(opts_write_json(digits_signif=4)$digits_signif, 4L)
  expect_identical(opts_write_json(auto_unbox=TRUE)$auto_unbox, TRUE)
  expect_identical(opts_write_json(pretty=TRUE)$pretty, TRUE)
  expect_identical(opts_write_json(fast_numerics=TRUE)$fast_numerics, TRUE)
  expect_identical(opts_write_json(json_verbatim=TRUE)$json_verbatim, TRUE)
})

test_that("opts_read_geojson parameter validation", {
  expect_error(opts_read_geojson(type="invalid"))
  expect_error(opts_read_geojson(property_promotion="invalid"))
  expect_error(opts_read_geojson(property_promotion_lgl="invalid"))
  expect_identical(opts_read_geojson(type="sfc")$type, "sfc")
  expect_identical(opts_read_geojson(property_promotion="list")$property_promotion, "list")
  expect_identical(opts_read_geojson(property_promotion_lgl="string")$property_promotion_lgl, "string")
})

test_that("flag constants structure", {
  expect_true(is.list(yyjson_read_flag))
  expect_true(is.list(yyjson_write_flag))
  expect_true(all(sapply(yyjson_read_flag, is.integer)))
  expect_true(all(sapply(yyjson_write_flag, is.integer)))
  expect_true(length(yyjson_read_flag) > 0)
  expect_true(length(yyjson_write_flag) > 0)
})

test_that("json with custom flags", {
  expect_identical(read_json_str("[1,2,3,]", yyjson_read_flag=yyjson_read_flag$YYJSON_READ_ALLOW_TRAILING_COMMAS), c(1,2,3))
  expect_identical(read_json_str("/* comment */ [1]", yyjson_read_flag=yyjson_read_flag$YYJSON_READ_ALLOW_COMMENTS), 1)
  expect_identical(read_json_str("Infinity", yyjson_read_flag=yyjson_read_flag$YYJSON_READ_ALLOW_INF_AND_NAN), Inf)
  expect_identical(read_json_str("NaN", yyjson_read_flag=yyjson_read_flag$YYJSON_READ_ALLOW_INF_AND_NAN), NaN)
})

test_that("json with write flags", {
  expect_match(write_json_str(list(a=1), yyjson_write_flag=yyjson_write_flag$YYJSON_WRITE_PRETTY), "\\n")
  expect_match(write_json_str("a/b", yyjson_write_flag=yyjson_write_flag$YYJSON_WRITE_ESCAPE_SLASHES), "\\\\/")
  expect_match(write_json_str("café", yyjson_write_flag=yyjson_write_flag$YYJSON_WRITE_ESCAPE_UNICODE), "\\\\u")
})

test_that("special numeric handling", {
  expect_identical(write_json_str(Inf, num_specials="string"), '"Inf"')
  expect_identical(write_json_str(NaN, num_specials="string"), '"NaN"')
  expect_identical(write_json_str(NA_real_, num_specials="string"), '"NA"')
  expect_identical(write_json_str(NA_character_, str_specials="string"), '"NA"')
})

test_that("data frame serialization options", {
  df <- data.frame(a=1:2, b=letters[1:2])
  expect_match(write_json_str(df, dataframe="columns"), '"a":\\[1,2\\]')
  expect_match(write_json_str(df, dataframe="rows"), '\\[\\{"a":1')
})

test_that("factor serialization options", {
  f <- factor(c("a", "b"))
  expect_match(write_json_str(f, factor="string"), '"a"')
  expect_match(write_json_str(f, factor="integer"), "1")
})

test_that("edge case inputs", {
  expect_identical(read_json_str("null"), NULL)
  expect_identical(write_json_str(NULL), "null")
  expect_identical(read_json_str("[]"), list())
  expect_identical(write_json_str(list()), "[]")
  expect_identical(read_json_str("{}"), structure(list(), names=character(0)))
})

test_that("large numbers", {
  big_int <- "9223372036854775807"
  expect_identical(read_json_str(big_int, int64="string"), big_int)
  expect_identical(class(read_json_str(big_int, int64="double")), "numeric")
})

test_that("connection handling", {
  tmp <- tempfile()
  writeLines('{"test": 123}', tmp)
  conn <- file(tmp, "r")
  result <- read_json_conn(conn)
  close(conn)
  expect_identical(result$test, 123)
  unlink(tmp)
})

test_that("file path normalization", {
  tmp <- tempfile()
  write_json_file(list(a=1), tmp)
  expect_true(file.exists(tmp))
  result <- read_json_file(tmp)
  expect_identical(result$a, 1)
  unlink(tmp)
})

test_that("various data types", {
  expect_identical(write_json_str(TRUE), "true")
  expect_identical(write_json_str(FALSE), "false")
  expect_identical(read_json_str("true"), TRUE)
  expect_identical(read_json_str("false"), FALSE)
})

test_that("name repair options", {
  x <- list(a=1, 2, c=3)
  expect_match(write_json_str(x, name_repair="minimal"), '"2":2')
  expect_match(write_json_str(x, name_repair="none"), '"":2')
})

test_that("complex nested structures", {
  nested <- list(a=list(b=list(c=1:3)))
  json_str <- write_json_str(nested)
  result <- read_json_str(json_str)
  expect_identical(result$a$b$c, 1:3)
})

test_that("empty and null handling", {
  expect_identical(read_json_str('{"a":null}')$a, NULL)
  expect_identical(write_json_str(list(a=NULL)), '{"a":null}')
})

test_that("raw vector operations", {
  raw_data <- write_json_raw(list(x=1:3))
  expect_true(is.raw(raw_data))
  result <- read_json_raw(raw_data)
  expect_identical(result$x, 1:3)
})

test_that("version info", {
  version <- yyjson_version()
  expect_true(is.character(version))
  expect_equal(length(version), 1)
  expect_match(version, "^\\d+\\.\\d+\\.\\d+")
})
