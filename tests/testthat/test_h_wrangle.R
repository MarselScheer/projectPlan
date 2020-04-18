testthat::context("Wrangle")

logger::log_threshold(logger::FATAL, namespace = "projectPlan")


test_that(
  "Conversion of numeric dates", {
    expect_equal(h.convert_numeric_date(letters[1:3], date_origin = "1899-12-30"), letters[1:3])
    expect_equal(h.convert_numeric_date(rep(NA_character_, 3), date_origin = "1899-12-30"), rep(NA_character_, 3))
    expect_equal(h.convert_numeric_date(c("43529", "43530"), date_origin = "1899-12-30"), c("2019-03-05", "2019-03-06"))
  })


d_in <- data.table::data.table(depends_on = c("A"), start = c("TODAY"))
d_expected <- data.table::data.table(depends_on = c("A"), start = as.character(NA), fixed_start_date = lubridate::as_date(lubridate::now()))
test_that(
  "TODAY becomes the current date",
  expect_equal(h.rd_preprocess_start_column(d_in, date_origin = "1899-12-30"), d_expected)
)

d_in <- data.table::data.table(depends_on = c("A"), start = c(NA))
d_expected <- data.table::data.table(depends_on = c("A"), start = as.character(NA), fixed_start_date = lubridate::as_date(NA))
test_that(
  "start-column contains only NA",
  expect_equal(h.rd_preprocess_start_column(d_in, date_origin = "1899-12-30"), d_expected)
)

d_in <- data.table::data.table(
  project = "p", section = "s", id = "i",
  depends_on = c(NA, "A"), start = c(NA, "B"))
d_expected <- data.table::data.table(
  project = "p", section = "s", id = "i",
  depends_on = c(NA, "A"), start = c(NA, "B"), fixed_start_date = c(lubridate::as_date(lubridate::now()), NA))
test_that(
  "Current date is default if no dependency and no start is defined",
  expect_equal(h.rd_preprocess_start_column(d_in, date_origin = "1899-12-30"), d_expected)
)


d_in <- data.table::data.table(end = c("2018-09-20", "WAIT", "43530"))
d_expected <- data.table::data.table(
  fixed_end_date = c(lubridate::ymd("2018-09-20"), NA, lubridate::ymd("2019-03-06"))
)
test_that(
  "ymd and 'numeric' ín -end- are processed correctly", {
    expect_equal(h.rd_preprocess_end_column(d_in, date_origin = "1899-12-30"), d_expected)
  }
)

d_in <- data.table::data.table(deadline = c("2018-09-20", "2018-10-27"))
d_expected <- data.table::data.table(
  deadline = c(lubridate::ymd("2018-09-20"), lubridate::ymd("2018-10-29")),
  raw_deadline = c("2018-09-20", "2018-10-27")
)
test_that(
  "ymd in -deadline- are processed correctly", {
    expect_equal(h.rd_preprocess_deadline_column(d_in, date_origin = "1899-12-30"), d_expected)
    expect_error(h.rd_preprocess_deadline_column(data.table::data.table(deadline = "20-09-2018"), date_origin = "1899-12-30"), regexp = "must be.*ymd")
  }
)

d_in <- data.table::data.table(
  project = "A", id = "I", section = "S",
  depends_on = c("a", "a, B::b"),
  start = c("b, B::b", "c, d, C::h"),
  task = letters[3:4],
  resource = letters[5:6],
  microtasks = letters[7:8],
  comments = letters[9:10],
  progress = c(10, 90),
  deadline = lubridate::ymd(c("2018-09-20", "2018-09-21")),
  fixed_start_date = lubridate::ymd(c("2018-09-10", "2018-09-11")),
  fixed_end_date = lubridate::ymd(c("2018-09-15", "2018-09-16")),
  est_days = c(2, 3),
  waiting = c(F, T),
  aborted = c(T, F),
  unscheduled = c(T, F)
)

d_expected <- data.table::data.table(
  project = "A", id = "A::I",
  depends_on = list(c("A::a", "B::b")),
  start = list(c("A::b", "A::c", "A::d", "B::b", "C::h")),
  prior_ids = list(c("A::a", "A::b", "A::c", "A::d", "B::b", "C::h")),
  section = "A::S",
  resource = c("e, f"),
  task = c("c, d"),
  microtasks = c("g, h"),
  comments = c("i, j"),
  progress = c(50),
  deadline = lubridate::ymd(c("2018-09-20")),
  fixed_start_date = lubridate::ymd(c("2018-09-10")),
  fixed_end_date = lubridate::ymd(c("2018-09-16")),
  est_days = c(5),
  waiting = c(T),
  aborted = c(T),
  unscheduled = c(T),
  nmb_combined_entries = 2L
)

test_that(
  "combine ids within project in order to make them unique", {
    expect_equal(h.rd_make_id_unique_within_project(d_in), d_expected)
  }
)


d_in <- data.frame(
  project = c("A"),
  section = NA_character_,
  id = c(NA_character_, NA_character_),
  depends_on = NA_character_,
  start = NA_character_,
  end = NA_character_,
  est_duration = NA_character_,
  status = NA_character_,
  resource = NA_character_,
  task = NA_character_,
  microtasks = NA_character_,
  comments = NA_character_,
  progress = NA,
  deadline = NA_character_,
  some_col = "will get lost"
)
d_expected <- data.table::data.table(
  project = c("A"),
  id = c("A::NOT_SPECIFIED_1", "A::NOT_SPECIFIED_2"),
  depends_on = NA_character_,
  start = NA_character_,
  prior_ids = NA_character_,
  section = "A::UNKNOWN",
  resource = "UNKNOWN",
  task = "UNKNOWN",
  microtasks = "-",
  comments = "-",
  progress = 0,
  deadline = lubridate::as_date(NA),
  fixed_start_date = lubridate::as_date(lubridate::now()),
  fixed_end_date = lubridate::as_date(NA),
  est_days = 1,
  waiting = FALSE,
  aborted = FALSE,
  unscheduled = FALSE,
  nmb_combined_entries = 1L
)
d_out <- wrangle_raw_plan(d_in)
test_that(
  "Complete raw data wrangling. check default values", {
    expect_equal(d_out, d_expected)
  }
)

d_in <- data.frame(
  project = c("A"),
  section = c("0_prep"),
  id = c("a", "a", "a"),
  depends_on = c("b, B::b", "b, b", "B::b"),
  start = c("2018-09-10", "2018-09-11", "2018-09-09"),
  end = c("2018-09-20", "2018-10-25", "2018-10-01"),
  est_duration = c("1", "2", "2"),
  status = c("aborted", "await", "unscheduled"),
  resource = c("r1, r2", "r2", "r3"),
  task = c("t1", "t2", "t3"),
  microtasks = c(NA_character_, NA_character_, ".t3_1"),
  comments = c("c1", NA_character_, "c3"),
  progress = c(0, 50, 70),
  deadline = c("2018-09-23", "2018-10-01", "2018-10-02"),
  stringsAsFactors = FALSE
)
d_expected <- data.table::data.table(
  project = c("A"),
  id = "A::a",
  depends_on = list(c("A::b", "B::b")),
  start = NA_character_,
  prior_ids = list(c("A::b", "B::b")),
  section = c("A::0_prep"),
  resource = c("r1, r2, r3"),
  task = c("t1, t2, t3"),
  microtasks = c("-, .t3_1"),
  comments = c("-, c1, c3"),
  progress = c(40),
  deadline = lubridate::as_date(c("2018-09-24")),
  fixed_start_date = lubridate::as_date(c("2018-09-09")),
  fixed_end_date = lubridate::as_date(c("2018-10-25")),
  est_days = 5,
  waiting = TRUE,
  aborted = TRUE,
  unscheduled = TRUE,
  nmb_combined_entries = 3L,
  stringsAsFactors = FALSE
)

d_out <- wrangle_raw_plan(d_in)
test_that(
  "Complete raw data wrangling with combining ids", {
    expect_equal(d_out, d_expected)
  }
)


d_in <- data.table::rbindlist(
  list(
    data.table::data.table(
      project = "A", id = letters[1:3], section = "S",
      depends_on = c(NA, "PREVIOUS, xyz", "PREVIOUS"),
      start = c("PREVIOUS", NA, NA)
    ),
    data.table::data.table(
      project = "B", id = letters[1:3], section = "S",
      depends_on = c(NA, "PREVIOUS", NA),
      start = c("PREVIOUS, A::a", NA, "PREVIOUS")
    )
  )
)

d_out_dep <- data.table::rbindlist(
  list(
    data.table::data.table(
      project = "A", id = letters[1:3], section = "S",
      depends_on = c(NA, "A::a, xyz", "A::b"),
      start = c("PREVIOUS", NA, NA)
    ),
    data.table::data.table(
      project = "B", id = letters[1:3], section = "S",
      depends_on = c(NA, "B::a", NA),
      start = c("PREVIOUS, A::a", NA, "PREVIOUS")
    )
  )
)

d_out_start <- data.table::rbindlist(
  list(
    data.table::data.table(
      project = "A", id = letters[1:3], section = "S",
      depends_on = c(NA, "PREVIOUS, xyz", "PREVIOUS"),
      start = c("PREVIOUS", NA, NA)
    ),
    data.table::data.table(
      project = "B", id = letters[1:3], section = "S",
      depends_on = c(NA, "PREVIOUS", NA),
      start = c("A::a, A::c", NA, "B::b")
    )
  )
)


test_that(
  "PREVIOUS tag is replaced correctly", {
    expect_equal(h.replace_TAG_PREVIOUS(d_in, "depends_on"), d_out_dep)
    expect_equal(h.replace_TAG_PREVIOUS(d_in, "start"), d_out_start)  
  }
)


