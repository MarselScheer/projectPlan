context("Wrangle")
library(projectPlan)

futile.logger::flog.threshold(futile.logger::FATAL, name = futile.logger::flog.namespace())


d_in <- dplyr::tibble(depends_on = c("A"), start = c("TODAY"))
d_expected <- dplyr::tibble(depends_on = c("A"), start = as.character(NA), fixed_start_date = lubridate::as_date(lubridate::now()))
test_that(
  "TODAY becomes the current date",
  expect_identical(h.rd_preprocess_start_column(d_in), d_expected)
)

d_in <- dplyr::tibble(depends_on = c(NA, "A"), start = c(NA, "B"))
d_expected <- dplyr::tibble(depends_on = c(NA, "A"), start = c(NA, "B"), fixed_start_date = c(lubridate::as_date(lubridate::now()), NA))
test_that(
  "Current date is default if no dependency and start is defined",
  expect_identical(h.rd_preprocess_start_column(d_in), d_expected)
)


d_in <- dplyr::tibble(end = c("2018-09-20", "WAIT", "3"))
d_expected <- dplyr::tibble(
  waiting = c(F, T, F), 
  est_days = c(NA, NA, 3),
  fixed_end_date = c(lubridate::ymd("2018-09-20"), NA, NA))
test_that(
  "WAIT, integer, ymd ín -end- are processed correctly", {
    expect_identical(h.rd_preprocess_end_column(d_in), d_expected)
    expect_error(h.rd_preprocess_end_column(dplyr::tibble(end = "a")), regexp = "must be 'WAIT'.*integer.*ymd")
  }
)


d_in <- dplyr::tibble(deadline = c("2018-09-20"))
d_expected <- dplyr::tibble(
  deadline = c(lubridate::ymd("2018-09-20")),
  raw_deadline = c("2018-09-20"))
test_that(
  "ymd ín -deadline- are processed correctly", {
    expect_identical(h.rd_preprocess_deadline_column(d_in), d_expected)
    expect_error(h.rd_preprocess_deadline_column(dplyr::tibble(deadline = "20-09-2018")), regexp = "must be.*ymd")
  }
)

d_in <- dplyr::tibble(
  project = "A", id = "I", section = "S",
  depends_on = c("a", "a, B::b"), 
  start = c("b, B::b", "c, d, C::h"),
  task = letters[3:4],
  resource = letters[5:6],
  progress = c(10, 90),
  deadline = lubridate::ymd(c("2018-09-20", "2018-09-21")),
  fixed_start_date = lubridate::ymd(c("2018-09-10", "2018-09-11")),
  fixed_end_date = lubridate::ymd(c("2018-09-15", "2018-09-16")),
  est_days = c(2,3),
  waiting = c(T, F))

d_expected <- data.table::data.table(
  project = "A", id = "A::I", 
  depends_on = list(c("A::a", "B::b")), 
  start = list(c("A::b", "B::b", "A::c", "C::h", "A::d")),
  prior_ids = list(c("A::a", "A::b", "B::b", "A::c", "C::h", "A::d")),
  section = "A::S",
  resource = c("e, f"),
  task = c("c, d"),
  progress = c(50),
  deadline = lubridate::ymd(c("2018-09-20")),
  fixed_start_date = lubridate::ymd(c("2018-09-10")),
  fixed_end_date = lubridate::ymd(c("2018-09-16")),
  est_days = c(5),
  waiting = c(T),
  nmb_combined_entries = 2L)

d_out <- 
test_that(
  "combine ids within project in order to make them unique", {
    expect_identical(h.rd_make_id_unique_within_project(d_in), d_expected)
  }
)


d_in <- data.frame(
  project = c("A"), 
  section = NA_character_, 
  id = NA_character_, 
  depends_on = NA_character_, 
  start = NA_character_, 
  end = NA_character_, 
  resource = NA_character_, 
  task = NA_character_, 
  progress = NA, 
  deadline = NA_character_,
  some_col = "will get lost"
)
d_expected <- data.table::data.table(
  project = c("A"), 
  id = "A::UNKNOWN", 
  depends_on = NA_character_, 
  start = NA_character_, 
  prior_ids = NA_character_,
  section = "A::UNKNOWN", 
  resource = "UNKNOWN", 
  task = "UNKNOWN", 
  progress = 0, 
  deadline = lubridate::as_date(NA),
  fixed_start_date = lubridate::as_date(lubridate::now()),
  fixed_end_date = lubridate::as_date(NA),
  est_days = 1,
  waiting = FALSE,
  nmb_combined_entries = 1L
)
d_out <- h.rd_wrangle(d_in)
test_that(
  "Complete raw data wrangling. check default values",{
    expect_identical(d_out, d_expected)  
  }
)

d_in <- data.frame(
  project = c("A"), 
  section = c("0_prep"), 
  id = c("a", "a"), 
  depends_on = c("b, B::b", "b, b"), 
  start = c("2018-09-10", "2018-09-11"), 
  end = c("2018-09-20", "2018-10-25"), 
  resource = c("r1, r2", "r2"), 
  task = c("t1", "t2"), 
  progress = c(0, 50), 
  deadline = c("2018-09-23", "2018-10-01"),
  stringsAsFactors = FALSE
)
d_expected <- data.table::data.table(
  project = c("A"), 
  id = "A::a",
  depends_on = list(c("A::b", "B::b")), 
  start = NA_character_, 
  prior_ids = list(c("A::b", "B::b")),
  section = c("A::0_prep"), 
  resource = c("r1, r2"), 
  task = c("t1, t2"), 
  progress = c(25), 
  deadline = lubridate::as_date(c("2018-09-23")),
  fixed_start_date = lubridate::as_date(c("2018-09-10")), 
  fixed_end_date = lubridate::as_date(c("2018-10-25")), 
  est_days = 0,
  waiting = FALSE,
  nmb_combined_entries = 2L,
  stringsAsFactors = FALSE
)

d_out <- h.rd_wrangle(d_in)
test_that(
  "Complete raw data wrangling with combining ids",{
    expect_identical(d_out, d_expected)  
  }
)

dt <- readxl::read_xlsx("../../kaggle/xlsx_2_gantt.rep/prjplan.xlsx", sheet = "Ongoing")
# 
# futile.logger::flog.threshold(futile.logger::TRACE)
# # futile.logger::flog.threshold(futile.logger::WARN)
# 
dtt <- h.rd_wrangle(dt)
# 
# dtt <- calculate_time_lines(dtt)
