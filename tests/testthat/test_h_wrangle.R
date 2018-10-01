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

d_expected <- dplyr::tibble(
  project = "A", id = "A::I", section = "A::S",
  depends_on = list(c("A::a", "B::b")), 
  start = list(c("A::b", "B::b", "A::c", "C::h", "A::d")),
  task = c("c, d"),
  resource = c("e, f"),
  progress = c(50),
  deadline = lubridate::ymd(c("2018-09-20")),
  fixed_start_date = lubridate::ymd(c("2018-09-10")),
  fixed_end_date = lubridate::ymd(c("2018-09-16")),
  est_days = c(5),
  waiting = c(T),
  prior_ids = list(c("A::a", "A::b", "B::b", "A::c", "C::h", "A::d")),
  nmb_combined_entries = 2L)

d_out <- h.rd_make_id_unique_within_project(d_in)
test_that(
  "combine ids within project in order to make them unique", {
    expect_identical(d_out$depends_on, d_expected$depends_on)
    expect_identical(d_out$start, d_expected$start)
    expect_identical(d_out$prior_ids, d_expected$prior_ids)
    d_out$depends_on <- NULL
    d_out$start <- NULL
    d_out$prior_ids <- NULL
    d_expected$depends_on <- NULL
    d_expected$start <- NULL
    d_expected$prior_ids <- NULL
    expect_identical(d_out, d_expected)
  }
)





# dt <- readxl::read_xlsx("../../kaggle/xlsx_2_gantt.rep/prjplan.xlsx", sheet = "Ongoing")
# 
# futile.logger::flog.threshold(futile.logger::TRACE)
# # futile.logger::flog.threshold(futile.logger::WARN)
# 
# dtt <- h.rd_wrangle(dt)
# 
# dtt <- calculate_time_lines(dtt)
