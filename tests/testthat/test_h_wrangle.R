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


h.rd_preprocess_deadline_column


# dt <- readxl::read_xlsx("../../kaggle/xlsx_2_gantt.rep/prjplan.xlsx", sheet = "Ongoing")
# 
# futile.logger::flog.threshold(futile.logger::TRACE)
# # futile.logger::flog.threshold(futile.logger::WARN)
# 
# dtt <- h.rd_wrangle(dt)
# 
# dtt <- calculate_time_lines(dtt)
