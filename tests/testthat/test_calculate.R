testthat::context("Calculate")

futile.logger::flog.threshold(futile.logger::FATAL, name = futile.logger::flog.namespace())

test_that(
  "Skip saturday and sunday", {
    expect_identical(h.turn_weekend_day_to_monday(lubridate::ymd("2018-10-06")), lubridate::ymd("2018-10-08"))
    expect_identical(h.turn_weekend_day_to_monday(lubridate::ymd("2018-10-14")), lubridate::ymd("2018-10-15"))
  }
)

test_that(
  "Weekends are excluded from a time span", {
    expect_error(h.exclude_weekends(lubridate::ymd("2018-10-06"), lubridate::ymd("2018-10-01")), regexp = "end time.*is before.*start time")
    expect_identical(h.exclude_weekends(lubridate::ymd("2018-10-06"), lubridate::ymd("2018-10-08")), lubridate::ymd("2018-10-10"))
    expect_identical(h.exclude_weekends(lubridate::ymd("2018-10-03"), lubridate::ymd("2019-02-28")), lubridate::ymd("2019-04-29"))
  }
)

test_that(
  "Calculate end times", {
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-04"), FALSE, 2, lubridate::ymd("2018-10-06")),
      lubridate::ymd("2018-10-08")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-04"), FALSE, 2, NA),
      lubridate::ymd("2018-10-08")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 5, NA),
      lubridate::ymd("2018-10-12")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 6, NA),
      lubridate::ymd("2018-10-15")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 5 + 5 + 5 + 6, NA),
      lubridate::ymd("2018-11-05")
    )
  }
)

dt_in <- data.table::data.table(
  id = c("a", "b", "c"),
  prior_ids = list(NA),
  fixed_start_date = lubridate::ymd("2018-10-05"),
  fixed_end_date = c(lubridate::ymd("2018-10-07"), lubridate::as_date(NA), lubridate::as_date(NA)),
  est_days = c(21L, 6, 21L),
  waiting = c(F, T, F)
)
dt_out <- data.table::copy(dt_in)

test_that(
  "Calculate time lines under no dependency", {
    dt_out <- calculate_time_lines(dt_out)
    # expect_identical(dt_out$time_start, c(lubridate::ymd("2018-10-05"), NA))
    # expect_identical(dt_out$time_end, c(lubridate::ymd("2018-11-05"), NA))
    # h.calculate_time_lines_at(dt_out, 2)
    expect_identical(dt_out$time_start, c(lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-05")))
    expect_identical(dt_out$time_end, c(lubridate::ymd("2018-10-08"), h.turn_weekend_day_to_monday(lubridate::as_date(lubridate::now())), lubridate::ymd("2018-11-05")))
  }
)

dt_in <- data.table::data.table(
  id = c("a", "b", "c"),
  prior_ids = list(NA, NA, c("a", "b")),
  fixed_start_date = c(lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-02"), lubridate::as_date(NA)),
  fixed_end_date = lubridate::as_date(NA),
  time_start = lubridate::as_date(NA),
  time_end = lubridate::as_date(NA),
  est_days = c(1, 6, 2),
  waiting = c(F, F, F)
)
dt_out <- data.table::copy(dt_in)

start_expected <- c(lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-02"), lubridate::ymd("2018-10-10"))
end_expected <- c(lubridate::ymd("2018-10-08"), lubridate::ymd("2018-10-10"), lubridate::ymd("2018-10-12"))
test_that(
  "Calculate time lines under first order dependency", {
    h.calculate_time_lines_at(dt_out, 3)
    expect_identical(dt_out$time_start, start_expected)
    expect_identical(dt_out$time_end, end_expected)
  }
)

dt_in <- dt_in[3:1]
dt_out <- data.table::copy(dt_in)

test_that(
  "Calculate time lines under first order dependency with non sorted entries", {
    h.calculate_time_lines_at(dt_out, 1)
    expect_identical(dt_out$time_start, start_expected[3:1])
    expect_identical(dt_out$time_end, end_expected[3:1])
  }
)


dt_in <- data.table::data.table(
  id = c("a", "b", "c"),
  prior_ids = list("b", "c", NA),
  fixed_start_date = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::ymd("2018-10-02")),
  fixed_end_date = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::ymd("2018-10-06")),
  time_start = lubridate::as_date(NA),
  time_end = lubridate::as_date(NA),
  est_days = c(1, 6, 2),
  waiting = c(F, F, F)
)
dt_out <- data.table::copy(dt_in)

start_expected <- c(lubridate::ymd("2018-10-16"), lubridate::ymd("2018-10-08"), lubridate::ymd("2018-10-02"))
end_expected <- c(lubridate::ymd("2018-10-17"), lubridate::ymd("2018-10-16"), lubridate::ymd("2018-10-08"))
test_that(
  "Calculate time lines under second order dependency with non sorted entries", {
    h.calculate_time_lines_at(dt_out, 1)
    expect_identical(dt_out$time_start, start_expected)
    expect_identical(dt_out$time_end, end_expected)
  }
)

dt_in <- data.table::data.table(
  id = c("a", "b", "c"),
  prior_ids = list("c", "c", NA),
  fixed_start_date = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::ymd("2018-10-02")),
  fixed_end_date = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::ymd("2018-10-06")),
  est_days = c(1, 6, 2),
  waiting = c(F, F, F)
)
dt_out <- data.table::copy(dt_in)

start_expected <- c(lubridate::ymd("2018-10-08"), lubridate::ymd("2018-10-08"), lubridate::ymd("2018-10-02"))
end_expected <- c(lubridate::ymd("2018-10-09"), lubridate::ymd("2018-10-16"), lubridate::ymd("2018-10-08"))
test_that(
  "Calculate time lines, multiple entries depend on one entrie", {
    dt_out <- calculate_time_lines(dt_out)
    expect_identical(dt_out$time_start, start_expected)
    expect_identical(dt_out$time_end, end_expected)
  }
)

d_in <- data.table::data.table(
  time_end = c(lubridate::ymd("2018-11-20"), lubridate::ymd("2018-10-19")),
  deadline = c(lubridate::ymd("2018-11-23"), NA)
)
d_out <- data.table::copy(d_in)
d_expected <- data.table::copy(d_out)
d_expected$dist_end_to_deadline <- lubridate::as.difftime(c(3, NA), units = "days")
h.calc_end_to_deadline(d_out)


test_that(
  "Distance to deadline", {
    expect_identical(
      h.calc_dist_to_deadline(
        c(lubridate::ymd("2018-11-20"), lubridate::ymd("2018-10-19"), lubridate::ymd("2018-10-18"), lubridate::ymd("2018-10-22"), lubridate::ymd("2018-11-02")),
        c(lubridate::ymd("2018-11-23"), lubridate::ymd("2018-10-22"), lubridate::ymd("2018-11-02"), lubridate::ymd("2018-10-19"), lubridate::ymd("2018-10-18"))
      ),
      lubridate::as.difftime(c(3, 1, 11, -1, -11), units = "days")
    )
    expect_identical(d_out, d_expected)
  }
)
