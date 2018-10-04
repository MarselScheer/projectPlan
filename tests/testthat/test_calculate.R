context("Calculate")

futile.logger::flog.threshold(futile.logger::FATAL, name = futile.logger::flog.namespace())

test_that(
  "Skip saturday and sunday",{
    expect_identical(h.turn_weekend_day_to_monday(lubridate::ymd("2018-10-06")), lubridate::ymd("2018-10-08"))
    expect_identical(h.turn_weekend_day_to_monday(lubridate::ymd("2018-10-14")), lubridate::ymd("2018-10-15"))
  }
)

test_that(
  "Weekends are excluded from a time span",{
    expect_error(h.exclude_weekends(lubridate::ymd("2018-10-06"), lubridate::ymd("2018-10-01")), regexp = "end time.*is before.*start time")
    expect_identical(h.exclude_weekends(lubridate::ymd("2018-10-06"), lubridate::ymd("2018-10-08")), lubridate::ymd("2018-10-10"))
    expect_identical(h.exclude_weekends(lubridate::ymd("2018-10-03"), lubridate::ymd("2019-02-28")), lubridate::ymd("2019-04-29"))
  }
)

test_that(
  "Calculate end times", {
    expect_identical(h.calculate_end_time(lubridate::ymd("2018-10-04"), FALSE, 2, lubridate::ymd("2018-10-06")), 
                     lubridate::ymd("2018-10-08"))
    expect_identical(h.calculate_end_time(lubridate::ymd("2018-10-04"), FALSE, 2, NA), 
                     lubridate::ymd("2018-10-08"))
    expect_identical(h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 5, NA), 
                     lubridate::ymd("2018-10-12"))
    expect_identical(h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 6, NA), 
                     lubridate::ymd("2018-10-15"))
    expect_identical(h.calculate_end_time(lubridate::ymd("2018-10-05"), FALSE, 5 + 5 + 5 + 6, NA), 
                     lubridate::ymd("2018-11-05"))
  }
)

dt <- data.frame(
  id = c("a", "b"),
  prior_ids = list(NA),
  fixed_start_date = lubridate::ymd("2018-10-05"),
  est_days = c(21L, 6),
  waiting = c(F, T)
)

dt$time_start <- NA
dt$time_start <- lubridate::as_date(dt$time_start)
dt$time_end <- NA
dt$time_end <- lubridate::as_date(dt$time_end)

dt <- data.table::data.table(dt)

debugonce(h.calculate_time_lines_at)
h.calculate_time_lines_at(dt, 1)

test_that(
  "Calculate time lines under no dependency", {
      
  }
)
