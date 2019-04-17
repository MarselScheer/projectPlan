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

now <- h.turn_weekend_day_to_monday(lubridate::as_date(lubridate::now()))
dt_in <- data.table::data.table(
  time_end = c(now - 1, now + 5, now - 1),
  waiting = c(F, T, T),
  deadline = c(lubridate::as_date(NA), now - 7, lubridate::as_date(NA))
)
dt_out <- data.table::data.table(
  time_end = c(now - 1, now + 5, now - 1),
  waiting = c(F, T, T),
  deadline = c(lubridate::as_date(NA), now - 7, now - 1)
)
h.set_deadline_for_waiting_tasks(dt_in)
test_that(
  "End time modification according to waiting status", {
    expect_identical(dt_in, dt_out)
  }
)



test_that(
  "Calculate end times", {
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-04"), 2, lubridate::ymd("2018-10-06")),
      lubridate::ymd("2018-10-08")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-04"), 2, NA),
      lubridate::ymd("2018-10-08")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), 5, NA),
      lubridate::ymd("2018-10-12")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), 6, NA),
      lubridate::ymd("2018-10-15")
    )
    expect_identical(
      h.calculate_end_time(lubridate::ymd("2018-10-05"), 5 + 5 + 5 + 6, NA),
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
  waiting = c(F, T, F),
  deadline = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::as_date(NA))
)
dt_out <- data.table::copy(dt_in)

test_that(
  "Calculate time lines under no dependency", {
    dt_out <- calculate_time_lines(dt_out)
    # expect_identical(dt_out$time_start, c(lubridate::ymd("2018-10-05"), NA))
    # expect_identical(dt_out$time_end, c(lubridate::ymd("2018-11-05"), NA))
    # h.calculate_time_lines_at(dt_out, 2)
    expect_identical(dt_out$time_start, c(lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-05"), lubridate::ymd("2018-10-05")))
    expect_identical(dt_out$time_end, c(lubridate::ymd("2018-10-08"), lubridate::ymd("2018-10-15"), lubridate::ymd("2018-11-05")))
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
  waiting = c(F, F, F),
  deadline = c(NA_character_, NA_character_, NA_character_)
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


d_in <- data.table::data.table(
  project = c("A"),
  section = c("A::s1", "A::s1"),
  aborted = c(T, T),
  time_start = c(lubridate::ymd("2019-01-20"), lubridate::ymd("2019-01-26")),
  time_end   = c(lubridate::ymd("2019-01-23"), lubridate::ymd("2019-01-27")),
  progress = c(23, 99)
)
d_out <- data.table::data.table(
  project = c("A"),
  section = c("A::s1 completed"),
  aborted = c(T),
  time_start = c(lubridate::ymd("2019-01-20")),
  time_end   = c(lubridate::ymd("2019-01-20")),
  progress = c(100),
  task = "A::s1 completed",
  waiting = F,
  resource = "collapsed"
)
test_that(
  "Collapsing when all tasks have status aborted",
  expect_identical(collapse_complete_sections(d_in), d_out)
)

d_in <- data.table::data.table(
  project = c("A", "A", "B", "B"),
  section = c("A::s1", "A::s1", "B::s1", "B::s1"),
  aborted = c(F, T, F, F),
  time_start = c(lubridate::ymd("2019-01-20"), lubridate::ymd("2019-01-26"), lubridate::ymd("2019-02-05"), lubridate::ymd("2019-01-05")),
  time_end   = c(lubridate::ymd("2019-01-23"), lubridate::ymd("2019-01-27"), lubridate::ymd("2019-02-15"), lubridate::ymd("2019-03-10")),
  deadline   = c(lubridate::as_date(NA), lubridate::as_date(NA), lubridate::ymd("2019-02-14"), lubridate::ymd("2019-03-05")),
  dist_end_to_deadline = c(difftime(NA, NA), difftime(NA, NA), -1, -5),
  progress = c(100, 99, 100, 100)
)
d_out <- data.table::data.table(
  project = c("A", "B"),
  section = c("A completed", "B completed"),
  aborted = c(F, F),
  time_start = c(lubridate::ymd("2019-01-20"), lubridate::ymd("2019-01-05")),
  time_end   = c(lubridate::ymd("2019-01-23"), lubridate::ymd("2019-03-10")),
  deadline   = c(lubridate::as_date(NA), lubridate::ymd("2019-02-14")),
  dist_end_to_deadline = c(difftime(NA, NA), -1),
  progress = c(100, 100),
  task = c("A completed", "B completed"),
  waiting = c(F, F),
  resource = "collapsed"
)
test_that(
  "Collapsing when all tasks have status aborted or are complete",
  expect_identical(collapse_complete_projects(d_in), d_out)
)


d_in <- data.table::data.table(
  project = c("A", "A", "B", "B"),
  section = c("A::s1", "A::s1", "B::s1", "B::s1"),
  aborted = c(T, F, F, F),
  time_start = c(lubridate::ymd("2019-01-20"), lubridate::ymd("2019-01-26"), lubridate::ymd("2019-02-05"), lubridate::ymd("2019-01-05")),
  time_end   = c(lubridate::ymd("2019-01-23"), lubridate::ymd("2019-01-27"), lubridate::ymd("2019-02-15"), lubridate::ymd("2019-03-10")),
  deadline   = c(lubridate::ymd("2019-01-23"), lubridate::ymd("2019-01-28"), lubridate::ymd("2019-02-14"), lubridate::ymd("2019-03-05")),
  dist_end_to_deadline = c(0, 1, -1, -5),
  progress = c(100, 99, 100, 99)
)
test_that(
  "No collapsing if section/Project is not complete", {
    expect_identical(collapse_complete_projects(d_in), d_in)
    expect_identical(collapse_complete_sections(d_in), d_in)
  }
)


test_that(
  "Error if parameter missing", {
    expect_error(collapse_section(d_in), regexp = "must be specified")
    expect_error(collapse_section(d_in, project = ""), regexp = "must be specified")
    expect_error(collapse_section(d_in, project = "A", section = "unk"), regexp = "does not exist")
    expect_error(collapse_section(d_in, project = "A", section = "unk"), regexp = "A::unk.*does not exist")
    expect_error(collapse_section(d_in, project = "C", section = "s1"), regexp = "C::s1.*does not exist")
    expect_error(h.collapse_project(d_in), regexp = "must be specified")
    expect_error(h.collapse_project(d_in, project = "C"), regexp = "does not exist")
  }
)


