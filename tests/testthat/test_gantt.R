testthat::context("gantt")
# library(projectPlan)

futile.logger::flog.threshold(futile.logger::FATAL, name = futile.logger::flog.namespace())

d_in <- data.table::data.table(id = 1:2, depends_on = list(letters[1:3], letters[2:5]))
d_nodeps <- data.table::data.table(id = 1:2, depends_on = list(NA, NA))
d_out <-
  data.table::rbindlist(
    list(
      data.table::data.table(id = 1L, prior_task = letters[1:3]),
      data.table::data.table(id = 2L, prior_task = letters[2:5])
    )
  )

test_that(
  "rearrange in a way that every dependency has its own row", {
    expect_identical(h.one_row_for_every_dependency(d_in), d_out)
    expect_null(h.one_row_for_every_dependency(d_nodeps))
  }
)

d_in <- data.table::data.table(
  id = c("a", "b", "c"),
  depends_on = list(NA, c("a"), c("a", "b")),
  y = 1:3,
  time_start = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-02-01"), lubridate::ymd("2018-03-01")),
  time_end = c(lubridate::ymd("2018-01-11"), lubridate::ymd("2018-02-11"), lubridate::ymd("2018-03-11"))
)
d_nodeps <- data.table::data.table(
  id = c("a", "b", "c"),
  depends_on = list(NA, NA, NA),
  y = 1:3,
  time_start = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-02-01"), lubridate::ymd("2018-03-01")),
  time_end = c(lubridate::ymd("2018-01-11"), lubridate::ymd("2018-02-11"), lubridate::ymd("2018-03-11"))
)
d_out <- data.table::data.table(
  id = c("b", "c", "c"),
  time_start_id = c(lubridate::ymd("2018-02-01"), lubridate::ymd("2018-03-01"), lubridate::ymd("2018-03-01")),
  y_id = c(2L, 3L, 3L),
  prior_task = c("a", "a", "b"),
  time_end_prior = c(lubridate::ymd("2018-01-11"), lubridate::ymd("2018-01-11"), lubridate::ymd("2018-02-11")),
  y_prior = c(1L, 1L, 2L),
  time_start = c(lubridate::ymd("2017-01-01")),
  time_end = c(lubridate::ymd("2019-01-01")),
  y = 0
)

test_that(
  "For every arrow one row", {
    expect_identical(h.calculate_arrows(d_in, lubridate::ymd("2017-01-01"), lubridate::ymd("2019-01-01")), d_out)
    expect_null(h.calculate_arrows(d_nodeps, lubridate::ymd("2017-01-01"), lubridate::ymd("2019-01-01")))
  }
)


test_that(
  "For every weekend one row", {
    expect_identical(
      h.make_weekend_rows(lubridate::ymd("2018-10-12"), lubridate::ymd("2018-10-15")),
      data.table::data.table(
        y = 0,
        id = "weekend",
        time_start = lubridate::ymd("2018-10-13"),
        resource = "weekend",
        time_end = lubridate::ymd("2018-10-14")
      )
    )
    expect_identical(
      # 2018-10-14 is sunday
      h.make_weekend_rows(lubridate::ymd("2018-10-14"), lubridate::ymd("2018-10-15")),
      data.table::data.table(
        y = 0,
        id = "weekend",
        time_start = lubridate::ymd("2018-10-13"),
        resource = "weekend",
        time_end = lubridate::ymd("2018-10-14")
      )
    )
    expect_identical(
      # 2018-10-13 is saturday
      h.make_weekend_rows(lubridate::ymd("2018-10-13"), lubridate::ymd("2018-10-20")),
      data.table::data.table(
        y = 0,
        id = "weekend",
        time_start = c(lubridate::ymd("2018-10-13"), lubridate::ymd("2018-10-20")),
        resource = "weekend",
        time_end = c(lubridate::ymd("2018-10-14"), lubridate::ymd("2018-10-21"))
      )
    )
  }
)

d_in <- data.table::data.table(
  project = "A",
  section = "B",
  id = letters[1:3],
  task = letters[1:3],
  resource = letters[1:3],
  depends_on = list(NA, NA, NA),
  waiting = c(F, F, F),
  aborted = F,
  y = 1:3,
  time_start = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-02-01"), lubridate::ymd("2018-03-01")),
  time_end = c(lubridate::ymd("2018-01-11"), lubridate::ymd("2018-02-11"), lubridate::ymd("2018-03-11")),
  dist_end_to_deadline = NA,
  progress = 10 * 1:3
)

test_that(
  "Just run a plot without arrows",
  expect_s3_class(gantt_by_sections(d_in, show_dependencies = TRUE), "ggplot")
)


d_in <- data.table::data.table(
  project = "A",
  section = "B",
  aborted = F,
  id = letters[1:3],
  task = letters[1:3],
  resource = letters[1:3],
  depends_on = list(NA, "a", NA),
  waiting = c(F, F, F),
  y = 1:3,
  time_start = c(lubridate::ymd("2018-01-01"), lubridate::ymd("2018-02-01"), lubridate::ymd("2018-03-01")),
  time_end = c(lubridate::ymd("2018-01-11"), lubridate::ymd("2018-02-11"), lubridate::ymd("2018-03-11")),
  dist_end_to_deadline = NA,
  progress = 10 * 8:10
)

test_that(
  "Just run a plot with arrows",
  expect_s3_class(gantt_by_sections(d_in, show_dependencies = TRUE), "ggplot")
)

