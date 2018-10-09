#' @export
gantt_by_sections <- function(dt, xlim, show_dependencies = FALSE, text_size = 3) {
  xmin <- min(dt$time_start, na.rm = TRUE)
  xmax <- max(dt$time_end, na.rm = TRUE)

  if (missing(xlim)) {
    xlim <- c(xmin - 2, xmax)
  } else {
    xlim <- lubridate::ymd(xlim)
  }

  pf <- data.table::copy(dt)
  data.table::setorder(pf, section, time_start)
  pf[, y := .N:1]
  pf[, ":="(mean_y = mean(y), min_y = min(y), max_y = max(y)), by = "section"]

  prjf <- pf[, .(min_y = min(y), max_y = max(y)), by = "project"]

  ret <- h.create_gantt(pf, xlim, xmin, xmax, text_size = text_size) +
    geom_rect(aes(fill = resource)) +
    scale_y_continuous(breaks = pf$mean_y, labels = pf$section) +
    geom_rect(data = prjf, aes(xmin = max(xmin, xlim[1]), xmax = xmax, ymin = min_y - 0.4, ymax = max_y + 0.4), color = "blue", alpha = 0, size = 1.25) +
    h.geom_progress() +
    h.mark_completed_tasks(pf)

  # if (show_dependencies) {
  #   arrowMatrix_section <- h.calculate_arrows(pf, start, end)
  #   ret <-
  #     ret +
  #     geom_segment(data = arrowMatrix_section, aes(x = time_end_dep, y = y_dep - 0.25, xend = time_start_grp + 0.25, yend = y_grp - 0.25), arrow = arrow(length = unit(0.2, "cm")), alpha = 0.5)
  # }

  ret
}

h.calculate_arrows <- function(sorted_dt, start, end) {
  one_row_dep <- h.one_row_for_every_dependency(sorted_dt)

  one_row_dep %>%
    dplyr::left_join(dplyr::select(sorted_dt, grp, time_start, y), by = "grp") %>%
    dplyr::rename(time_start_grp = time_start, y_grp = y) %>%
    dplyr::left_join(dplyr::select(sorted_dt, grp, time_end, y), by = c("dep" = "grp")) %>%
    dplyr::rename(time_end_dep = time_end, y_dep = y) %>%
    dplyr::mutate(time_start = start, time_end = end, y = 0)
}


h.make_weekend_rows <- function(start, end) {
  wd <- lubridate::wday(start)
  if (wd %in% c(1, 7)) {
    start <- start - 2
    wd <- lubridate::wday(start)
  }

  days_till_sat <- 7 - wd
  first_sat <- start + days_till_sat

  nmb_we <- floor(as.integer(end - start) / 7)
  ret <- data.table::data.table(
    y = 0,
    id = replicate("weekend", n = nmb_we + 1),
    time_start = first_sat + (0:nmb_we) * 7,
    resource = "weekend"
  )
  # time_start is saturday, time_end is sunday
  ret$time_end <- ret$time_start + 1
  ret
}

h.create_gantt <- function(pf, xlim, xmin, xmax, text_size = 3) {
  we <- h.make_weekend_rows(xmin - 7, xmax + 7)

  pf %>%
    ggplot(aes(xmin = time_start, xmax = time_end, ymin = y - 0.3, ymax = y + 0.3)) +
    geom_text(aes(x = time_end, y = y, label = task, hjust = 0), size = text_size) +
    geom_rect(aes(xmin = max(xmin, xlim[1]), xmax = xmax, ymin = min_y - 0.4, ymax = max_y + 0.4), color = "black", alpha = 0, linetype = 3) +
    geom_vline(xintercept = lubridate::as_date(lubridate::now()), size = 2, color = "red", alpha = 0.2) +
    geom_rect(data = we, aes(xmin = time_start, xmax = time_end + 1, ymin = 0, ymax = nrow(pf)), alpha = 0.05) +
    coord_cartesian(xlim = xlim) +
    theme_minimal() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank()
    ) +
    ylab("") +
    xlab("")
}


h.geom_progress <- function() {
  geom_errorbarh(aes(xmin = time_start, xmax = time_start + (time_end - time_start) * progress / 100, y = y), size = 1)
}

h.mark_completed_tasks <- function(pf) {
  sub <- filter(pf, progress == 100)
  geom_rect(data = sub, fill = "grey")
}
