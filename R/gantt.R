#' Visualization of the project plan as a Gantt-chart stratified according to projects and sections
#'
#' The gantt-chart shows one block for every defined section and uses resources to color the rectangles.
#'
#' @param dt \code{data.table} created by \link{calculate_time_lines}
#' @param xlim limits for the x-axis
#' @param show_dependencies visualize the dependencies between the tasks as arrows
#' @param text_size size of the font
#'
#' @return ggplot2-object displaying a gantt-chart
#' @examples 
#' raw_plan <- import_xlsx(system.file("template","projects.xlsx", package = "projectPlan"))
#' pre_plan <- wrangle_raw_plan(raw_plan)
#' prj_plan <- calculate_time_lines(pre_plan)
#' gantt_by_sections(prj_plan, show_dependencies = TRUE)
#' @export
gantt_by_sections <- function(dt, xlim, show_dependencies = FALSE, text_size = 3) {
  xmin <- min(dt$time_start, na.rm = TRUE)
  xmax <- max(dt$time_end, na.rm = TRUE)

  if (missing(xlim)) {
    xlim <- c(xmin - 2, xmax + 7)
  } else {
    xlim <- lubridate::ymd(xlim)
  }

  pf <- data.table::copy(dt)
  data.table::setorderv(pf, c("section", "time_start"))

  with(NULL, pf[, y := .N:1])
  with(pf, pf[, ":="(mean_y = mean(y), min_y = min(y), max_y = max(y)), by = "section"])

  prjf <- with(pf, pf[, list(min_y = min(y), max_y = max(y)), by = "project"])

  ret <- with(
    NULL,
    h.create_gantt(pf, xlim, xmin, xmax, text_size = text_size) +
      ggplot2::geom_rect(ggplot2::aes(fill = resource)) +
      ggplot2::scale_y_continuous(breaks = pf$mean_y, labels = pf$section) +
      ggplot2::geom_rect(data = prjf, ggplot2::aes(xmin = max(xmin, xlim[1]), xmax = xmax, ymin = min_y - 0.4, ymax = max_y + 0.4), color = "blue", alpha = 0, size = 1.25) +
      h.geom_progress() +
      h.mark_completed_tasks(pf)
  )

  ret <- h.plot_status(ret, pf)
  ret <- h.plot_deadlines(ret, pf)

  if (show_dependencies) {
    arrowMatrix_section <- h.calculate_arrows(pf, xmin, xmax)
    if (!is.null(arrowMatrix_section)) {
      ret <- ret +
        with(
          NULL,
          ggplot2::geom_segment(data = arrowMatrix_section, ggplot2::aes(x = time_end_prior, y = y_prior - 0.25, xend = time_start_id + 0.25, yend = y_id - 0.25), arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), alpha = 0.5)
        )
    }
  }

  ret
}


h.one_row_for_every_dependency <- function(dt) {
  if (all(is.na(unlist(dt$depends_on)))) {
    return(NULL)
  }

  ret <- data.table::copy(dt)
  max_deps <- max(unlist(lapply(ret$depends_on, length)))

  cols <- paste0("..col", 1:max_deps)
  ret[, (cols) := data.table::transpose(ret$depends_on)]
  ret <- data.table::melt(ret, id.vars = "id", measure.vars = cols, value.name = "prior_task")

  ret <- ret[!is.na(ret$prior_task)]
  data.table::setorderv(ret, c("id", "prior_task"))
  ret$variable <- NULL
  ret
}


h.calculate_arrows <- function(sorted_dt, start, end) {
  one_row_dep <- h.one_row_for_every_dependency(sorted_dt)

  if (is.null(one_row_dep)) {
    return(NULL)
  }

  time_start <- sorted_dt[, c("id", "time_start", "y")]
  time_end <- sorted_dt[, c("id", "time_end", "y")]

  ret <- time_end[one_row_dep, on = c(id = "prior_task")]
  data.table::setnames(ret, c("id", "time_end", "y", "i.id"), c("prior_task", "time_end_prior", "y_prior", "id"))

  ret <- time_start[ret, on = "id"]
  old_cols <- c("time_start", "y")
  data.table::setnames(ret, old_cols, paste0(old_cols, "_id"))

  ret[, ":="(time_start = start, time_end = end, y = 0)]
  ret
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

  with(
    NULL,
    ggplot2::ggplot(pf, ggplot2::aes(xmin = time_start, xmax = time_end, ymin = y - 0.3, ymax = y + 0.3)) +
      ggplot2::geom_text(ggplot2::aes(x = time_end, y = y, label = task, hjust = 0), 
                         size = text_size, 
                         nudge_x = max(as.numeric((xlim[2] - xlim[1])) / 100, 1)) +
      ggplot2::geom_rect(ggplot2::aes(xmin = max(xmin, xlim[1]), xmax = xmax, ymin = min_y - 0.4, ymax = max_y + 0.4), color = "black", alpha = 0, linetype = 3) +
      ggplot2::geom_vline(xintercept = lubridate::as_date(lubridate::now()), size = 2, color = "red", alpha = 0.2) +
      ggplot2::geom_rect(data = we, ggplot2::aes(xmin = time_start, xmax = time_end + 1, ymin = 0, ymax = nrow(pf) + 1), alpha = 0.05) +
      ggplot2::coord_cartesian(xlim = xlim) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        panel.grid.minor.y = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank()
      ) +
      ggplot2::ylab("") +
      ggplot2::xlab("")
  )
}


h.geom_progress <- function() {
  with(
    NULL,
    ggplot2::geom_errorbarh(ggplot2::aes(xmin = time_start, xmax = time_start + (time_end - time_start) * progress / 100, y = y), size = 1)
  )
}

h.mark_completed_tasks <- function(pf) {
  sub <- with(NULL, pf[progress == 100])
  if (nrow(sub) == 0) {
    return(NULL)
  }
  ggplot2::geom_rect(data = sub, fill = "grey")
}

h.plot_status <- function(gp, pf) {
  sub <- data.table::copy(pf)
  # probably not the best way to reuse the existing size for the upcoming labels
  size <- gp$layers[[1]]$aes_params$size
  
  idx <- sub$unscheduled
  if (any(idx, na.rm = TRUE)) {
    gp <- gp +
      with(
        NULL,
        ggplot2::geom_label(
          # two or more rows with the same id (for instance because resources were separated by rows) would generate mutiple deadline labels
          data = dplyr::slice(dplyr::group_by(sub[idx], id), 1),
          ggplot2::aes(y = y, x = time_start, hjust = 0, vjust = 0.5), 
          label = "U", fill = "yellow1", color = "black", size = size
        )
      )
  }
  
  # TODO: for grouped tasks, this might overwrite the unscheduled-label
  idx <- sub$waiting
  if (any(idx, na.rm = TRUE)) {
    gp <- gp +
      with(
        NULL,
        ggplot2::geom_label(
          # two or more rows with the same id (for instance because resources were separated by rows) would generate mutiple deadline labels
          data = dplyr::slice(dplyr::group_by(sub[idx], id), 1),
          ggplot2::aes(y = y, x = time_start, hjust = 0, vjust = 0.5), 
          label = "A", fill = "orange1", color = "black", size = size
        )
      )
  }
  gp
}

h.plot_deadlines <- function(gp, pf) {
  sub <- data.table::copy(pf)
  
  with(NULL, sub[, due_text := paste("Ends", dist_end_to_deadline, "days before deadline", sep = " ")])
  with(NULL, sub[aborted == TRUE, dist_end_to_deadline := NA])
  
  # probably not the best way to reuse the existing size for the upcoming labels
  size <- gp$layers[[1]]$aes_params$size

  idx <- sub$dist_end_to_deadline <= 0
  if (any(idx, na.rm = TRUE)) {
    gp <- gp +
      with(
        NULL,
        ggplot2::geom_label(
          # two or more rows with the same id (for instance because resources were separated by rows) would generate mutiple deadline labels
          data = dplyr::slice(dplyr::group_by(sub[idx], id), 1),
          ggplot2::aes(y = y, x = time_start, label = due_text, hjust = 1), fill = "red3", color = "white",
          size = size
        )
      )
  }

  idx <- sub$dist_end_to_deadline > 0
  if (any(idx, na.rm = TRUE)) {
    gp <- gp +
      with(
        NULL,
        ggplot2::geom_label(
          # two or more rows with the same id (for instance because resources were separated by rows) would generate mutiple deadline labels
          data = dplyr::slice(dplyr::group_by(sub[idx], id), 1),
          ggplot2::aes(y = y, x = time_start, label = due_text, hjust = 1), fill = "green4", color = "white",
          size = size
        )
      )
  }

  idx <- !is.na(sub$deadline) & sub$progress != 100
  if (any(idx, na.rm = TRUE)) {
    today <- lubridate::as_date(lubridate::now())

    next_deadline <- min(sub$deadline[idx])
    next_deadlines_idx <- which(next_deadline == sub$deadline[idx])
    next_deadline_tasks <- paste(unique(sub$task[idx][next_deadlines_idx]), collapse = "; ")

    dist <- h.calc_dist_to_deadline(today, next_deadline)
    fill <- "red3"
    if (dist > 0) {
      fill <- "green4"
    }
    gp <- gp +
      ggplot2::geom_label(
        ggplot2::aes(y = 0, x = today, label = paste("Next deadline in\n", dist, "days", sep = " "), hjust = 1),
        fill = fill,
        color = "white",
        size = size
      ) +
      ggplot2::geom_label(
        ggplot2::aes(y = 0, x = today, label = next_deadline_tasks, hjust = 0),
        fill = fill,
        color = "white",
        size = size
      )
  }



  gp
}
