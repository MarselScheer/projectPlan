.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package projectPlan leverage the logger-package. Use logger::log_threshold(level = logger::WARN/INFO/..., namespace = 'projectPlan') to modify the visibility of messages.")
}

.onLoad <- function(libname, pkgname) {
  pkg_logger <- logger::layout_glue_generator(format = '{node}/{pid}/{namespace}/{fn} {time} {level}: {msg}')
  logger::log_layout(pkg_logger, namespace = pkgname)
  logger::log_threshold(logger::WARN, name = pkgname)
}
