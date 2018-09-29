.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Package projectPlan leverage the futile.logger-package. Use futile.logger::flog.threshold() to modify the visibility of messages.")
}

.onLoad <- function(libname, pkgname) {
  futile.logger::flog.layout(
    futile.logger::layout.format("[~l] [~t] [~n.~f] ~m"),
    name = pkgname
  )
  futile.logger::flog.threshold(futile.logger::WARN, name = pkgname)
}
