#' @useDynLib community.simulator, .registration = TRUE
NULL

compiled_rhs_args <- function(args, model, parameters, times, temperatures,
                              interpolation) {
  if (!is.loaded("cs_prepare", PACKAGE = "community.simulator")) {
    stop("Compiled RHS is unavailable. Reinstall community.simulator with a C compiler, or use simulation.ode.rhs: r.", call. = FALSE)
  }
  if (length(parameters) > .Machine$integer.max) stop("Compiled RHS parameter vector is too large.")
  .Call("cs_prepare", as.integer(length(parameters)), PACKAGE = "community.simulator")
  # Match approxfun's default sorting, NA removal and duplicate-time averaging.
  forcing <- stats::approx(times, temperatures, xout = sort(unique(times)),
                           method = interpolation, rule = 2, f = 0, ties = mean)
  args$func <- paste0("cs_", model)
  args$parms <- as.numeric(parameters)
  args$dllname <- "community.simulator"
  args$initfunc <- "cs_initmod"
  args$initforc <- "cs_initforc"
  args$forcings <- list(cbind(forcing$x, forcing$y))
  args$fcontrol <- list(method = interpolation, rule = 2, f = 0, ties = mean)
  args
}
