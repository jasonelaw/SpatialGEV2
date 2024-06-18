#' @export
#' @importFrom broom tidy
tidy.spatialGEVfit <- function(x, effects = c("fixed", "random")){
  effects <- match.arg(effects)
  model_summary <- summary(x$report, select = effects)
  ret <- as_tibble(model_summary, rownames = "term")
  names(ret) <- c("term", "estimate", "std.error", "statistic", "p.value")
  if(identical(effects, "random")){
    ret <- ret |>
      mutate(
        term = sprintf("%s[%s]", term, rownames(x$locs_obs))
      )
  }
  ret
}
