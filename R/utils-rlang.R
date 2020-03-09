#' Tidy eval helpers
#'
#' @description
#'
#' * \code{\link[rlang:nse-defuse]{enquo}()} delays the execution of one or
#'   several function arguments. \code{enquo()} returns a single quoted
#'   expression, which is like a blueprint for the delayed computation.
#'
#' * \code{\link[rlang:nse-defuse]{expr}()} quotes a new expression _locally_. It
#'   is mostly useful to build new expressions around arguments
#'   captured with [enquo()] or [enquos()]:
#'   \code{expr(mean(!!enquo(arg), na.rm = TRUE))}.
#'
#' * \code{\link[rlang]{as_name}()} transforms a quoted variable name
#'   into a string. Supplying something else than a quoted variable
#'   name is an error.
#'
#'   \code{\link[rlang]{as_label}()} transforms any kind of R object,
#'   including quoted function calls and vectors, into a string.
#'   Its purpose is to summarise that object into a single label.
#'   That label is often suitable as a default name.
#'
#'   If you don't know what a quoted expression contains (for instance
#'   expressions captured with \code{enquo()} could be a variable
#'   name, a call to a function, or an unquoted constant), then use
#'   \code{as_label()}.
#'
#' To learn more about tidy eval and how to use these tools, visit
#' \url{https://tidyeval.tidyverse.org} and the
#' \href{https://adv-r.hadley.nz/metaprogramming.html}{Metaprogramming
#' section} of \href{https://adv-r.hadley.nz}{Advanced R}.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang enquo .data := as_label
#' @aliases enquo .data := as_label
#' @export enquo .data := as_label
NULL
