#' Construct strings with color
#'
#' @description
#' The [crayon][crayon::crayon] package defines a number of functions used to
#' color terminal output. `glue_col()` and `glue_data_col()` functions provide
#' additional syntax to make using these functions in glue strings easier.
#'
#' Using the following syntax will apply the function [crayon::blue()] to the text 'foo bar'.
#'
#' ```
#' {blue foo bar}
#' ```
#'
#' If you want an expression to be evaluated, simply place that in a normal brace
#' expression (these can be nested).
#'
#' ```
#' {blue 1 + 1 = {1 + 1}}
#' ```
#'
#' If the text you want to color contains, e.g., an unpaired quote or a comment
#' character, specify `.literal = TRUE`.
#'
#' @inheritParams glue
#' @inherit glue return
#' @export
#' @examplesIf require(crayon)
#' library(crayon)
#'
#' glue_col("{blue foo bar}")
#'
#' glue_col("{blue 1 + 1 = {1 + 1}}")
#'
#' glue_col("{blue 2 + 2 = {green {2 + 2}}}")
#'
#' white_on_black <- bgBlack $ white
#' glue_col("{white_on_black
#'   Roses are {red {colors()[[552]]}},
#'   Violets are {blue {colors()[[26]]}},
#'   `glue_col()` can show \\
#'   {red c}{yellow o}{green l}{cyan o}{blue r}{magenta s}
#'   and {bold bold} and {underline underline} too!
#' }")
#'
#' # this would error due to an unterminated quote, if we did not specify
#' # `.literal = TRUE`
#' glue_col("{yellow It's} happening!", .literal = TRUE)
#'
#' # `.literal = TRUE` also prevents an error here due to the `#` comment
#' glue_col(
#'   "A URL: {magenta https://github.com/tidyverse/glue#readme}",
#'   .literal = TRUE
#' )
#'
#' # `.literal = TRUE` does NOT prevent evaluation
#' x <- "world"
#' y <- "day"
#' glue_col("hello {x}! {green it's a new {y}!}", .literal = TRUE)
glue_col <- function(
  ...,
  .envir = parent.frame(),
  .na = "NA",
  .literal = FALSE
) {
  glue(
    ...,
    .envir = .envir,
    .na = .na,
    .literal = .literal,
    .transformer = color_transformer
  )
}