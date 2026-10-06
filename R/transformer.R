#' Parse and Evaluate R code
#'
#' This is a simple wrapper around `eval(parse())`, used as the default
#' transformer.
#' @param text Text (typically) R code to parse and evaluate.
#' @param envir environment to evaluate the code in
#' @seealso `vignette("transformers", "glue")` for documentation on creating
#'   custom glue transformers and some common use cases.
#' @export
identity_transformer <- function(text, envir = parent.frame()) {
  with_glue_error(
    expr <- parse(text = text, keep.source = FALSE),
    "Failed to parse glue component"
  )
  with_glue_error(
    eval(expr, envir),
    paste0("Failed to evaluate glue component {", text, "}")
  )
}

with_glue_error <- function(expr, message) {
  if (!requireNamespace("rlang", quietly = TRUE)) {
    return(expr)
  }

  withCallingHandlers(
    expr,
    error = function(cnd) {
      rlang::abort(
        message,
        parent = cnd,
        call = NULL
      )
    }
  )
}



#' @rdname glue_col
#' @export
glue_data_col <- function(
  .x,
  ...,
  .envir = parent.frame(),
  .na = "NA",
  .literal = FALSE
) {
  glue_data(
    .x,
    ...,
    .envir = .envir,
    .na = .na,
    .literal = .literal,
    .transformer = color_transformer
  )
}

color_transformer <- function(code, envir) {
  res <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) {
    e
  })
  if (!inherits(res, "error")) {
    return(eval(res, envir = envir))
  }

  code <- glue_collapse(code, "\n")
  m <- regexpr("(?s)^([[:alnum:]_]+)[[:space:]]+(.+)", code, perl = TRUE)
  has_match <- m != -1
  if (!has_match) {
    stop(res)
  }
  starts <- attr(m, "capture.start")
  ends <- starts + attr(m, "capture.length") - 1L
  captures <- substring(code, starts, ends)
  fun <- captures[[1]]
  text <- captures[[2]]
  out <- glue(text, .envir = envir, .transformer = color_transformer)

  color_fun <- get0(fun, envir = envir, mode = "function")
  if (is.null(color_fun) && requireNamespace("crayon", quietly = TRUE)) {
    color_fun <- get0(fun, envir = asNamespace("crayon"), mode = "function")
  }

  if (is.null(color_fun)) {
    # let nature take its course, i.e. throw the usual error
    get(fun, envir = envir, mode = "function")
  } else {
    color_fun(out)
  }
}
