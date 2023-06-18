#' Restrict column selection when applying a function
#'
#' @description `guardrails_restrict()` takes a function `.f` and any number of
#'   [tidy selections](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html).
#'   It returns a function that selects columns from a data frame and applies
#'   `.f` to them.
#'
#'   Selections must be named because the names become arguments of the output
#'   function. They are Boolean and `TRUE` by default, so they restrict column
#'   selection unless specified otherwise.

#' @param .f Function which the output function will apply across multiple
#'   columns of a data frame.
#' @param .dplyr_verb String. Name of a dplyr verb like `"mutate"` or
#'   `"summarize"` that uses
#'   [data-masking](https://dplyr.tidyverse.org/articles/programming.html#data-masking),
#'   and therefore works with `dplyr::across()`.
#' @param .eval_f Boolean. Should `.f` appear in the output function as its body
#'   and arguments, rather than by name? Default is `TRUE`.
#' @param .default_guardrails Boolean. Should the guardrails guide selection by
#'   default (of the output function's arguments named in `...`)? Default is
#'   `TRUE`.
#' @param ...
#'   <[`tidy-select`](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html)>
#'   Any number of named tidy selections passed on to `.dplyr_verb`.
#'
#' @return A function that wraps `.dplyr_verb` to apply `.f` to any number of
#'   columns in a data frame.
#'
#' @export
#'
#' @examples
#' iris <- tibble::as_tibble(iris)
#' my_guardrails_f <- guardrails_restrict(
#'   .f = function(x) x * 100,
#'   .dplyr_verb = "mutate",
#'   check_numeric = where(is.numeric)
#' )
#' my_guardrails_f(iris)

guardrails_restrict <- function(
    .f,
    .dplyr_verb = c(
      "arrange", "count", "filter", "group_by", "mutate",
      "summarise", "summarize"
    ),
    .eval_f = TRUE,
    .default_guardrails = TRUE,
    ...) {

  conditions_quos <- rlang::enquos(...)

  if (!is.function(.f)) {
    rlang::abort(c(
      "`.f` must be a function.",
      "x" = paste0("It is of type ", typeof(.f), ".")
    ))
  }

  if (
    length(.default_guardrails) != 1L ||
    is.na(.default_guardrails) ||
    !is.logical(.default_guardrails)
  ) {
    rlang::abort("`.default_guardrails` must be `TRUE` or `FALSE`.")
  }

  # Capture the name of `.f` and, by default, make sure to evaluate `.f` itself
  # (this is copied from pairmaps):
  f_value <- substitute(.f)
  if (.eval_f) {
    f_value <- .f
  }

  rlang::arg_match(.dplyr_verb, c(
    "arrange", "count", "filter", "group_by", "mutate", "summarise", "summarize"
  ))

  condition_list <- vector("list", length = length(conditions_quos))
  condition_names <- names(conditions_quos)

  if (any(condition_names == "")) {
    rlang::abort(c(
      "All conditions must be named.",
      "i" = "The names are needed as arguments of the output function."
    ))
  }

  .dplyr_verb <- paste0("dplyr::", .dplyr_verb)
  .dplyr_verb <- rlang::parse_expr(.dplyr_verb)

  for (i in seq_along(conditions_quos)) {
    selection_var_name <- paste0("selection", i)
    condition_list[[i]] <- rlang::expr({
      !!rlang::sym(selection_var_name) <-
        if (!!rlang::sym(condition_names[i])) {
        rlang::expr(!!rlang::quo_get_expr(conditions_quos[[i]]))
      } else {
        rlang::expr(everything())
      }
    })
  }

  selection_sequence <- paste0("!!selection", seq_along(conditions_quos))
  selection_sequence <- paste(selection_sequence, collapse = " & ")
  selection_sequence <- paste("!!cols &", selection_sequence)
  selection_sequence <- rlang::parse_expr(selection_sequence)

  # In the output function, the default for each flag should be `TRUE` by
  # default of `.default_guardrails`:
  conditions_quos <- as.list(conditions_quos)
  conditions_quos[TRUE] <- .default_guardrails

  # Construct the output function from its three components (see Hadley Wickham,
  # *Advanced R*, ch. 6.2.1;
  # https://adv-r.hadley.nz/functions.html#fun-components):
  rlang::new_function(
    args = rlang::inject(rlang::pairlist2(
      data =, cols = rlang::expr(dplyr::everything()),
      !!!conditions_quos, ... =
    )),
    body = rlang::expr({
      cols <- rlang::enquo(cols)
      `!!!`(condition_list)
      `!!`(.dplyr_verb)(data, dplyr::across(
        .cols = `!!`(selection_sequence),
        .fns  = `!!`(f_value)
      ), ...)
    }),
    env = rlang::env()
  )
}
