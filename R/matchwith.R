#' Patern Matching for R
#'
#' Just like Hakell's "case of" or OCaml's "match with" but not support guard syntax.
#' @description Only supported those functionalities. Guard is still under consideration, not yet implemented.
#' \itemize{
#'  \item Constatnt Pattern (like 1, "1", NULL as R's atomic expression)
#'  \item Variable Pattern (just a symbol: x)
#'  \item Cons Pattern (x::xs)
#'  \item Tuple Pattern (VECSXP pattern in R)
#'  \item Wildcard Pattern (., _, otherwise)
#' }
#'
#' There are three Wildcard Symbol, '.', '_', and `otherwise'.
#' You can use one of them in the bottom part of arguments of 'match_with'.
#' @param ... The first (actual) argument of ... is
#' @name match_with
#' @examples
#' # Syntax
#' # f <- function(expr) {
#' #   match_with(expr
#' #   , pattern_1 -> res_1
#' #   , pattern_2 -> res_2
#' #              ...
#' #   , pattern_n -> res_n
#' #   )
#' # }
#'
#' fib <- function(n) {
#'   match_with(n
#'   , 0 -> 0
#'   , 1 -> 1
#'   , . -> fib(n - 1) + fib(n - 2)
#'   )
#' }
#' fib(10)
#'
#' fizzbuzz <- function(z) {
#'   match_with(list(z %% 5, z %% 3)
#'   , list(0, 0) -> "FizzBuzz"
#'   , list(0, .) -> "Fizz"
#'   , list(., 0) -> "Buzz"
#'   , otherwise  -> as.character(z)
#'   )
#' }
#' sapply(1:30, fizzbuzz)
#'
#' # compare with Haskell's definition
#' # https://wiki.haskell.org/Fold
#' # Note:
#' # If lst is R's list (VECSXP), `length(lst) == 0` can be replaced with `list()`.
#' # If lst is R's integer vector (INTSXP), `length(lst) == 0` can be replaced with `integer(0)`.
#' # If lst is R's numeric vector (REALSXP), `length(lst) == 0` can be replaced with `numeric(0)`.
#' foldr <- function(f, init, lst) {
#'   match_with(lst
#'   , length(lst) == 0 -> init
#'   , x::xs            -> f(x, foldr(f, init, xs))
#'   )
#' }
#'
#' foldl <- function(f, init, lst) {
#'   match_with(lst
#'   , length(lst) == 0 -> init
#'   , x::xs            -> foldl(f, f(init, x), xs)
#'   )
#' }
#' foldr(function(x, y) paste0("(", x, "+", y, ")"), "0", as.character(1:13))
#' foldl(function(x, y) paste0("(", x, "+", y, ")"), "0", as.character(1:13))
#'
#' len <- function(xs) {
#'   match_with(xs
#'   , length(xs) == 0 -> 0
#'   , y::ys           -> 1 + len(ys)
#'   )
#' }
#' len(c(10, 11, 12))
#' len(list(10, 11, 12))
NULL

#' @rdname match_with
#' @export
match_with <- (function() {
  wildcards <- c(quote(.), quote(`_`), quote(otherwise))
  wildcards_char <- lapply(wildcards, as.character)

  equals_recursive <- function(c1, c2, wildcard = NULL, strict_int_dbl= FALSE) {
    if (!is.null(wildcard) && !is.language(c2)) {
      stop("when using a wildcard, c2 must be a call or a symbol") }

    # coerce characters into symbols
    if (!is.null(wildcard) && any(is.character(wildcard))) {
      wildcard <- lapply(wildcard, as.symbol) }

    # shortcut without a wildcard
    if (is.null(wildcard) && strict_int_dbl) {
      return(identical(c1, c2)) }
    if (is.null(wildcard) && !strict_int_dbl && xor(is.numeric(c1), is.numeric(c2))) {
      return(FALSE) }

    # c2 may have wildcard symbol
    out_fun <- function(c1, c2) {
      ## When using `==`, symbols are coerced into character by
      ## PRINTNAME(x) in C lang level (See next source code:
      ## https://github.com/wch/r-source/blob/trunk/src/main/relop.c#L185 )
      ## and comparison between a symbol and list of symbols may
      ## mistake in a special situation: quote(`_`) == list(quote(`_`), "_").
      ## The result in above is c(FALSE, TRUE).
      c2_has_wildcard <-
        !is.null(wildcard) && is.symbol(c2) &&
        (as.character(c2) %in% wildcards_char)

      if (c2_has_wildcard) TRUE
      else if (length(c1) != length(c2)) FALSE
      else if (length(c1) == 0) identical(c1, c2)
      else if (length(c1) == 1) {
        if (!strict_int_dbl && is.numeric(c1) && is.numeric(c2)) {
          isTRUE(as.double(c1) == as.double(c2)) }
        else {
          identical(c1, c2) }}
      else {
        for(i in seq_along(c1)) {
          if (!out_fun(c1[[i]], c2[[i]])) return(FALSE) }
        return(TRUE) }
    }
    out_fun(c1, c2)
  }

  # extract_patterns

  check_matched <- function(expr_info, parent_frame, l_expr, r_expr) {
    l_expr_len <- length(l_expr)
    has_wildcard <- has_wc(l_expr)
    cons_pattern <- l_expr_len == 3 && l_expr[[1]] == "::"

    if (l_expr_len == 1 && is.atomic(l_expr)) { # Constant Pattern
      if (equals_recursive(l_expr, expr_info$value)) {
        list(is_matched = TRUE, matched_value = eval(r_expr, parent_frame))
      } else {
        list(is_matched = FALSE, matched_value = quote(Nothing))
      }
    } else if (l_expr_len == 1 && is.symbol(l_expr)) { # Variable Pattern or Wildcard Pattern
      if (has_wildcard) {
        list(is_matched = TRUE, matched_value = eval(r_expr, parent_frame))
     } else {
        list(is_matched = TRUE, matched_value = eval(r_expr, setNames(list(expr_info$value), as.character(l_expr)), parent_frame))
      }
    } else if (cons_pattern) {
      if (is.symbol(l_expr[[2]]) && is.symbol(l_expr[[3]]))  {
        list(is_matched = TRUE, matched_value = match_hdtl(expr_info$value, l_expr, r_expr, parent_frame))
      } else {
        stop("pattern of `x::xs` is only acceptable. `x::y::yss` is not supported")
      }
    } else if (l_expr_len > 1 && # integer(0) shold be matched hear?
               equals_recursive(expr_info$value_deparse(), l_expr, if (has_wildcard) wildcards else NULL)) {
      # tuple pattern
      list(is_matched = TRUE, matched_value = eval(r_expr, parent_frame))
    } else if (isTRUE(eval(l_expr, parent_frame))) {
      # Guard for test
      list(is_matched = TRUE, matched_value = eval(r_expr, parent_frame))
    } else{
      #print("last if")
      list(is_matched = FALSE, matched_value = quote(Nothing))
    }
  }

  match_hdtl <- function(expr_value, l_expr, r_expr, env_) {
    lst <- `names<-`(
      list(expr_value[[1]], expr_value[-1]),
      list(as.character(l_expr[[2]]), as.character(l_expr[[3]]))
    )
    eval(r_expr, lst, env_)
  }
  # not exported

  has_wc <- function(call_) {
    any(all.vars(call_) %in% wildcards_char)
  }

  ## main
  function(...) {
    dots <- as.vector(substitute((...)), "list")[-1]
    conds <- dots[-1]
    parent_frame <- parent.frame()
    expr <- dots[[1]]
    expr_value <- eval(expr, parent_frame)
    expr_name <- names(dots[1])
    expr_value_deparse <- parse(text = deparse(expr_value))[[1]]
    delayedAssign("expr_value_deparse", parse(text = deparse(expr_value))[[1]])

    expr_info <- list(expr = expr, value = expr_value, name = expr_name, value_deparse = function() expr_value_deparse)

    for (i in seq_along(conds)) {
      statement <- conds[[i]]
      if (missing(statement))
        stop("need to remove the last comma")

      if (statement[[1]] != quote(`<-`))
        stop("use `->` as converter")

      l_expr <- statement[[3]]
      r_expr <- statement[[2]]

      ans_info <- check_matched(expr_info, parent_frame, l_expr, r_expr)

      if (ans_info$is_matched) {
        return(ans_info$matched_value)
      }

      # pattern_atomic <- is.atomic(l_expr)
      # pattern_cons <- length(l_expr) == 3 && l_expr[[1]] == "::"
      # pattern_tuple <- length(l_expr) > 1 && l_expr[[1]] == "list"
      # pattern_wc <- length(l_expr) == 1 && any(wildcards_char %in% cond_vars)
      # cond_vars <- if (pattern_atomic) "" else all.vars(l_expr)
      # is_wildcard <- length(l_expr) == 1 && any(wildcards_char %in% cond_vars)
      #
      # if (is_wildcard && i != length(conds)) {
      #   stop("wildcard must be last part of arguments") }
      #
      # has_wildcard_in_expr <- any(wildcards_char %in% cond_vars)
      #
      # symbol_is_referred <- is.symbol(expr) &&
      #   (as.character(expr) %in% cond_vars) && !has_wildcard_in_expr
      #
      # name_is_referred <- !is.null(expr_name) &&
      #   (expr_name %in% cond_vars) && !has_wildcard_in_expr
      #
      # eval_arg_2nd <-
      #   if (pattern_cons) {
      #     `names<-`(
      #       list(expr_value[[1]], expr_value[-1]),
      #       list(as.character(l_expr[[2]]), as.character(l_expr[[3]]))
      #     ) }
      #   else if (name_is_referred) {
      #     `names<-`(list(expr_value), list(expr_name)) }
      #   else if (!pattern_atomic && l_expr[[1]] == "list" && !has_wildcard_in_expr) {
      #     # list(0, x) list(y, x)
      #     NULL }
      #   else {
      #     NULL }
      #
      # ans <- eval(statement[[2]], envir = eval_arg_2nd, enclos = parent_frame)
      #
      # #
      # if (is_wildcard || pattern_cons) {
      #   return(ans) } # `wildcard`, x::xs
      # else if ((symbol_is_referred || name_is_referred) && eval(l_expr, eval_arg_2nd, parent_frame)) {
      #   return(ans) } # is.null(x), x %% 2 == 0
      # else if (pattern_atomic && equals_recursive(expr_value, l_expr)) {
      #   return(ans) } # 1, "", NULL
      # else if (!pattern_atomic && !pattern_cons && !has_wildcard_in_expr && equals_recursive(expr_value, eval(l_expr, parent_frame))) {
      #   return(ans) } # list(1,2), numeric(0)
      # else if (!pattern_atomic && has_wildcard_in_expr && equals_recursive(expr_value_deparse, l_expr, wildcard = wildcards)) {
      #   return(ans) } # list(1,.),
      # else if (!pattern_atomic && l_expr[[1]] == "list")
      #   # # list(x, y)
      # else {
      #   # not matched in this loop
      # }
    }
    stop("The input is non-matched pattern. Need to write proper
            syntax or set default wildcard `.` at last.")
  }
})()
