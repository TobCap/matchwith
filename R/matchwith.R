#' Patern Matching for R
#'
#' Just like Hakell's "case of" or OCaml's "match with" but not support guard syntax.
#' @description Only supports those functionalities. Guard is still under consideration, not yet completely implemented.
#' \itemize{
#'  \item Constatnt Pattern (like 1, "1", NULL as R's atomic expression)
#'  \item Cons Pattern (x::xs)
#'  \item Tuple Pattern with matching symbols (VECSXP is used instead of Tuple)
#'  \item Wildcard Pattern (., _, otherwise)
#'  \item Guard clauses (when using one of getGroupMembers("Compare"), `!`, any, all, identical, and isTRUE)
#' }
#'
#' There are three Wildcard Symbol, '.', '_', and `otherwise'.
#' You can use one of them in the bottom part of arguments of 'match_with'.
#' @param ... The first (actual) argument of ... is trying to match following patterns.
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
#'   match_with(list(z %% 3, z %% 5)
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
#'
#' \dontrun{
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
#' }
NULL

#' @rdname match_with
#' @export
match_with <- (function() {
  wildcards <- list(quote(.), quote(`_`), quote(otherwise))
  wildcards_char <- lapply(wildcards, as.character)
  bool_funs <- c(
    "!", "any", "all", "identical", "isTRUE",
    getGroupMembers("Compare"),
    getGroupMembers("Logic"), # for warning
    ls(pattern = "^is\\.", envir = baseenv())
    )
  not_matched <- list(is_matched = FALSE, new_list = NULL)

   # extract_patterns
  check_matched <- function(expr_info, parent_frame, l_expr, r_expr) {
    l_expr_len <- length(l_expr)
    cons_pattern <- l_expr_len == 3 && l_expr[[1]] == "::"
    is_guard <- l_expr_len > 1 && any(as.character(l_expr[[1]]) %in% bool_funs)

    # return list(is_matched = LGLSXP, new_list = VECSXP)
    if (cons_pattern) {
      if (is.symbol(l_expr[[2]]) && is.symbol(l_expr[[3]]))  {
        list(is_matched = TRUE, new_list = match_hdtl(expr_info$value, l_expr, r_expr))
      } else {
        stop("pattern of `x::xs` is only acceptable. `x::y::ys` is not supported")
      }
    } else if (is_guard) {
      if (as.character(l_expr[[1]]) %in% c("|", "&")) {
        warning("`&` or `|` require to use all() or any()", domain = NA)
      }

      if (eval(l_expr, parent_frame)) {
        list(is_matched = TRUE, new_list = NULL)
      } else {
        list(is_matched = FALSE, new_list = NULL)
      }
    } else if ({.m <- match_var(l_expr, expr_info$value_deparse()); .m[[1]]}) {
      list(is_matched = TRUE, new_list = .m[[2]])
   } else {
      list(is_matched = FALSE, new_list = NULL)
    }
  }

  match_hdtl <- function(expr_value, l_expr, r_expr) {
    `names<-`(
      list(expr_value[[1]], expr_value[-1]),
      list(as.character(l_expr[[2]]), as.character(l_expr[[3]]))
    )
  }
  # not exported

  match_var <- function(l_expr, expr_orig, is_head = FALSE, acc = list()) {
    # returns list of a result of matched and pairs of symbol and value
    if (is.symbol(l_expr)) {
      if (has_wc(l_expr)) list(TRUE, acc)
      else if (!is_head) list(TRUE, c(setNames(list(expr_orig), as.character(l_expr)), acc))
      else if (identical(l_expr, expr_orig)) list(TRUE, acc)
      else list(FALSE, NULL) }
    else if (length(l_expr) != length(expr_orig)) list(FALSE, NULL)
    else if (length(l_expr) == 0) list(identical(l_expr, expr_orig), acc) # for NULL
    else if (is.atomic(l_expr)) list(isTRUE(l_expr == expr_orig), acc) # absorbs difference of numeric and integer
    else if (is.recursive(l_expr) && is.recursive(expr_orig)) {
      hd <- match_var(l_expr[[1]], expr_orig[[1]], is.call(l_expr) && is.symbol(l_expr[[1]]), acc)
      tl <- match_var(as.list(l_expr[-1]), as.list(expr_orig[-1]), FALSE, acc)
      list(hd[[1]] && tl[[1]], c(hd[[2]], tl[[2]])) }
    else list(FALSE, NULL)
  }

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
    # expr_name <- names(dots[1])
    # expr_value_deparse <- parse(text = deparse(expr_value))[[1]]
    delayedAssign("expr_value_deparse", parse(text = deparse(expr_value))[[1]])

    expr_info <- list(expr = expr, value = expr_value, value_deparse = function() expr_value_deparse)

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
        return(eval(r_expr, ans_info$new_list, parent_frame))
      }
    }
    stop("The input is non-matched pattern. Need to write proper
            syntax or set default wildcard `.` at last.")
  }
})()
