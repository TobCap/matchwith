#' Patern Matching for R
#'
#' @param ... the first (actual) argument is
#' @name match_with
#' @exmaples
#' fib <- function(n)
#'   match_with(n
#'   , 0 -> 0
#'   , 1 -> 1
#'   , . -> fib(n - 1) + fib(n - 2)
#'   )
#' fib(10)
NULL

#' @rdname match_with
#' @export
match_with <- function(...) {
  dots <- as.vector(substitute((...)), "list")[-1]
  conds <- dots[-1]
  parent_frame <- parent.frame()
  expr <- dots[[1]]
  expr_value <- eval(expr, parent_frame)
  expr_name <- names(dots[1])
  # expr_value_deparse <- parse(text = deparse(expr_value))[[1]]
  delayedAssign("expr_value_deparse", parse(text = deparse(expr_value))[[1]])

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
      ## deparse() in C lang level (See R source's relop.c#80-81)
      ## and comparison between a symbol and list of symbols may
      ## mistake in a special situation: quote(`_`) == list(quote(`_`), "_")
      ## The result in above is c(FALSE, TRUE).
      c2_is_wildcard <-
        !is.null(wildcard) && is.symbol(c2) &&
        (as.character(c2) %in% wildcards_char)

      if (c2_is_wildcard) TRUE
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

  for(i in seq_along(conds)) {
    statement <- conds[[i]]

    if (missing(statement))
      stop("need to remove the last comma")

    if (statement[[1]] != quote(`<-`))
      stop("use `->` as converter")

    cond_expr <- statement[[3]]
    cond_is_atomic <- is.atomic(cond_expr)
    cond_vars <- if (cond_is_atomic) "" else all.vars(cond_expr)
    is_wildcard <- length(cond_expr) == 1 && any(wildcards_char %in% cond_vars)

    if (is_wildcard && i != length(conds)) {
      stop("wildcard must be last part of arguments") }

    has_wildcard_in_expr <- any(wildcards_char %in% cond_vars)
    has_double_colon <- length(cond_expr) == 3 && quote(`::`) == cond_expr[[1]]

    symbol_is_referred <- is.symbol(expr) &&
      (as.character(expr) %in% cond_vars) && !has_wildcard_in_expr

    name_is_referred <- !is.null(expr_name) &&
      (expr_name %in% cond_vars) && !has_wildcard_in_expr

    eval_arg_2nd <-
      if (has_double_colon) {
        `names<-`(
          list(expr_value[[1]], expr_value[-1]),
          list(as.character(cond_expr[[2]]), as.character(cond_expr[[3]])) ) }
    else if (name_is_referred) {
      `names<-`(list(expr_value), list(expr_name)) }
    else NULL

    ans <- eval(statement[[2]], envir = eval_arg_2nd, enclos = parent_frame)

    #
    if (is_wildcard || has_double_colon) {
      return(ans) } # `wildcard`, x::xs
    else if ((symbol_is_referred || name_is_referred) && eval(cond_expr, eval_arg_2nd, parent_frame)) {
      return(ans) } # is.null(x), x %% 2 == 0
    else if (cond_is_atomic && equals_recursive(expr_value, cond_expr)) {
      return(ans) } # 1, "", NULL
    else if (!cond_is_atomic && !has_double_colon && !has_wildcard_in_expr && equals_recursive(expr_value, eval(cond_expr, parent_frame))) {
      return(ans) } # list(1,2), numeric(0)
    else if (!cond_is_atomic && has_wildcard_in_expr && equals_recursive(expr_value_deparse, cond_expr, wildcard = wildcards)) {
      return(ans) } # list(1,.),
    else {
      # not matched in this loop
    }
  }
  stop("The input is non-matched pattern. Need to write proper
          syntax or set default wildcard `.` at last.")
}
