# The reap() function basically just defines a new environment and calls it's argument within that
# context. The sow function takes a named parameters list, and evaluates it's parameters and
# assigns to the nearest enclosing "reap" environment.
# Finally, reap() will return a list with the "natural" return value of expression it was passed as
# the first element, and then it will add named elements corresponding to the names used during the
# sow() calls. The sower() function wraps a function and collects it's output via sow().
#
# https://stackoverflow.com/questions/25706285/is-there-an-r-equivalent-to-mathematicas-sow-and-reap-functions-for-building-li
# https://stackoverflow.com/questions/33249460/getting-intermediate-results-from-nloptr-in-r/40613061



reap <- function (...) {
  expr <- substitute(...)
  REAPENV <- new.env()
  parent.env(REAPENV) <- parent.frame()
  x <- eval(expr, REAPENV)
  c(list(x), as.list(REAPENV))
}



sow <- function (...) {
  expr <- substitute(alist(...))[-1]
  for (f in rev(sys.frames())) {
    if(exists("REAPENV", envir = f)) {
      re <- get("REAPENV", envir = f)
      if (is.null(names(expr))) {
        names(expr) <- if (length(expr) == 1) {"sow"} else {letters[1:length(expr)]}
      }
      stopifnot(all(nchar(names(expr)) != 0))
      for(n in names(expr)) {
        sx <- eval(expr[[n]], parent.frame())
        cv <- if(exists(n, envir = re, inherits = FALSE)) {get(n, envir = re)} else {list()}
        if(length(cv) > 0) {
          assign(n, append(cv, sx), envir = re)
        } else {
          assign(n, sx, envir = re)
        }
      }
      break;
    }
  }
  invisible(NULL)
}



sower <- function (f, n = deparse(substitute(f))) {
  function(...) {
    x <- f(...)
    do.call("sow", setNames(list(x), n))
    x
  }
}
