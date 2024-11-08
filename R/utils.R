
str_sub2 = function(chr_, start_, stop_ = nchar(chr_)) {
  substr(chr_, start_, stop_)
}

replacePlusSign = function(text_) {
  gsub("\\+", "+\n", text_)
}

appendWithName = function(lst_, ...) {

  lst_appending_ = list(...)
  for (i_ in seq_along(lst_appending_)) {

    name_ = names(lst_appending_)[[i_]]
    value_ = lst_appending_[[i_]]

    lst_[[name_]] = value_

  }

  return(lst_)

}

run_gently = function(expr_, timeout_ = 0.1, env_ = .GlobalEnv) {

  setTimeLimit(elapsed = timeout_, transient = TRUE)

  res_ = tryCatch(
    eval(expr_, envir = env_),
    error = function(e__) {
      obj__ = list()
      class(obj__) = 'customCall'
      return(obj__)
    }
  )

  setTimeLimit(cpu = Inf, elapsed = Inf)

  return(res_)

}

is.customCall = function(obj_) {

  'customCall' %in% class(obj_)

}

spliceOne = function(assignment_, fun_) {

  assignment_ = as.character(assignment_)
  body_ = capture.output(fun_)
  body_ = paste(body_, collapse = '\n')

  paste(assignment_, body_, sep = ' = ')

}

spliceAll = function(lst_) {

  # assignments_ = enexpr(lst_)
  assignments_ = as.character(lst_)[-1]
  assignments_ = map2(assignments_, lapply(assignments_, get, envir = .GlobalEnv), spliceOne)

  paste(unlist(assignments_), collapse = '\n\n')

}

firstly = function() {
  c('stats', 'graphics', 'grDevices', 'utils', 'datasets', 'methods', 'base')
}

cat_red = function(text_) {
  cat(paste0("\033[31m", text_, "\033[0m\n"))
}

cat_green = function(text_) {
  cat(paste0("\033[32m", text_, "\033[0m\n"))
}
