



is.global = function(chr_) {

  T %in% getAnywhere(chr_)$where == '.GlobalEnv'

}

#' get_symbols
#'
#' @param ast_node_
#'
#' @import rlang codetools purrr
#'
#' @return symbols
#' @keywords internal
get_symbols = function(fun_) {

  symbols_ = as.list(findGlobals(fun_))

  # browser()
  for (i_ in seq_along(symbols_)) {

    symbol_ = symbols_[[i_]]
    if (is.function(easyGet(symbol_)) & is.global(symbol_)) {
      symbols_[[i_]] = get_symbols2(easyGet(symbol_))
    }

  }

  return(unlist(symbols_))

}

get_pkgs = function(symbols_) {

  where_ = lapply(symbols_, \(x) getAnywhere(x)[['where']])

  return(where_)

}

get_toLoad = function(pkgs_) {

  pkgs_ = pkgs_[unlist(lapply(pkgs_, length)) != 0]
  pkgs_ = lapply(pkgs_, \(pkg__) str_sub2(pkg__[grepl('package:', pkg__)], 9))
  pkgs_ = setdiff(unique(unlist(pkgs_)), firstly())

  return(pkgs_)

}

get_data = function(symbols_, pkgs_) {

  toGet_ = symbols_[unlist(lapply(pkgs_, \(pkg__) T %in% grepl('.GlobalEnv', pkg__)))]
  data_ = lapply(toGet_, get, envir = .GlobalEnv)
  names(data_) = toGet_

  return(data_)

}

easyGet = function(chr_, env_ = .GlobalEnv) {

  tryCatch(
    get(chr_, envir = env_),
    error = \(e) return(NULL)
  )

}

checkit = function(toLoad_, data_, code_) {

  data_ = appendWithName(data_, code_ = code_)
  isolated_ = list2env(data_, parent = emptyenv())

  for (pkg_ in c(firstly(), toLoad_)) {
    parent.env(isolated_) = as.environment(sprintf('package:%s', pkg_))
  }

  check_ = tryCatch(
    expr = {
      with(isolated_, {eval(code_)})
      return('pass')
    },
    error = function(e__) {
      cat_red(paste('Fail to pass. Error: ', e__$message, sep = "\n"))
      return('Fail to pass')
    }
  )

  if (check_ == 'Fail to pass') stop()

}

