

#' get_symbols
#'
#' @param ast_node_
#'
#' @import rlang
#'
#' @return
#' @export
get_symbols = function(ast_node_) {

  if (is_symbol(ast_node_)) {
    return(as.character(ast_node_))
  }

  if (is_pairlist(ast_node_) || is.call(ast_node_)) {
    return(flatten_chr(map(ast_node_, get_symbols)))
  }

  return(NULL)

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

