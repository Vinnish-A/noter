headMaker = function(path_, template_) {

  if (file.exists(path_)) return(NULL)

  if (pkgload::is_dev_package('noter')) {
    template_ = readLines('inst/extdata/template.Rmd')
  } else {
    template_ = readLines(template_)
  }

  paste(template_, collapse = '\n')

}

typist = function(pkgs_, code_, mod_) {

  dependency_ = unlist(lapply(pkgs_, \(pkg_) sprintf('library(%s)', pkg_)))
  toEval_ = mod_(code_)

  res_ = paste0(c(dependency_, toEval_), collapse = '\n')

  return(res_)

}

wrapper = function(RData_, name_, comments_, text_) {

  text_ = paste0(styler::style_text(text_), collapse = '\n')

  startor_ = \(name__) sprintf('```{r %s}', name__)
  endor_ = \(n__) paste0('```', paste0(rep('\n', n__), collapse = ''))

  if (RData_ > 0) {
    text_ = paste(sprintf("load('%s.RData')", name_), text_, sep = '\n')
  }
  if (is.null(comments_)) {
    paste(startor_(name_), text_, endor_(2), sep = '\n')
  } else {
    paste(comments_, startor_(name_), text_, endor_(2), sep = '\n')
  }

}

bodyMaker = function(RData_, name_, comments_, pkgs_, code_, mod_ = replacePlusSign) {

  suppressMessages(wrapper(RData_, name_, comments_, typist(pkgs_, code_, mod_)))

}

tmpID = function() {

  date_ = format(Sys.Date(), "%Y%m%d")
  suffix_ = substr(basename(tempfile()), 9, 14)

  paste(date_, suffix_, sep = '_')

}

saveData = function(data_, path_, name_) {

  isolated_ = list2env(data_, parent = emptyenv())
  filename_ = file.path(dirname(path_), paste0(name_, '.RData'))
  save(list = ls(isolated_), envir = isolated_, file = filename_)

  return(filename_)

}
