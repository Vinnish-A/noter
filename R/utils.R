
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

firstly = function() {
  c('stats', 'graphics', 'grDevices', 'utils', 'datasets', 'methods', 'base')
}

cat_red = function(text_) {
  cat(paste0("\033[31m", text_, "\033[0m\n"))
}

cat_green = function(text_) {
  cat(paste0("\033[32m", text_, "\033[0m\n"))
}
