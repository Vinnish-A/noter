
last_trace = new.env()

resetTrace = function() {
  last_trace$file = NULL
  last_trace$rdata = NULL
  last_trace$append = NULL
}

undo = function(path = getOption('noter_path')) {

  modify = 0

  if (!is.null(last_trace$file)) {
    file.remove(last_trace$file)
    cat_green(paste('File', last_trace$file, 'deleted'))
    modify = modify + 1
  }

  if (!is.null(last_trace$rdata)) {
    file.remove(last_trace$rdata)
    cat_green(paste('RData', last_trace$rdata, 'deleted'))
    modify = modify + 1
  }

  if (!is.null(last_trace$append)) {
    file = readLines(path)

    pos_dots = which(grepl('```$', file))
    chunk_start = min(which(grepl(last_trace$append, file)))
    chunk_end = min(pos_dots[pos_dots > chunk_start]) + 1

    if (!file[chunk_start - 1] %in% c('', '\n')) chunk_start = chunk_start - 1

    file = file[setdiff(1:length(file), seq(chunk_start, chunk_end))]
    writeLines(file, path)

    cat_green(paste('Append', last_trace$append, 'deleted'))
    modify = modify + 1
  }

  if (modify == 0) {
    cat_red('The previous operation is empty, or the previous operation can no longer be recalled.')
  }

}
