#' noter
#'
#' @return NULL
#' @export
noter = function(
    code,
    comments = NULL,
    taskID = tmpID(),
    path = getOption('noter_path'),
    check = getOption('noter_check'),
    template = getOption('noter_template')
) {

  # init
  resetTrace()
  if (!dir.exists(dirname(path))) dir.create(dirname(path))

  # encode
  if (is.character(code)) {
    code = parse(text = code)[[1]]
  } else {
    code = substitute(code)
  }

  text = deparse(code)
  symbols = get_symbols(eval(parse(text = sprintf('function() { %s }', text))))

  # browser()
  # ready to reproduce
  name = taskID
  pkgs = get_pkgs(symbols)
  toLoad = get_toLoad(pkgs)
  data = get_data(symbols, pkgs)

  # check
  if (check) checkit(toLoad, data, code)

  # generate text
  head = headMaker(path, template)
  body = bodyMaker(length(data), name, comments, toLoad, text)

  if (is.null(head)) {
    cat(body, file = path, append = T)
    last_trace$append = name
  } else {
    cat(paste(head, body, sep = '\n'), file = path)
    last_trace$file = path
  }

  if (length(data) > 0) {
    path2rdata = saveData(data, path, name)
    last_trace$rdata = path2rdata
  }

  cat_green(paste(sprintf('Writing to %s', path), '', sep = '\n'))
  cat(body, sep = '\n')

}
