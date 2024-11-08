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
    template = getOption('noter_template'),
    expr = F,
    assignment = NULL,
    mustLoad = NULL
) {

  # init
  resetTrace()
  if (!dir.exists(dirname(path))) dir.create(dirname(path))

  # accessory
  assignment = enexpr(assignment)
  assignment = spliceAll(assignment)
  if (assignment != '') assignment = paste0(assignment, '\n')

  # encode
  if (expr) {
    code = substitute(code)
    text = paste('{', assignment, paste(deparse(code), collapse = '\n'), '}', sep = '\n')
  }
  if (is.character(code)) {
    text = paste('{', assignment, code, '}', sep = '\n')
  } else if (is.function(code)) {
    text = capture.output(code)
    text = paste(text[-c(1, length(text))], collapse = '\n')
    text = paste('{', assignment, text, '}', sep = '\n')
  }

  symbols = get_symbols(eval(parse(text = sprintf('function() { %s }', text))))

  # ready to reproduce
  name = taskID
  pkgs = get_pkgs(symbols)
  toLoad = get_toLoad(pkgs) |> c(mustLoad)
  data = get_data(symbols, pkgs)

  # check
  if (check) checkit(toLoad, data, text)

  # generate text
  head = headMaker(path, template)
  body = bodyMaker(length(data), name, comments, toLoad, substr(text, 2, nchar(text)-1))

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


