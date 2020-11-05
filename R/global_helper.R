as_call <- function(x) {
  if (inherits(x, "formula")) {
    stopifnot(length(x) == 2)
    x[[2]]
  } else if (is.atomic(x) || is.name(x) || is.call(x)) {
    x
  } else {
    stop("Unknown input")
  }
}

interpolate <- function(code, ..., mydir, `_env` = parent.frame(), file = "code_All.R", append = FALSE, save_result = FALSE, eval = TRUE) {
  stopifnot(inherits(code, "formula"), length(code) == 2)
  
  args <- lapply(list(...), as_call)
  expr <- methods::substituteDirect(as_call(code), args)
  
  cat(paste0(as.character(expr)[2], "\n"), file = file.path(mydir, file), append = append)
  
  if (save_result) cat(paste0(paste(readLines(file.path(mydir, file)), collapse = "\n"), "\n"), file = file.path(mydir, "code_All.R"), append = TRUE)
  if (eval) eval(expr, `_env`)
}

## For code printing
clean_readlines <- function(file) {
  return(tidy_source(file, output = FALSE)$text.tidy)
}

init_code_all_R <- function(userdir) {
  interpolate(~(library(bulletxtrctr)), file = "code_All.R", mydir = userdir, append = FALSE, nodupes = TRUE)
  interpolate(~(library(x3ptools)), file = "code_All.R", mydir = userdir, append = TRUE, nodupes = TRUE)
  interpolate(~(library(dplyr)), file = "code_All.R", mydir = userdir, append = TRUE, nodupes = TRUE)
  # interpolate(~('\n'), file = "code_All.R", mydir = userdir, append = TRUE, nodupes = TRUE)
}