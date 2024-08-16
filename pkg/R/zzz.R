.py_email <- NULL
.py_codecs <- NULL

.onLoad <- function(libname, pkgname) {
    rapv <- Sys.getenv("RETICULATE_AUTOCREATE_PACKAGE_VENV", "")
    if(nzchar(rapv))
        on.exit(Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = rapv))
    Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")
    .py_email <<- import("email", delay_load = TRUE)
    .py_codecs <<- import("codecs", delay_load = TRUE)
}
