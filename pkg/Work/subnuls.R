## As of 2024-08, in some cases getting the Python results back into R
## fails with 'Embedded NUL in string', apparently from
##   	python.cpp:#define as_utf8_r_string(str) Rcpp::String(as_std_string(str))
## where in turn Rcpp has
##   Changes in Rcpp version 1.0.0 (2018-11-05)
##      \item \code{Rcpp::String} no longer silently drops embedded
##      \code{NUL} bytes in strings but throws new Rcpp exception
##      \code{embedded_nul_in_string}.
##      (Kevin in \ghpr{917} fixing \ghit{916}).

## One possibly workaround is fixing things ourselves.  This needs
## changing the code to use
##   .py_email <- reticulate::import("email", convert = FALSE)
## and then doing all conversions by hand using
##   reticulate::py_to_r
## and to avoid the embedded NUL errors suitably handly them on the
## Python side (see below): easy if we already have a str, not so clear
## if we have a list of str.

use("tm.plugin.mail")
use("tm")
## Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")

.py_email <- reticulate::import("email", convert = FALSE)

## Equivalent of .py_msg_from_content():
work <- function(x) {
    .py_email$message_from_bytes(charToRaw(paste(x, collapse = "\n")),
                                 policy = .py_email$policy$default)
}

reticulate::py_run_string("def subnuls(x):\n    return x.replace(\"\\x00\", \"<00>\")")

## Variant of .py_get_bdy_from_msg():
fun <- function(msg) {
    charset <- reticulate::py_to_r(msg$get_content_charset())
    if(!is.null(charset) && startsWith(charset, "unknown"))
        msg$set_charset("us-ascii")
    wrk <- function(e)
        reticulate::py_to_r(reticulate::py$subnuls(e$get_content()))
    one <- function(e) {
        ct <- reticulate::py_to_r(e$get_content_type())
        if(ct == "text/plain")
            structure(wrk(e), names = "plain")
        else if(ct == "text/html")
            structure(wrk(e), names = "html")
        else
            character()
    }
    if(reticulate::py_to_r(msg$is_multipart())) {
        bdy <- lapply(reticulate::iterate(msg$iter_parts()), one)
        structure(unlist(bdy), names = unlist(lapply(bdy, names)))
    }
    else
        one(msg)
}

txts <- VCorpus(MBoxSource("~/Mail/misc"),
                readerControl =
                    list(reader = tm.plugin.mail:::readMailBytes))
msgs <- lapply(seq_along(txts),
               function(i) work(txts[[i]]))

## For individual header fields, we can easily access and convert:
types <- vapply(msgs,
                function(e)
                    reticulate::py_to_r(e$get_content_type()),
                "")
ctes <- vapply(msgs,
               function(e) {
                   y <- reticulate::py_to_r(e$get("Content-Transfer-Encoding")) 
                   if(is.null(y)) "" else y
               },
               "")

## Now for getting the body parts:
bdys <- lapply(msgs,
               function(e) tryCatch(fun(e), error = identity))
table(vapply(bdys, inherits, NA, "error"))
