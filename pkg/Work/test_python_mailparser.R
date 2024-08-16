## Suppose we have the Python mail-parser library
##   <https://pypi.org/project/mail-parser/>
## downloaded from
##   <https://pypi.org/project/mail-parser/#files>
## and extracted into e.g.
##   ~/tmp/mail-parser-3.15.0/

## To test how well this works for us, we can do the following:

use("tm")
use("tm.plugin.mail")

Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")

.py_mail_parser <-
    reticulate::import_from_path("mailparser",
                                 "/home/Hornik/tmp/mail-parser-3.15.0/")

wrk <- function(f) {
    message(sprintf("Processing %s ...", basename(f)))
    msgs <- VCorpus(MBoxSource(f),
                        readerControl = list(reader = readMailBytes))
    info <- vapply(seq_along(msgs),
                   function(i) {
                       x <- charToRaw(paste(msgs[[i]],
                                            collapse = "\n"))
                       inherits(tryCatch(.py_mail_parser$parse_from_bytes(x),
                                         error = identity),
                                "error")
                   },
                   NA)
    if(any(info))
        message(sprintf("  Got %d error(s) in %d messages",
                        sum(info), length(info)))
    saveRDS(info, sprintf("mailparser_test_%s.rds", basename(f)))
    info
}

run <- function(f) {
    msgs <- VCorpus(MBoxSource(f),
                        readerControl = list(reader = readMailBytes))
    lapply(seq_along(msgs),
           function(i) {
               x <- charToRaw(paste(msgs[[i]], collapse = "\n"))
               tryCatch(.py_mail_parser$parse_from_bytes(x),
                        error = identity)
           })
}



if(FALSE) {
    files <- Sys.glob("~/Mail/[A-Za-z]*")
    files <- files[utils::file_test("-f", files)]
    infos <- lapply(files, wrk)
    names(infos) <- files
}

if(FALSE) {
    files <-
        Sys.glob("~/Work/Projects/rmails/tm.corpus.R.devel.mails/Work/*.gz")
    infos <- lapply(files, wrk)
    zzz <- run("~/Work/Projects/rmails/tm.corpus.R.devel.mails/Work/2024-05.txt.gz")
}
