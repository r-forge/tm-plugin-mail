## To test the parsing of messages using message_from_bytes() from the
## Python email library.

use("tm.plugin.mail")
use("tm")

## Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")

wrk <- function(f) {
    message(sprintf("Processing %s ...", basename(f)))
    test <- tm.plugin.mail:::.py_msg_from_content
    msgs <- VCorpus(MBoxSource(f),
                    readerControl =
                        list(reader = tm.plugin.mail:::readMailBytes))
    info <- vapply(seq_along(msgs),
                   function(i) {
                       inherits(tryCatch(test(msgs[[i]]),
                                         error = identity),
                                "error")
                   },
                   NA)
    if(any(info))
        message(sprintf("  Got %d error(s) in %d messages",
                        sum(info), length(info)))
    info
}

if(FALSE) {
    files <- Sys.glob("~/Mail/[A-Za-z]*")
    files <- files[utils::file_test("-f", files)]
    infos <- lapply(files, wrk)
    names(infos) <- files
    names(infos)[vapply(infos, sum, 0) > 0]
    which(infos[[names(infos)[vapply(infos, sum, 0) > 0]]])
}

if(FALSE) {
    files <-
        Sys.glob("~/Work/Projects/rmails/tm.corpus.R.devel.mails/Work/*.gz")
    infos <- lapply(files, wrk)
    names(infos) <- files
    names(infos)[vapply(infos, sum, 0) > 0]
}

if(FALSE) {
    files <-
        Sys.glob("~/Work/Projects/rmails/tm.corpus.R.help.mails/Work/*.gz")
    infos <- lapply(files, wrk)
    names(infos) <- files
    names(infos)[vapply(infos, sum, 0) > 0]
}

if(FALSE) {
    files <-
        Sys.glob("~/Data/Archive/Mail/*.gz")
    infos <- lapply(files, wrk)
    names(infos) <- files
    names(infos)[vapply(infos, sum, 0) > 0]
}
