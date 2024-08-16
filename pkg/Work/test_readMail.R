## To test the new readMail().

use("tm.plugin.mail")
use("tm")

if(FALSE) {
texts <- VCorpus(MBoxSource("~/tmp/misc"))
probs <- lapply(texts, function(e) e$meta$problems)
which(lengths(probs) > 0L)
chars <- lapply(texts, as.character)
table(lengths(chars))
}

tst <- function(f) {
    message(sprintf("Processing %s ...", basename(f)))
    texts <- VCorpus(MBoxSource(f))
    probs <- lapply(texts, function(e) e$meta$problems)
    infos <- NULL
    where <- which(lengths(probs) > 0L)
    if(length(where)) {
        infos <- unlist(probs[where])
        names(infos) <- where
        print(infos)
    }
    infos
}

if(FALSE) {
    tst("~/tmp/misc")
}

if(FALSE) {
    files <-
        Sys.glob("~/Work/Projects/rmails/tm.corpus.R.devel.mails/Work/*.gz")
    infos <- lapply(files, tst)
    names(infos) <- files
    Filter(length, infos)
}    

if(FALSE) {
    files <- Sys.glob("~/Mail/[A-Za-z]*")
    files <- files[utils::file_test("-f", files)]
    infos <- lapply(files, tst)
    names(infos) <- files
    Filter(length, infos)
    table(unlist(infos))
}    

