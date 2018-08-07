MBoxSource <-
function(mbox, encoding = "")
{
    SimpleSource(encoding = encoding, file = NULL, length = 0,
                 reader = readMail, mbox = mbox, msgLines = 0,
                 msgOffsets = 0, class = "MBoxSource")
}

close.MBoxSource <-
function(con, ...)
{
    x <- con
    if (!is.null(x$file)) {
        close(x$file)
        x$file <- NULL
        x$length <- x$msgLines <- x$msgOffsets <- 0
    }
    x
}

getElem.MBoxSource <-
function(x)
{
    stopifnot(!is.null(x$file))

    seek(x$file, x$msgOffsets[x$position])
    list(content = iconv(readLines(x$file,
                                   x$msgLines[x$position],
                                   warn = FALSE),
                         x$encoding, "UTF-8", "byte"),
         uri = x$mbox)
}

open.MBoxSource <-
function(con, ...)
{
    x <- con
    x$file <- file(x$mbox)
    open(x$file)
    message.nr <- 0
    offsets <- integer(0)
    lines <- integer(0)
    while (length(line <- readLines(x$file, 1L, warn = FALSE)) == 1L) {
        if (startsWith(line, "From ")) {
            message.nr <- message.nr + 1
            offsets[message.nr] <- seek(x$file)
            lines[message.nr] <- 0
        } else
            lines[message.nr] <- lines[message.nr] + 1
    }
    x$length <- length(lines)
    x$msgLines <- lines
    x$msgOffsets <- offsets
    x
}
