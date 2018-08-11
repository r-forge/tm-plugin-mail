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
        if(!is.null(x$compress))
            unlink(x$file)
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
    ## If the mbox source was compressed, we need to decompress, as
    ## seeking does not work otherwise ...
    magic <- readBin(x$mbox, "raw", n = 5L)
    z <- if(all(magic[1L : 2L] == c(0x1f, 0x8b)))
             gzfile(x$mbox)
         else if(rawToChar(magic[1L : 3L]) == "BZh")
             bzfile(x$mbox)
         else if(rawToChar(magic[1L : 5L]) ==
                 paste0(rawToChar(as.raw(0xfd)), "7zXZ"))
             xzfile(x$mbox)
         else
             NULL
    if(!is.null(z)) {
        lines <- readLines(z, warn = FALSE)
        close(z)
        ## Text connections are not seekable, so we really need a
        ## tempfile ...
        f <- tempfile()
        writeLines(lines, f, useBytes = TRUE)
        x$compress <- TRUE
        x$file <- file(f)
    } else
        x$file <- file(x$mbox)
    open(x$file)
    message.nr <- 0L
    offsets <- numeric()
    lines <- integer()
    while (length(line <- readLines(x$file, 1L, warn = FALSE)) == 1L) {
        if (startsWith(line, "From ")) {
            message.nr <- message.nr + 1L
            offsets[message.nr] <- seek(x$file)
            lines[message.nr] <- 0L
        } else
            lines[message.nr] <- lines[message.nr] + 1L
    }
    x$length <- length(lines)
    x$msgLines <- lines
    x$msgOffsets <- offsets
    x
}
