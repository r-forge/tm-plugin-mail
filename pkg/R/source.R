MBoxSource <-
function(mbox, format = "mbox", delim = NULL)
{
    format <- match.arg(format, c("mbox", "mboxo", "mboxrd"))
    if(is.null(delim))
        delim <- if(format == "mbox") delim_mbox else "^From "
    
    SimpleSource(encoding = "bytes", reader = readMail,
                 mbox = mbox, format = format, delim = delim,
                 file = NULL, msgOffsets = 0, msgLengths = 0, 
                 class = "MBoxSource")
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
        x$length <- x$msgOffsets <- x$msgLengths <- 0
    }
    x
}

getElem.MBoxSource <-
function(x)
{
    stopifnot(!is.null(x$file))
    seek(x$file, x$msgOffsets[x$position])
    lines <- read_bytes(x$file, x$msgLengths[x$position])
    if(x$format == "mboxo")
        lines <- sub("^>From ", "From ", lines)
    else if(x$format == "mboxrd")
        lines <- sub("^>(>*From )", "\\1", lines)
    list(content = lines, uri = x$mbox)
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
        lines <- read_bytes(z)
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
    
    .offsets <- numeric()
    .lengths <- integer()
    blank <- TRUE
    num <- 0L
    len <- 0L
    while(length(line <- read_bytes(x$file, 1L)) == 1L) {
        if(blank && grepl(x$delim, line)) {
            .lengths[num] <- len
            num <- num + 1L
            len <- 0L
            .offsets[num] <- seek(x$file)
            blank <- FALSE
        } else {
            len <- len + 1L
            blank <- !nzchar(line)
        }
    }
    .lengths[num] <- len
    x$length <- length(.lengths)
    x$msgLengths <- .lengths
    x$msgOffsets <- .offsets
    x
}

read_bytes <- function(con, n = -1L)
    readLines(con, n, warn = FALSE, encoding = "bytes")

delim_mbox <-
    paste0("^From ",
           ".* ",
           sprintf("(%s) ",
                   paste(c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri",
                           "Sat"),
                         collapse = "|")),
           sprintf("(%s) ",
                   paste(c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                           "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                         collapse = "|")),
           "[ 0-3][0-9] ",
           "[0-2][0-9]:[0-5][0-9]:[0-6][0-9] ",
           sprintf("(%s)? *",
                   paste(c("[A-Z]?[A-Z]?[A-Z][A-Z]( DST)?",
                           "[+-]?[0-9][0-9][0-9][0-9]",
                           ""),
                         collapse = "|")),
           "[ 0-9][ 0-9][ 0-9][0-9]",
           ".*$")
    


