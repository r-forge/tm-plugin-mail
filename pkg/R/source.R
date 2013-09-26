MBoxSource <-
function(mbox, encoding = "unknown")
{
    f <- file(mbox)
    open(f)
    message.nr <- 0
    offsets <- integer(0)
    lines <- integer(0)
    while (length(line <- readLines(f, 1)) == 1) {
        newMessage <- length(grep("^From ", line, useBytes = TRUE))
        if (newMessage) {
            message.nr <- message.nr + 1
            offsets[message.nr] <- seek(f)
            lines[message.nr] <- 0
        } else {
            lines[message.nr] <- lines[message.nr] + 1
        }
    }
    s <- tm::Source(encoding = encoding, length = length(lines),
                    vectorized = TRUE, class = "MBoxSource")
    s$File <- f
    s$mbox <- mbox
    s$MessageOffsets <- offsets
    s$MessageLines <- lines
    s
}

getElem.MBoxSource <-
function(x)
{
    seek(x$File, x$MessageOffsets[x$Position])
    list(content = readLines(x$File, x$MessageLines[x$Position],
                             encoding = x$Encoding),
         uri = x$mbox)
}

pGetElem.MBoxSource <-
function(x)
{
    lapply(1:length(x$MessageOffsets),
        function(y) {
            seek(x$File, x$MessageOffsets[y])
            list(content = readLines(x$File, x$MessageLines[y],
                                     encoding = x$Encoding),
                 uri = x$mbox)
        })
}

eoi.MBoxSource <-
function(x) length(x$MessageOffsets) <= x$Position
