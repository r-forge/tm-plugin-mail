# Author: Ingo Feinerer

# E-mail document
MailDocument <-
    function(x = character(0), author = character(0), datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
             description = character(0), header = character(0), heading = character(0), id = character(0),
             origin = character(0), language = character(0), localmetadata = list())
{
    doc <- tm::PlainTextDocument(x, author, datetimestamp, description, heading, id, origin, language, localmetadata)
    attr(doc, "Header") <- header
    class(doc) <- unique(c("MailDocument", class(doc)))
    doc
}

as.PlainTextDocument.MailDocument <- function(x) {
    attr(x, "Header") <- NULL
    class(x) <- c("PlainTextDocument", "TextDocument", "character")
    x
}
