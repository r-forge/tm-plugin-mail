## E-mail document
MailDocument <-
function(x = character(),
         author = character(),
         datetimestamp = as.POSIXlt(Sys.time(), tz = "GMT"),
         description = character(),
         header = character(),
         heading = character(),
         id = character(),
         language = character(),
         origin = character(),
         ...,
         meta = NULL)
{
    ## Argh.
    ## As of 2024-08, tm::PlainTextDocument() does as.character() on the
    ## content, which strips all attributes ...
    y <- PlainTextDocument(x,
                           author, datetimestamp, description,
                           heading, id, language, origin,
                           header = header,
                           ...,
                           meta = meta,
                           class = "MailDocument")
    names(y$content) <- names(x)
    y
}

`content<-.MailDocument` <-
function(x, value)
{
    ## Argh.
    ## As of 2024-08, tm:::`content<-.PlainTextDocument` does
    ##   x$content <- as.character(value)
    ## which strips all attributes ...
    x$content <- as.character(value)
    names(x$content) <- names(value)
    x
}
