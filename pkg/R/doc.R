## Author: Ingo Feinerer

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
    PlainTextDocument(x,
                      author, datetimestamp, description, heading, id,
                      language, origin,
                      header = header,
                      ...,
                      meta = meta,
                      class = "MailDocument")
