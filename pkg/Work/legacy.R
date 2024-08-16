## To illustrate issues with the legacy mail reader code.

use("tm")
use("tm.plugin.mail")

msgs <- VCorpus(MBoxSource("~/Mail/aasc"),
                readerControl =
                    list(reader = tm.plugin.mail:::readMailLegacy))
hdrs <- lapply(msgs, function(e) e$meta$header)

## MIME 2047 trouble:
hdrs[[3]]$Subject

## MIME 2045 trouble:
hdrs[[6]][["Content-Type"]]
hdrs[[6]][["Content-Transfer-Encoding"]]
msgs[[6]]$content

## Some MIME empirics (actually not quite perfect as the header field
## names are not case sensitive).
table(lengths(lapply(hdrs, `[[`, "MIME-Version")))

ctypes <- lapply(hdrs, `[[`, "Content-Type")
table(lengths(ctypes))
writeLines(formatDL(sort(table(tolower(unlist(ctypes))))))

ctes <- lapply(hdrs, `[[`, "Content-Transfer-Encoding")
table(lengths(ctes))
writeLines(formatDL(sort(table(tolower(unlist(ctes))))))
