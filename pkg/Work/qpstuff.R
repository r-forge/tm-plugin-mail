qpdecode <- function(x, header = FALSE) {
    x <- gsub("=\n", "", x)
    if(header)
        x <- gsub("_", " ", x)
    ## See URLdecode():
    vapply(x,
           function(e) {
               e <- charToRaw(e)
               u <- charToRaw("=")
               v <- raw(0L)
               i <- 1L
               while(i <= length(e)) {
                   if(e[i] != u) {
                       v <- c(v, e[i])
                       i <- i + 1L
                   }
                   else {
                       y <- as.integer(e[i + 1L:2L])
                       y[y > 96L] <- y[y > 96L] - 32L
                       y[y > 57L] <- y[y > 57L] - 7L
                       y <- sum((y - 48L) * c(16L, 1L))
                       v <- c(v, as.raw(as.character(y)))
                       i <- i + 3L
                   }
               }
               rawToChar(v)
           },
           "",
           USE.NAMES = FALSE)
}

qpdecode_mime_header <- function(s) {
    s <- gsub("[[:space:]]+", " ", s)
    p <- "=[?]([^?]+)[?]Q[?]([^?]+)[?]=[[:space:]]?"
    m <- gregexpr(p, s)
    regmatches(s, m) <-
        lapply(regmatches(s, m),
               function(e) qpdecode(sub(p, "\\2", e)))
    s
}

if(FALSE) {
    s <- "X-CRAN-Issue-Info: =?us-ascii?Q?2024-09-09%5F09:40:33%5Fhornik:ERROR%5Ffrom%5Fupgrade%5Fsf:Q?=
    =?us-ascii?Q?UERIED:2024-09-09:hornik?="

    s <- "X-CRAN-Issue-Info: =?us-ascii?Q?2024-09-09=5F09:20:53=5Fhornik:ERROR=5Ffrom=5Fupgrade=5Fmsm?=
    =?us-ascii?Q?:QUERIED:2024-09-09:hornik:BEFORE:2024-09-23?="

    s <- "X-CRAN-Issue-Info: =?us-ascii?Q?2024-09-09=5F09:20:53=5Fhornik:ERROR=5Ffrom=5Fupgrade=5Fmsm?=
    =?us-ascii?Q?:QUERIED:2024-09-09:hornik:BEFORE:2024-09-23?="
    qpdecode_mime_header(s)
}

if(FALSE) {
qpdecode("sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent")
qpdecode("stern=2Ede_-_t=C3=A4glich", header = TRUE)
}

if(FALSE) {
Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")
.py_quopri <- reticulate::import("quopri")
.py_quopri$encodestring(charToRaw("München"))
.py_quopri$encodestring(charToRaw(iconv("München", to = "latin1")))

.py_quopri$decodestring("sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent")
.py_quopri$decodestring("stern=2Ede_-_t=C3=A4glich")$decode("UTF-8")
}    
