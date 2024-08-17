Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")
.py_quopri <- reticulate::import("quopri")
.py_quopri$encodestring(charToRaw("München"))
.py_quopri$encodestring(charToRaw(iconv("München", to = "latin1")))

.py_quopri$decodestring("sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent")
.py_quopri$decodestring("stern=2Ede_-_t=C3=A4glich")$decode("UTF-8")

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

qpdecode("sur la route =C3=A0 suivre les voil=C3=A0 bient=C3=B4t qui te d=C3=A9gradent")
qpdecode("stern=2Ede_-_t=C3=A4glich", header = TRUE)
