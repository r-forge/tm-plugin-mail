Sys.setenv("RETICULATE_AUTOCREATE_PACKAGE_VENV" = "false")
.py_quopri <- reticulate::import("quopri")
.py_quopri$encodestring(charToRaw("München"))
.py_quopri$encodestring(charToRaw(iconv("München", to = "latin1")))
