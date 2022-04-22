hash <- function(text) {
	text <- as.character(text)
	hash <- .C(C_fnv1a64, text = text, ntext = length(text), hash = raw(8))$hash
	paste(as.hexmode(as.integer(hash)), collapse = '')
}
