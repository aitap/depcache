hash <- function(value, version) {
	hash <- .Call(C_hash, value, version)
	paste(as.hexmode(as.integer(hash)), collapse = '')
}
