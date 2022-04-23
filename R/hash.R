hash <- function(value, version)
	paste(as.character(.Call(C_hash, value, version)), collapse = ' ')
