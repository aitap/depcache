# Test for R_MissingArg, not actually missing argument
.missing <- function(x) {
	x <- x
	missing(x)
}

# apply changes to objects in order to make their hashes reproducible
# between R versions and operating systems
fixup <- function(x) {
	# Source references can be different for equivalent functions and
	# expressions and so must be removed.
	# removeSource() only worked with functions until R 3.6.
	if ((if (getRversion() >= '3.6') is.language else is.function)(x))
		x <- removeSource(x)
	# Functions are special in that they are recursive, but can't be
	# directly subsetted: they consist of formals, body and environment,
	# all of which could be subsetted.
	if (is.function(x)) {
		body(x) <- Recall(body(x))
		formals(x) <- Recall(formals(x))
	}
	# Otherwise recurse for all recursive objects, except environments
	# (we don't touch those because of their reference semantics) and
	# functions (can't be subsetted, see above).
	# The user might have to intervene here.
	if (is.recursive(x) && !is.environment(x) && !is.function(x))
		for (i in seq_along(x))
			# 1. Inside formals() there can be R_MissingArg, better
			# leave it alone.
			# 2. There's nothing to fix up in a NULL, and inserting a
			# NULL in a list removes the entry.
			if (!.missing(x[[i]]) && !is.null(x[[i]]))
				x[[i]] <- Recall(x[[i]])
	# A character vector may be of native encoding, which may be
	# different between operating systems, resulting in different byte
	# values and hashes, despite the values meaning the same thing. It's
	# possible to have strings of different Encoding() which are
	# nevertheless identical(), so this is most likely the right thing
	# to do... unless the string is not representable in UTF-8. The user
	# might have to intervene here too.
	if (is.character(x)) x <- enc2utf8(x)
	x
}
