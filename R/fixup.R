# Test for R_MissingArg, not actually missing argument
.missing <- function(x) {
	x <- x
	missing(x)
}

# In R < 3.6, removeSource only works with functions, despite language
# objects also have source references to remove.
# In R < 3.5, there's a bug concerning function calls involving NULLs:
# elementwise replacement is done using x[[i]] <- Recall(x[[i]]), so
# when x[[i]] is NULL, the language object ends up being shortened by
# one.
# On the one hand, we're playing with undocumented stuff here. On the
# other hand, there will never be another version of R 3.x, so any
# remaining bugs can be fixed right here.
.removeSource <- function(x)
	if (getRversion() >= '3.6') removeSource(x) else {
		# no attributes for symbols; no source references for primitives
		if (is.name(x) || is.primitive(x)) return(x)
		attr(x, 'srcref') <- NULL
		attr(x, 'wholeSrcref') <- NULL
		attr(x, 'srcfile') <- NULL
		# might contain more source references below
		if (is.function(x)) {
			# functions are recursive language objects, but aren't
			# iterable
			body(x) <- Recall(body(x))
		} else if (is.recursive(x) && is.language(x)) {
			for (i in seq_along(x))
				x[i] <- list(Recall(x[[i]])) # watch out for NULLs
		}
		x
	}

# apply changes to objects in order to make their hashes reproducible
# between R versions and operating systems
fixup <- function(x) {
	# There's nothing to fix up about primitives, and they trip up
	# body() replacement.
	if (is.primitive(x)) return(x)
	# Source references can be different for equivalent functions and
	# expressions and so must be removed.
	if (is.language(x)) x <- .removeSource(x)
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
