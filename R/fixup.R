# apply changes to objects in order to make their hashes reproducible
# between R versions and operating systems
fixup <- function(x)
	if (is.function(x)) {
		# In newer versions of R, functions may contain bytecode, which
		# gets serialised together with the function. If we re-create
		# the body of the function on the fly, the function gets
		# serialised the same way as in older R.
		`body<-`(fun = x, envir = environment(x), value = Recall(body(x)))
	} else if (is.recursive(x) && !is.environment(x)) {
		# Envirionments are recursive objects, but they have reference
		# semantics. Instead of trying to re-create an environment with
		# fixed-up contents but otherwise exactly the same as the
		# original one, we just don't touch it, relying on the user do
		# do the right thing (if needed, ignore the environment and
		# manually add the important parts as dependencies).
		for (i in seq_along(x)) x[[i]] <- Recall(x[[i]])
		x
	} else if (is.character(x)) {
		# A character vector may be of native encoding, which may be
		# different between operating systems, resulting in different
		# byte values and hashes, despite the values meaning the same
		# thing. It's possible to have strings of different Encoding()
		# which are nevertheless identical(), so this is most likely the
		# right thing to do... unless the string is not representable in
		# UTF-8.
		enc2utf8(x)
	} else x
