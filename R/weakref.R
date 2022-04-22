weakref <- function(key, val, fin, onexit)
	.Call(C_weakref, key, val, fin, onexit)

weakrefkey <- function(weakref)
	.Call(C_weakrefkey, weakref)

weakrefval <- function(weakref)
	.Call(C_weakrefval, weakref)

env.addr <- function(env) {
	paste(as.hexmode(as.integer(.Call('env_addr', env))), collapse = '')
}
