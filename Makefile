R = R

PACKAGE = $(shell $(R)script -e "\
 cat(read.dcf('DESCRIPTION')[,c('Package','Version')], sep = '_'); \
 cat('.tar.gz') \
")

all: $(PACKAGE)

.PHONY: check cran install

$(PACKAGE): man/* R/* tests/* tests-dev/* src/* DESCRIPTION NAMESPACE .Rbuildignore
	$(R) CMD build .

check: $(PACKAGE)
	$(R) CMD check $(PACKAGE)

cran: $(PACKAGE)
	$(R) CMD check --timings --as-cran $(PACKAGE)

install: $(PACKAGE)
	$(R) CMD INSTALL $(PACKAGE)
