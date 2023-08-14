SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/*.el)

.PHONY: clean checkdoc lint package install compile test

ci: clean package install compile test

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install --dev

compile:
	@echo "Compiling..."
	$(EASK) compile

test:
	@echo "Testing..."
	$(EASK) test ert $(TEST-FILES)

clean:
	$(EASK) clean all
