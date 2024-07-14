SHELL := /usr/bin/env bash

EMACS ?= emacs
EASK ?= eask

TEST-FILES := $(shell ls test/*-test.el)

.PHONY: clean checkdoc lint package install compile download-sonarlint test

ci: clean package install compile download-sonarlint test

package:
	@echo "Packaging..."
	$(EASK) package

install:
	@echo "Installing..."
	$(EASK) install-deps --dev

compile:
	@echo "Compiling..."
	$(EASK) compile

download-sonarlint:
	@echo "Downloading SonarLint..."
	$(EASK) eval '(progn (require (quote lsp-sonarlint)) (lsp-sonarlint-download))'

test:
	@echo "Testing..."
	$(EASK) test ert $(TEST-FILES)

clean:
	$(EASK) clean all
