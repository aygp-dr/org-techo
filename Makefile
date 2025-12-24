# org-techo Makefile
# Use gmake on FreeBSD

EMACS ?= emacs
BATCH := $(EMACS) --batch -Q

PACKAGE := org-techo
VERSION := $(shell grep "^;; Version:" $(PACKAGE).el | sed 's/.*: //')

SRC := $(PACKAGE).el
TEST_DIR := tests
TESTS := $(wildcard $(TEST_DIR)/*.el)

.PHONY: all clean test lint checkdoc package-lint byte-compile ci help

all: lint test ## Run all checks

help: ## Show this help
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | \
		awk 'BEGIN {FS = ":.*?## "}; {printf "  %-15s %s\n", $$1, $$2}'

# =============================================================================
# Testing
# =============================================================================

test: ## Run ERT tests
	$(BATCH) -L . \
		-l ert \
		-l $(SRC) \
		-l $(TEST_DIR)/test-$(PACKAGE).el \
		-f ert-run-tests-batch-and-exit

test-verbose: ## Run ERT tests with verbose output
	$(BATCH) -L . \
		-l ert \
		-l $(SRC) \
		-l $(TEST_DIR)/test-$(PACKAGE).el \
		--eval "(ert-run-tests-batch-and-exit '(not (tag :slow)))"

# =============================================================================
# Linting
# =============================================================================

lint: checkdoc package-lint byte-compile ## Run all linters

checkdoc: ## Check documentation strings
	$(BATCH) -L . \
		-l $(SRC) \
		--eval "(require 'checkdoc)" \
		--eval "(setq checkdoc-verb-check-experimental-flag nil)" \
		--eval "(checkdoc-file \"$(SRC)\")" \
		--eval "(message \"checkdoc: OK\")"

package-lint: ## Run package-lint
	$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(unless (package-installed-p 'package-lint) (package-install 'package-lint))" \
		--eval "(require 'package-lint)" \
		-f package-lint-batch-and-exit $(SRC)

byte-compile: ## Byte compile the package
	$(BATCH) -L . \
		--eval "(setq byte-compile-error-on-warn t)" \
		-f batch-byte-compile $(SRC)

# =============================================================================
# CI
# =============================================================================

ci: lint test ## Run CI pipeline (lint + test)
	@echo "CI pipeline passed!"

# =============================================================================
# Package
# =============================================================================

package: clean ## Create package tarball
	@mkdir -p dist
	tar --transform 's,^,$(PACKAGE)-$(VERSION)/,' \
		-cvzf dist/$(PACKAGE)-$(VERSION).tar.gz \
		$(SRC) templates/ README.org

# =============================================================================
# Cleanup
# =============================================================================

clean: ## Clean build artifacts
	rm -f *.elc
	rm -rf dist/
	rm -f $(TEST_DIR)/*.elc

# =============================================================================
# Development
# =============================================================================

install-deps: ## Install development dependencies
	$(BATCH) \
		--eval "(require 'package)" \
		--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)" \
		--eval "(package-initialize)" \
		--eval "(package-refresh-contents)" \
		--eval "(dolist (pkg '(package-lint)) (unless (package-installed-p pkg) (package-install pkg)))"

run: ## Start Emacs with org-techo loaded
	$(EMACS) -Q -L . -l $(SRC) \
		--eval "(message \"org-techo loaded. Try M-x org-techo-goto-today\")"
