# org-techo Makefile
# Use gmake on FreeBSD

EMACS ?= emacs
BATCH := $(EMACS) --batch -Q

PACKAGE := org-techo
VERSION := $(shell grep "^;; Version:" $(PACKAGE).el | sed 's/.*: //')

SRC := $(PACKAGE).el
TEST_DIR := tests
TESTS := $(wildcard $(TEST_DIR)/*.el)

.PHONY: all clean test lint checkdoc package-lint byte-compile ci help \
       test-fixtures fixtures-sample fixtures-full

all: lint test ## Run all checks

help: ## Show this help
	@awk 'BEGIN {FS = ":.*## "} /^[a-zA-Z_-]+:.*## / {printf "  %-15s %s\n", $$1, $$2}' $(MAKEFILE_LIST)

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
	@mkdir -p dist/$(PACKAGE)-$(VERSION)
	@cp $(SRC) README.org dist/$(PACKAGE)-$(VERSION)/
	@cp -r templates dist/$(PACKAGE)-$(VERSION)/
	@cd dist && tar -cvzf $(PACKAGE)-$(VERSION).tar.gz $(PACKAGE)-$(VERSION)
	@rm -rf dist/$(PACKAGE)-$(VERSION)

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

# =============================================================================
# Test Fixtures (L7 Engineer Persona)
# =============================================================================

FIXTURE_DIR := test-fixtures

fixtures-sample: ## Generate 7-day sample fixtures
	$(BATCH) -L . \
		-l $(SRC) \
		-l $(FIXTURE_DIR)/generate-test-data.el \
		-f org-techo-test-generate-sample

fixtures-full: ## Generate 90-day full fixtures
	$(BATCH) -L . \
		-l $(SRC) \
		-l $(FIXTURE_DIR)/generate-test-data.el \
		-f org-techo-test-generate-all

test-fixtures: ## Run fixture validation tests
	$(BATCH) -L . \
		-l ert \
		-l $(SRC) \
		-l $(FIXTURE_DIR)/generate-test-data.el \
		-l $(FIXTURE_DIR)/test-fixtures.el \
		-f ert-run-tests-batch-and-exit

fixtures-clean: ## Clean generated fixtures
	rm -rf $(FIXTURE_DIR)/techo-sample
	rm -rf $(FIXTURE_DIR)/techo-data
