# Constants

COLOR_RED=\033[0;31m
COLOR_GREEN=\033[0;32m
COLOR_YELLOW=\033[0;33m
COLOR_BLUE=\033[0;34m
COLOR_NONE=\033[0m
COLOR_CLEAR_LINE=\r\033[K

CMDSEP=;

# Targets

help:
	@grep -E '^[0-9a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) \
		| sort \
		| awk 'BEGIN {FS = ":.*?## "}; {printf "$(COLOR_BLUE)%-15s$(COLOR_NONE) %s\n", $$1, $$2}'

.PHONY: clean 
clean: ## Clean the build
	@stack clean

.PHONY: test 
test: clean ## Run the tests
	@stack test

.PHONY: run 
run: clean ## Run the application
	@stack run

.PHONY: autotest
autotest: ## Start auto testing when files change
	@git ls-files | entr -s "stack test"
