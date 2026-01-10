# main entry point to build project

.PHONY: release debug clean clean-all help

release:
	@$(MAKE) --no-print-directory -f BuildRelease.mk all

debug:
	@$(MAKE) --no-print-directory -f BuildDebug.mk all

clean:
	@$(MAKE) --no-print-directory -f BuildClean.mk clean

clean-all:
	@$(MAKE) --no-print-directory -f BuildClean.mk clean-all

help:
	@echo "Usage: make <target>"
	@echo ""
	@echo "  release   - Build optimized release"
	@echo "  debug     - Build with debug symbols"
	@echo "  clean     - Remove build artifacts"
	@echo "  clean-all - Remove everything including generated sources"