# This Makefile uses "castle-engine" build tool for most operations
# (like compilation).
# See https://github.com/castle-engine/castle-engine/wiki/Build-Tool
# for instructions how to install/use this build tool.

.PHONY: run
run: check_one_unit_dependencies
	./check_units_dependencies.sh

check_one_unit_dependencies: check_one_unit_dependencies.lpr
	castle-engine compile

.PHONY: clean
clean:
	castle-engine clean
