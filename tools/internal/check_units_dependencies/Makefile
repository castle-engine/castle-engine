# Run the test by "make".
# Cleanup by "make clean".

.PHONY: run
run: check_one_unit_dependencies
	./check_units_dependencies.sh

check_one_unit_dependencies: check_one_unit_dependencies.lpr
	castle-engine compile

.PHONY: clean
clean:
	castle-engine clean
