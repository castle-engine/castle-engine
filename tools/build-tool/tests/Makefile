MODE:=debug

.PHONY: run
run:
	castle-engine compile --mode=$(MODE) $(CASTLE_ENGINE_TOOL_OPTIONS)
	castle-engine run -- --all

.PHONY: clean
clean:
	castle-engine clean
