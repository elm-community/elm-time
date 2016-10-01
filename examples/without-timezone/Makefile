SOURCES = $(shell find ../../src -type f -name "*.elm" -print)

index.js: Main.elm $(SOURCES)
	elm make --output=index.js Main.elm
	uglifyjs -c dead_code,properties,conditionals,evaluate,unused index.js > index.min.js
