.PHONY: all compile clean doc

all: compile

compile:
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump
	rm -f `find doc/* -prune -type f`

doc:
	cp priv/doc/* doc
	./priv/docgen.erl doc src/erlami*.erl

