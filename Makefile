all:
	@dune build @all

watch:
	@dune build @all -w

test:
	@dune runtest --force --no-buffer

clean:
	@dune clean

doc:
	@dune build @doc

update-submodules:
	@git submodule update --init

VERSION=$(shell awk '/^version:/ {print $$2}' linol.opam)

update_next_tag:
	@echo "update version to $(VERSION)..."
	sed -i "s/NEXT_RELEASE/$(VERSION)/g" $(wildcard src/*.ml) $(wildcard src/*.mli) $(wildcard src/**/*.ml) $(wildcard src/**/*.mli)
