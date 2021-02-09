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
