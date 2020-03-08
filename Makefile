build:
	dune build main.exe

test: build
	python tests.py