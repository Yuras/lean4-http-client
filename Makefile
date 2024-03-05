all:
	lake build

run:
	lake exec http-client-main

test:
	lake exec http-client-spec
