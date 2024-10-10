init:
	git submodule update --init
	cd rocket-chip && git submodule update --init

test: init format fix
	mill -i ogpu\[chisel\].test

format:
	mill -i ogpu\[chisel\].reformat
	mill -i ogpu\[chisel\].test.reformat

fix:
	mill -i ogpu\[chisel\].fix
	mill -i ogpu\[chisel\].test.fix

count:
	mill -i ogpu\[chisel\].printLineCount

.phony: test
