init:
	git submodule update --init --recursive

toy_alu: init
	mill -i opengpgpu.runMain  opengpgpu.alu.ALURTL

test: init
	mill -i opengpgpu.test

z_test: init
	mill -i opengpgpu.test -z $(OPT)

alu: init
	mill -i opengpgpu.runMain  opengpgpu.pipeline.ALURTL

valu_fir: init
	mill -i opengpgpu.runMain opengpgpu.pipeline.VectorALUFIR

.phony: test
