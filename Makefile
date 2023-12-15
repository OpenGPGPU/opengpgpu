init:
	git submodule update --init --recursive

toy_alu: init
	mill.0.10 -i opengpgpu.runMain  opengpgpu.alu.ALURTL

test: init
	mill.0.10 -i opengpgpu.test

z_test: init
	mill.0.10 -i opengpgpu.test -z $(OPT)

alu: init
	mill.0.10 -i opengpgpu.runMain  opengpgpu.pipeline.ALURTL

valu_fir: init
	mill.0.10 -i opengpgpu.runMain opengpgpu.pipeline.VectorALUFIR

.phony: test
