toy_alu:
	mill -i opengpgpu.runMain  opengpgpu.alu.ALURTL
test:
	mill -i opengpgpu.test

alu:
	mill -i opengpgpu.runMain  opengpgpu.pipeline.ALURTL

valu_fir:
	mill -i opengpgpu.runMain opengpgpu.pipeline.VectorALUFIR

.phony: test
