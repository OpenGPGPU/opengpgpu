toy_alu:
	mill -i opengpgpu.runMain  opengpgpu.alu.ALURTL
test:
	mill -i opengpgpu.test

.phony: test
