MAKEFLAGS=-r

link_cmd = ln -s $(realpath $(script)) ~/bin;
link_home_bin_scripts := $(foreach script,$(wildcard bin/*),$(link_cmd))

all:
	@echo "Use make with a specific target"

bin:
	$(link_home_bin_scripts)

.PHONY: all bin
