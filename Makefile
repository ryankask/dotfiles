link_cmd = ln -s $(realpath $(script)) ~/bin
link_home_bin_scripts := $(foreach script,$(wildcard bin/*),$(link_cmd))

.PHONY: bin
bin:
	$(link_home_bin_scripts)
