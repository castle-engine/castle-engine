# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.
help_message.inc: help_message.txt
	file_to_pascal_string help_message.txt help_message.inc

# Simple install.
# You may as well symlink to /usr/local/share/lets_take_a_walk, for system-wide install.
install:
	rm -f $(HOME)/.lets_take_a_walk.data
	ln -s $(shell pwd) $(HOME)/.lets_take_a_walk.data

# Run also "dircleaner . clean" here to really clean
.PHONY: clean
clean:
	rm -f lets_take_a_walk     devel/vrml/process_base_b \
	      lets_take_a_walk.exe devel/vrml/process_base_b.exe
