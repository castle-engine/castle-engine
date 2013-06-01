# Note about file_to_pascal_string: it's another program of mine,
# you can get it from pasdoc [http://pasdoc.sourceforge.net/] sources.
help_message.inc: help_message.txt
	file_to_pascal_string help_message.txt help_message.inc

# Simple install.
# You may as well symlink data to /usr/local/share/lets_take_a_walk,
# for system-wide install.
install:
	rm -f $(HOME)/.local/share/lets_take_a_walk
	ln -s $(shell pwd)/data $(HOME)/.local/share/lets_take_a_walk
