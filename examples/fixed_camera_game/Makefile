default:
	@echo 'No default target in this Makefile'

# Simple install.
# You may as well symlink data to /usr/local/share/rift,
# for system-wide install.
install:
	rm -f $(HOME)/.local/share/rift
	ln -s $(shell pwd)/data $(HOME)/.local/share/rift
