egl_sizes_test: egl_sizes_test.c
	$(CC) -o $@ $<

freetype_sizes: freetype_sizes.c
	$(CC) $(shell pkg-config --cflags freetype2) -o $@ $< $(shell pkg-config --libs freetype2)
