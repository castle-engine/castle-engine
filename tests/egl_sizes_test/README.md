Simple C application to test the size of various types used by C, EGL, X.

Output (on x86_64 CPU):

```
$ make
$ ./egl_sizes_test
int size: 4
unsigned int size: 4
unsigned long size: 8
EGLint size: 4
EGLNativeDisplayType size: 8
EGLNativePixmapType size: 8
EGLNativeWindowType size: 8
XID size: 8
Pixmap size: 8
Window size: 8
```

See castleinternalegl.pas for some comments why this is useful.
