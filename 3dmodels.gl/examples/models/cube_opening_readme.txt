I wanted to show that models with textures are animated OK also.

This is not so obvious --- note that each animation frame along the
way is separately optimized, but at the same time they all have to
share the same texture image loaded from file, and they have to share
the same OpenGL texture index used to render this texture. Otherwise
my programs (inside VRMLNodes where texture is loaded from file to memory)
or OpenGL (where textures are provided, and are internally stored by OpenGL)
could eat *very very large* amounts of memory.