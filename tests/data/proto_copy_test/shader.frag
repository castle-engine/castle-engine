#version 130

uniform sampler2D tex;
uniform sampler2D palette;

varying vec4 coord;

void main(void) {
  int index = int(round(texture2D(tex, coord.xy).r * 255.0));
  if (index == 255) discard;
  gl_FragColor = texelFetch(palette, ivec2(index, 0), 0);
}