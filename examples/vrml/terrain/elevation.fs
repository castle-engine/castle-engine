uniform sampler2D tex_sand;
uniform sampler2D tex_bread;
uniform sampler2D tex_rock;
varying vec3 position;
varying vec3 normal;

/* No, these can't be simple constants in the shader, fglrx is f** broken,
   see rants in my other shaders. */
uniform float z0; // below sand
uniform float z1; // below sand + bread
uniform float z2; // below bread
uniform float z3; // below bread + rock
// above only rock

uniform float color_scale;
uniform float tex_scale;

void main(void)
{
  /* calculate tex color by nicely blending textures based on
     height (position.z) */
  vec4 tex;
  if (position.z <= z0)
    tex = texture2D(tex_sand, position.xy); else
  if (position.z <= z1)
    tex = mix( texture2D(tex_sand, position.xy),
               texture2D(tex_bread, position.xy),
               (position.z - z0) / (z1 - z0)); else
  if (position.z <= z2)
    tex = texture2D(tex_bread, position.xy); else
  if (position.z <= z3)
    tex = mix( texture2D(tex_bread, position.xy),
               texture2D(tex_rock, position.xy),
               (position.z - z2) / (z3 - z2)); else
    tex = texture2D(tex_rock, position.xy);

  /* use normal to darken color on steep faces.
     Steep face =~ one with small n.z */
  vec3 n = normalize(normal);

  tex *= n.z;

  gl_FragColor = (gl_Color * color_scale) + (tex * tex_scale);
}
