/* This shader code will be used for adding light source contribution.
   It really only makes sense when used from within VRMLShader unit,
   it's not a standalone piece of code. */

vec3 light_dir;

/* Check gl_LightSource[light_number].position first, as we want to add nothing
   (not even ambient term) when were outside of spot light cone. */
if (gl_LightSource[light_number].position.w != 0.0)
{
  /* we assume in this case gl_LightSource[light_number].position.w == 1,
     so there's no need to divide by it. This is true for our VRML/X3D
     lights. */
  /* positional light */
  light_dir = normalize(gl_LightSource[light_number].position.xyz - vec3(vertex_eye));

  /* non-spot lights have always cutoff = 180, with cos = -1,
     so the check below will always be false. No need to explicitly
     compare with -1, nice. */
  if (dot(normalize(gl_LightSource[light_number].spotDirection), -light_dir) <
      gl_LightSource[light_number].spotCosCutoff)
    return;
} else
{
  /* directional light */
  light_dir = normalize(gl_LightSource[light_number].position.xyz);
}

float scale = 1.0;
/* PLUG: light-scale (scale) (inout float scale) */

/* add ambient term */
vec4 light_color = gl_SideLightProduct[light_number].ambient;

/* add diffuse term */
light_color += gl_SideLightProduct[light_number].diffuse
  * max(dot(normal_eye, light_dir), 0.0);

/* add specular term */
vec3 reflect = normalize(-reflect(light_dir, normal_eye));
/* vertex to camera direction = camera pos - vertex pos.
   We work in eye space here, so camera pos = always zero. */
vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));
light_color += gl_SideLightProduct[light_number].specular
  * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);

color += light_color * scale;
