/* This shader code will be used for adding light source contribution. */

vec3 light_dir;

/* Check light_source.position first, as we want to add nothing
   (not even ambient term) when were outside of spot light cone. */
if (light_source.position.w != 0.0)
{
  /* we assume in this case light_source.position.w == 1,
     so there's no need to divide by it. This is true for our VRML/X3D
     lights. */
  /* positional light */
  light_dir = normalize(light_source.position.xyz - vec3(vertex_eye));

  /* non-spot lights have always cutoff = 180, with cos = -1,
     so the check below will always be false. No need to explicitly
     compare with -1, nice. */
  if (dot(normalize(light_source.spotDirection), -light_dir) <
      light_source.spotCosCutoff)
    return;
} else
{
  /* directional light */
  light_dir = normalize(light_source.position.xyz);
}

/* add ambient term */
color += light_products.ambient;

/* add diffuse term */
color += light_products.diffuse
  * max(dot(normal_eye, light_dir), 0.0);

/* add specular term */
vec3 reflect = normalize(-reflect(light_dir, normal_eye));
/* vertex to camera direction = camera pos - vertex pos.
   We work in eye space here, so camera pos = always zero. */
vec3 vertex_to_camera_dir = normalize(-vec3(vertex_eye));
color += light_products.specular
  * pow(max(dot(reflect, vertex_to_camera_dir), 0.0), material.shininess);
