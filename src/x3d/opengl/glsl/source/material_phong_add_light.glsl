/* Shader code used for adding light source contribution for Material
   (Phong lighting model).
   This is used by both desktop OpenGL and OpenGLES.
   This is used in both Gouraud and Phong shading,
   so it may go either in vertex or fragment shader.
*/

/* Light source position (or direction, if not LIGHT<Light>_TYPE_POSITIONAL)
   in eye coordinates. */
uniform vec3 castle_LightSource<Light>Position;
#ifdef LIGHT<Light>_TYPE_SPOT
uniform vec3 castle_LightSource<Light>SpotDirection;
uniform float castle_LightSource<Light>SpotCosCutoff;
#ifdef LIGHT<Light>_HAS_BEAM_WIDTH
/* In radians. Note that this differs from gl_LightSource[<Light>].spotCutoff
   that is in degrees. */
uniform float castle_LightSource<Light>SpotCutoff;
uniform float castle_LightSource<Light>BeamWidth;
#endif
#ifdef LIGHT<Light>_HAS_SPOT_EXPONENT
uniform float castle_LightSource<Light>SpotExponent;
#endif
#endif

#ifdef LIGHT<Light>_HAS_AMBIENT
/* Multiplied colors of light source ambient and material ambient. */
uniform vec4 castle_SideLightProduct<Light>Ambient;
#endif
uniform vec4 castle_LightSource<Light>Diffuse;

#ifdef LIGHT<Light>_HAS_SPECULAR
/* Multiplied colors of light source specular and material specular. */
uniform vec4 castle_SideLightProduct<Light>Specular;
#endif

#ifdef LIGHT<Light>_HAS_ATTENUATION
/* Attenuation: constant, linear, quadratic. */
uniform vec3 castle_LightSource<Light>Attenuation;
#endif

#ifdef LIGHT<Light>_HAS_RADIUS
uniform float castle_LightSource<Light>Radius;
#endif

// In case of OpenGLES, all shader code is glued, so this is already declared
#ifndef GL_ES
uniform float castle_MaterialShininess;
vec4 castle_material_complete_diffuse_alpha;
#endif

/* Add light contribution.
   Note: this never changes color.a.
*/
void PLUG_add_light(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye)
{
  vec3 light_dir;

/* Calculate light_dir */
#ifdef LIGHT<Light>_TYPE_POSITIONAL
  light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);
  float distance_to_light = length(light_dir);
  light_dir /= distance_to_light;
#else
  light_dir = normalize(castle_LightSource<Light>Position);
#endif

#ifdef LIGHT<Light>_TYPE_SPOT
  /* Check SpotCosCutoff first, as we want to add nothing
     (not even ambient term) when were outside of spot light cone. */

  float spot_cos = dot(normalize(castle_LightSource<Light>SpotDirection), -light_dir);
  if (spot_cos < castle_LightSource<Light>SpotCosCutoff)
    return;
#endif

  float scale = 1.0;
  /* PLUG: light_scale (scale, normal_eye, light_dir) */

#ifdef LIGHT<Light>_TYPE_SPOT
#ifdef LIGHT<Light>_HAS_BEAM_WIDTH
  /* calculate spot following VRML 2.0/X3D idea of beamWidth */
  float cutOffAngle = castle_LightSource<Light>SpotCutoff;
  scale *= clamp(
    (                    acos(spot_cos) - cutOffAngle) /
    (castle_LightSource<Light>BeamWidth - cutOffAngle),
    0.0, 1.0);
#endif

#ifdef LIGHT<Light>_HAS_SPOT_EXPONENT
  /* calculate spot like fixed-function pipeline, using exponent */
  scale *= pow(spot_cos, castle_LightSource<Light>SpotExponent);
#endif
#endif

#ifdef LIGHT<Light>_HAS_ATTENUATION
  scale /= max(1.0,
           castle_LightSource<Light>Attenuation.x +
           castle_LightSource<Light>Attenuation.y * distance_to_light +
           castle_LightSource<Light>Attenuation.z * distance_to_light * distance_to_light);
#endif

#ifdef LIGHT<Light>_HAS_RADIUS
  if (distance_to_light >= castle_LightSource<Light>Radius)
    scale = 0.0;
#endif

  /* add ambient term */
  vec4 light_color =
#ifdef LIGHT<Light>_HAS_AMBIENT
  castle_SideLightProduct<Light>Ambient;
  /* PLUG: material_light_ambient (light_color) */
#else
  vec4(0.0);
#endif

  /* add diffuse term */
  vec4 diffuse = castle_LightSource<Light>Diffuse * castle_material_complete_diffuse_alpha;

  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye) */
  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);
  light_color += diffuse * diffuse_factor;

#ifdef LIGHT<Light>_HAS_SPECULAR
  /* add specular term */
  /* halfVector is an average of
     - normalize(light position - vertex_eye) (we already have this
       in light_dir) and
     - normalize(camera position - vertex_eye)
       (and camera position == zero in camera space). */
  vec3 halfVector = normalize(light_dir - normalize(vec3(vertex_eye)));
  if (diffuse_factor != 0.0) {
    vec4 specular_color = castle_SideLightProduct<Light>Specular;
    /* PLUG: material_light_specular (specular_color) */
    float material_shininess = castle_MaterialShininess;
    /* PLUG: material_shininess (material_shininess) */
    light_color += specular_color *
      pow(max(dot(halfVector, normal_eye),
        0.0), material_shininess);
  }
#endif

  color += light_color * scale;
}
