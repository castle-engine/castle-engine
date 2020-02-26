/* Shader code used for adding light source contribution for Material
   (Phong lighting model).
   This is used by both desktop OpenGL and OpenGLES.
   This is used in both Gouraud and Phong shading,
   so it may go either in vertex or fragment shader.
*/

/* Light source position (or direction, if not LIGHT<Light>_TYPE_POSITIONAL)
   in eye coordinates. */
uniform vec3 castle_LightSource<Light>Position;

/* SpotLight specific parameters */
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
uniform vec3 castle_LightSource<Light>AmbientColor;
#endif
uniform vec3 castle_LightSource<Light>Color;

#ifdef LIGHT<Light>_HAS_ATTENUATION
/* Attenuation: constant, linear, quadratic. */
uniform vec3 castle_LightSource<Light>Attenuation;
#endif

#ifdef LIGHT<Light>_HAS_RADIUS
uniform float castle_LightSource<Light>Radius;
#endif

// In case of OpenGLES, all shader code is glued, so this is already declared
#ifndef GL_ES
uniform vec3 castle_MaterialAmbient;
uniform vec3 castle_MaterialSpecular;
uniform float castle_MaterialShininess;
#endif

/* Add light contribution.
   Note: this never changes color.a.
*/
void PLUG_add_light(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  /* Calculated color from
     Material.baseColor/transparency (or ColorRGBA node) * base texture.
     Contains complete "base/transparency" information that is independent of light source.
     In case of Gouraud shading it is not multiplied by the base texture
     (because it cannot be, as we're on vertex shader). */
  const in vec4 material_base_alpha)
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
  vec3 light_color =
#ifdef LIGHT<Light>_HAS_AMBIENT
  castle_LightSource<Light>AmbientColor * castle_MaterialAmbient;
  /* PLUG: material_light_ambient (light_color) */
#else
  vec3(0.0);
#endif

  /* add diffuse term */
  vec3 diffuse = castle_LightSource<Light>Color * material_base_alpha.rgb;

  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);
  light_color += diffuse * diffuse_factor;

  color.rgb += light_color * scale;
}
