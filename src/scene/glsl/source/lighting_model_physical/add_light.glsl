/*
  Copyright 2020-2022 Michalis Kamburelis and glTF-Sample-Viewer authors.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Shader code used for adding light source contribution for PhysicalMaterial
  (PBR lighting model).
  This is used by both desktop OpenGL and OpenGLES.

  In the future this may be used in both Gouraud and Phong shading,
  so it may go either in vertex or fragment shader.
  For now: since PhysicalMaterial now forces Phong shading, this is only used in Phong shading.
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

uniform vec3 castle_LightSource<Light>Color;

#ifdef LIGHT<Light>_HAS_ATTENUATION
/* Attenuation: constant, linear, quadratic. */
uniform vec3 castle_LightSource<Light>Attenuation;
#endif

#ifdef LIGHT<Light>_HAS_RADIUS
uniform float castle_LightSource<Light>Radius;
#endif

/* EnvironmentLight parameters */
#ifdef LIGHT<Light>_TYPE_ENVIRONMENT
uniform samplerCube diffuseTexture;
uniform samplerCube specularTexture;
#endif

// In case of OpenGLES, all shader code is glued, so this is already declared
#ifndef GL_ES
AngularInfo getAngularInfo(const in vec3 pointToLight, const in vec3 normal, const in vec3 view);
vec3 specularReflection(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);
float visibilityOcclusion(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);
float microfacetDistribution(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);
vec3 diffuse(const in MaterialInfo materialInfo);
#endif

/* Add light contribution.
   Note: this never changes color.a.
*/
void PLUG_add_light(inout vec4 color,
  const in vec4 vertex_eye,
  const in vec3 normal_eye,
  const in MaterialInfo material_info,
  const in vec3 view)
{
  vec3 light_dir;

/* Calculate light_dir */
#ifdef LIGHT<Light>_TYPE_PUNCTUAL
  #ifdef LIGHT<Light>_TYPE_POSITIONAL
    light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);
    float distance_to_light = length(light_dir);
    light_dir /= distance_to_light;
  #else
    light_dir = normalize(castle_LightSource<Light>Position);
  #endif
#else
  light_dir = normal_eye;
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
  if (castle_LightSource<Light>BeamWidth < cutOffAngle) {
    scale *= clamp(
      (                    acos(spot_cos) - cutOffAngle) /
      (castle_LightSource<Light>BeamWidth - cutOffAngle),
      0.0, 1.0);
  }
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

  /* Main code that calculates per-light-source contribution to the final color.
     Was called "getPointShade" in glTF-Sample-Viewer.

     light_dir now must be the direction from current vertex to light source.
     It is assumed to be already normalized.

     view is direction from current vertex to camera, in eye-space,
     already normalized.
     IOW, "normalize(- vertex_eye)" because camera position is zero in eye-space.
  */
  AngularInfo angularInfo = getAngularInfo(light_dir, normal_eye, view);

  if (angularInfo.NdotL > 0.0 || angularInfo.NdotV > 0.0)
  {
    // Calculate the shading terms for the microfacet specular shading model
    vec3 F = specularReflection(material_info, angularInfo);
    float Vis = visibilityOcclusion(material_info, angularInfo);
    float D = microfacetDistribution(material_info, angularInfo);

    // Calculation of analytical lighting contribution
    vec3 diffuseContrib = (1.0 - F) * diffuse(material_info);
    vec3 specContrib = F * Vis * D;

#ifdef LIGHT<Light>_TYPE_ENVIRONMENT
  diffuseContrib *= textureCube(diffuseTexture, light_dir).rgb;
  specContrib *= textureCube(specularTexture, light_dir).rgb;
#endif

    // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
    vec3 light_contribution = angularInfo.NdotL * (diffuseContrib + specContrib);

    color.rgb += castle_LightSource<Light>Color *
      light_contribution *
      scale;
  }
}
