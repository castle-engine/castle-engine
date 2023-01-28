{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/add_light.glsl" }
'/*' + LineEnding +
'  Copyright 2020-2022 Michalis Kamburelis and glTF-Sample-Viewer authors.' + LineEnding +
'' + LineEnding +
'  This file is part of "Castle Game Engine".' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is free software; see the file COPYING.txt,' + LineEnding +
'  included in this distribution, for details about the copyright.' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is distributed in the hope that it will be useful,' + LineEnding +
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + LineEnding +
'' + LineEnding +
'  ----------------------------------------------------------------------------' + LineEnding +
'' + LineEnding +
'  Shader code used for adding light source contribution for PhysicalMaterial' + LineEnding +
'  (PBR lighting model).' + LineEnding +
'  This is used by both desktop OpenGL and OpenGLES.' + LineEnding +
'' + LineEnding +
'  In the future this may be used in both Gouraud and Phong shading,' + LineEnding +
'  so it may go either in vertex or fragment shader.' + LineEnding +
'  For now: since PhysicalMaterial now forces Phong shading, this is only used in Phong shading.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Light source position (or direction, if not LIGHT<Light>_TYPE_POSITIONAL)' + LineEnding +
'   in eye coordinates. */' + LineEnding +
'uniform vec3 castle_LightSource<Light>Position;' + LineEnding +
'' + LineEnding +
'/* SpotLight specific parameters */' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_SPOT' + LineEnding +
'uniform vec3 castle_LightSource<Light>SpotDirection;' + LineEnding +
'uniform float castle_LightSource<Light>SpotCosCutoff;' + LineEnding +
'#ifdef LIGHT<Light>_HAS_BEAM_WIDTH' + LineEnding +
'/* In radians. Note that this differs from gl_LightSource[<Light>].spotCutoff' + LineEnding +
'   that is in degrees. */' + LineEnding +
'uniform float castle_LightSource<Light>SpotCutoff;' + LineEnding +
'uniform float castle_LightSource<Light>BeamWidth;' + LineEnding +
'#endif' + LineEnding +
'#ifdef LIGHT<Light>_HAS_SPOT_EXPONENT' + LineEnding +
'uniform float castle_LightSource<Light>SpotExponent;' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'uniform vec3 castle_LightSource<Light>Color;' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_ATTENUATION' + LineEnding +
'/* Attenuation: constant, linear, quadratic. */' + LineEnding +
'uniform vec3 castle_LightSource<Light>Attenuation;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_RADIUS' + LineEnding +
'uniform float castle_LightSource<Light>Radius;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* EnvironmentLight parameters */' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_ENVIRONMENT' + LineEnding +
'uniform samplerCube diffuseTexture;' + LineEnding +
'uniform samplerCube specularTexture;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'// In case of OpenGLES, all shader code is glued, so this is already declared' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'AngularInfo getAngularInfo(const in vec3 pointToLight, const in vec3 normal, const in vec3 view);' + LineEnding +
'vec3 specularReflection(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);' + LineEnding +
'float visibilityOcclusion(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);' + LineEnding +
'float microfacetDistribution(const in MaterialInfo materialInfo, const in AngularInfo angularInfo);' + LineEnding +
'vec3 diffuse(const in MaterialInfo materialInfo);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Add light contribution.' + LineEnding +
'   Note: this never changes color.a.' + LineEnding +
'*/' + LineEnding +
'void PLUG_add_light(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye,' + LineEnding +
'  const in MaterialInfo material_info,' + LineEnding +
'  const in vec3 view)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir;' + LineEnding +
'' + LineEnding +
'/* Calculate light_dir */' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_PUNCTUAL' + LineEnding +
'  #ifdef LIGHT<Light>_TYPE_POSITIONAL' + LineEnding +
'    light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);' + LineEnding +
'    float distance_to_light = length(light_dir);' + LineEnding +
'    light_dir /= distance_to_light;' + LineEnding +
'  #else' + LineEnding +
'    light_dir = normalize(castle_LightSource<Light>Position);' + LineEnding +
'  #endif' + LineEnding +
'#else' + LineEnding +
'  light_dir = normal_eye;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_SPOT' + LineEnding +
'  /* Check SpotCosCutoff first, as we want to add nothing' + LineEnding +
'     (not even ambient term) when were outside of spot light cone. */' + LineEnding +
'' + LineEnding +
'  float spot_cos = dot(normalize(castle_LightSource<Light>SpotDirection), -light_dir);' + LineEnding +
'  if (spot_cos < castle_LightSource<Light>SpotCosCutoff)' + LineEnding +
'    return;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  float scale = 1.0;' + LineEnding +
'  /* PLUG: light_scale (scale, normal_eye, light_dir) */' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_SPOT' + LineEnding +
'#ifdef LIGHT<Light>_HAS_BEAM_WIDTH' + LineEnding +
'  /* calculate spot following VRML 2.0/X3D idea of beamWidth */' + LineEnding +
'  float cutOffAngle = castle_LightSource<Light>SpotCutoff;' + LineEnding +
'  if (castle_LightSource<Light>BeamWidth < cutOffAngle) {' + LineEnding +
'    scale *= clamp(' + LineEnding +
'      (                    acos(spot_cos) - cutOffAngle) /' + LineEnding +
'      (castle_LightSource<Light>BeamWidth - cutOffAngle),' + LineEnding +
'      0.0, 1.0);' + LineEnding +
'  }' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_SPOT_EXPONENT' + LineEnding +
'  /* calculate spot like fixed-function pipeline, using exponent */' + LineEnding +
'  scale *= pow(spot_cos, castle_LightSource<Light>SpotExponent);' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_ATTENUATION' + LineEnding +
'  scale /= max(1.0,' + LineEnding +
'           castle_LightSource<Light>Attenuation.x +' + LineEnding +
'           castle_LightSource<Light>Attenuation.y * distance_to_light +' + LineEnding +
'           castle_LightSource<Light>Attenuation.z * distance_to_light * distance_to_light);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_RADIUS' + LineEnding +
'  if (distance_to_light >= castle_LightSource<Light>Radius)' + LineEnding +
'    scale = 0.0;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* Main code that calculates per-light-source contribution to the final color.' + LineEnding +
'     Was called "getPointShade" in glTF-Sample-Viewer.' + LineEnding +
'' + LineEnding +
'     light_dir now must be the direction from current vertex to light source.' + LineEnding +
'     It is assumed to be already normalized.' + LineEnding +
'' + LineEnding +
'     view is direction from current vertex to camera, in eye-space,' + LineEnding +
'     already normalized.' + LineEnding +
'     IOW, "normalize(- vertex_eye)" because camera position is zero in eye-space.' + LineEnding +
'  */' + LineEnding +
'  AngularInfo angularInfo = getAngularInfo(light_dir, normal_eye, view);' + LineEnding +
'' + LineEnding +
'  if (angularInfo.NdotL > 0.0 || angularInfo.NdotV > 0.0)' + LineEnding +
'  {' + LineEnding +
'    // Calculate the shading terms for the microfacet specular shading model' + LineEnding +
'    vec3 F = specularReflection(material_info, angularInfo);' + LineEnding +
'    float Vis = visibilityOcclusion(material_info, angularInfo);' + LineEnding +
'    float D = microfacetDistribution(material_info, angularInfo);' + LineEnding +
'' + LineEnding +
'    // Calculation of analytical lighting contribution' + LineEnding +
'    vec3 diffuseContrib = (1.0 - F) * diffuse(material_info);' + LineEnding +
'    vec3 specContrib = F * Vis * D;' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_ENVIRONMENT' + LineEnding +
'  diffuseContrib *= textureCube(diffuseTexture, light_dir).rgb;' + LineEnding +
'  specContrib *= textureCube(specularTexture, light_dir).rgb;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'    // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)' + LineEnding +
'    vec3 light_contribution = angularInfo.NdotL * (diffuseContrib + specContrib);' + LineEnding +
'' + LineEnding +
'    color.rgb += castle_LightSource<Light>Color *' + LineEnding +
'      light_contribution *' + LineEnding +
'      scale;' + LineEnding +
'  }' + LineEnding +
'}' + LineEnding +
''
