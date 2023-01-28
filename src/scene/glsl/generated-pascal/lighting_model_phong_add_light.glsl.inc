{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_phong/add_light.glsl" }
'/*' + LineEnding +
'  Copyright 2010-2022 Michalis Kamburelis.' + LineEnding +
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
'  Shader code used for adding light source contribution for Material' + LineEnding +
'  (Phong lighting model).' + LineEnding +
'  This is used by both desktop OpenGL and OpenGLES.' + LineEnding +
'  This is used in both Gouraud and Phong shading,' + LineEnding +
'  so it may go either in vertex or fragment shader.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Punctual (PointLight, DirectionalLight, SpotLight) parameters */' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_PUNCTUAL' + LineEnding +
'  /* Light source position (or direction, if not LIGHT<Light>_TYPE_POSITIONAL)' + LineEnding +
'     in eye coordinates. */' + LineEnding +
'  uniform vec3 castle_LightSource<Light>Position;' + LineEnding +
'' + LineEnding +
'  /* SpotLight specific parameters */' + LineEnding +
'  #ifdef LIGHT<Light>_TYPE_SPOT' + LineEnding +
'    uniform vec3 castle_LightSource<Light>SpotDirection;' + LineEnding +
'    uniform float castle_LightSource<Light>SpotCosCutoff;' + LineEnding +
'    #ifdef LIGHT<Light>_HAS_BEAM_WIDTH' + LineEnding +
'      /* In radians. Note that this differs from gl_LightSource[<Light>].spotCutoff' + LineEnding +
'         that is in degrees. */' + LineEnding +
'      uniform float castle_LightSource<Light>SpotCutoff;' + LineEnding +
'      uniform float castle_LightSource<Light>BeamWidth;' + LineEnding +
'    #endif' + LineEnding +
'    #ifdef LIGHT<Light>_HAS_SPOT_EXPONENT' + LineEnding +
'      uniform float castle_LightSource<Light>SpotExponent;' + LineEnding +
'    #endif' + LineEnding +
'  #endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_AMBIENT' + LineEnding +
'uniform vec3 castle_LightSource<Light>AmbientColor;' + LineEnding +
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
'/* Add light contribution.' + LineEnding +
'   Note: this never changes color.a.' + LineEnding +
'*/' + LineEnding +
'void PLUG_add_light(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye,' + LineEnding +
'  const in MaterialInfo material_info)' + LineEnding +
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
'  /* add ambient term */' + LineEnding +
'  vec3 light_color =' + LineEnding +
'#ifdef LIGHT<Light>_HAS_AMBIENT' + LineEnding +
'  castle_LightSource<Light>AmbientColor * material_info.ambient;' + LineEnding +
'#else' + LineEnding +
'  vec3(0.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add diffuse term */' + LineEnding +
'  vec3 diffuse = castle_LightSource<Light>Color * material_info.diffuse_alpha.rgb;' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_ENVIRONMENT' + LineEnding +
'  diffuse *= textureCube(diffuseTexture, light_dir).rgb;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);' + LineEnding +
'  light_color += diffuse * diffuse_factor;' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT<Light>_HAS_SPECULAR' + LineEnding +
'  /* add specular term */' + LineEnding +
'  /* halfVector is an average of' + LineEnding +
'     - normalize(light position - vertex_eye) (we already have this' + LineEnding +
'       in light_dir) and' + LineEnding +
'     - normalize(camera position - vertex_eye)' + LineEnding +
'       (and camera position == zero in camera space). */' + LineEnding +
'  vec3 halfVector = normalize(light_dir - normalize(vec3(vertex_eye)));' + LineEnding +
'  if (diffuse_factor != 0.0) {' + LineEnding +
'    vec3 specular_color = castle_LightSource<Light>Color * material_info.specular;' + LineEnding +
'' + LineEnding +
'    #ifdef LIGHT<Light>_TYPE_ENVIRONMENT' + LineEnding +
'      specular_color *= textureCube(specularTexture, light_dir).rgb;' + LineEnding +
'    #endif' + LineEnding +
'' + LineEnding +
'    light_color += specular_color *' + LineEnding +
'      pow(max(dot(halfVector, normal_eye), 0.0), material_info.shininess);' + LineEnding +
'  }' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  color.rgb += light_color * scale;' + LineEnding +
'}' + LineEnding +
''
