{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/add_light.glsl" }
'/* Shader code used for adding light source contribution for Material' + LineEnding +
'   (Phong lighting model).' + LineEnding +
'   This is used by both desktop OpenGL and OpenGLES.' + LineEnding +
'   This is used in both Gouraud and Phong shading,' + LineEnding +
'   so it may go either in vertex or fragment shader.' + LineEnding +
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
'#ifdef LIGHT<Light>_HAS_AMBIENT' + LineEnding +
'uniform vec3 castle_LightSource<Light>AmbientColor;' + LineEnding +
'#endif' + LineEnding +
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
'// In case of OpenGLES, all shader code is glued, so this is already declared' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'uniform vec3 castle_MaterialAmbient;' + LineEnding +
'uniform vec3 castle_MaterialSpecular;' + LineEnding +
'uniform float castle_MaterialShininess;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'/* Add light contribution.' + LineEnding +
'   Note: this never changes color.a.' + LineEnding +
'*/' + LineEnding +
'void PLUG_add_light(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye,' + LineEnding +
'  /* Calculated color from' + LineEnding +
'     Material.baseColor/transparency (or ColorRGBA node) * base texture.' + LineEnding +
'     Contains complete "base/transparency" information that is independent of light source.' + LineEnding +
'     In case of Gouraud shading it is not multiplied by the base texture' + LineEnding +
'     (because it cannot be, as we''re on vertex shader). */' + LineEnding +
'  const in vec4 material_base_alpha)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir;' + LineEnding +
'' + LineEnding +
'/* Calculate light_dir */' + LineEnding +
'#ifdef LIGHT<Light>_TYPE_POSITIONAL' + LineEnding +
'  light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);' + LineEnding +
'  float distance_to_light = length(light_dir);' + LineEnding +
'  light_dir /= distance_to_light;' + LineEnding +
'#else' + LineEnding +
'  light_dir = normalize(castle_LightSource<Light>Position);' + LineEnding +
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
'  scale *= clamp(' + LineEnding +
'    (                    acos(spot_cos) - cutOffAngle) /' + LineEnding +
'    (castle_LightSource<Light>BeamWidth - cutOffAngle),' + LineEnding +
'    0.0, 1.0);' + LineEnding +
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
'  castle_LightSource<Light>AmbientColor * castle_MaterialAmbient;' + LineEnding +
'  /* PLUG: material_light_ambient (light_color) */' + LineEnding +
'#else' + LineEnding +
'  vec3(0.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add diffuse term */' + LineEnding +
'  vec3 diffuse = castle_LightSource<Light>Color * material_base_alpha.rgb;' + LineEnding +
'' + LineEnding +
'  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);' + LineEnding +
'  light_color += diffuse * diffuse_factor;' + LineEnding +
'' + LineEnding +
'  color.rgb += light_color * scale;' + LineEnding +
'}' + LineEnding +
''
