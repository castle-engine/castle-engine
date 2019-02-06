{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "template_light.glsl" }
'/* Shader code used for adding light source contribution.' + LineEnding +
'   This is used by both desktop OpenGL and OpenGLES.' + LineEnding +
'   This is used in both Gouraud and Phong shading,' + LineEnding +
'   so it may go either in vertex or fragment shader.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'/* Light source position (or direction, if not LIGHT_TYPE_POSITIONAL)' + LineEnding +
'   in eye coordinates. */' + LineEnding +
'uniform vec3 castle_LightSource<Light>Position;' + LineEnding +
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
'uniform vec3 castle_LightSource<Light>SpotDirection;' + LineEnding +
'uniform float castle_LightSource<Light>SpotCosCutoff;' + LineEnding +
'#ifdef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'/* In radians. Note that this differs from gl_LightSource[<Light>].spotCutoff' + LineEnding +
'   that is in degrees. */' + LineEnding +
'uniform float castle_LightSource<Light>SpotCutoff;' + LineEnding +
'uniform float castle_LightSource<Light>BeamWidth;' + LineEnding +
'#endif' + LineEnding +
'#ifdef LIGHT_HAS_SPOT_EXPONENT' + LineEnding +
'uniform float castle_LightSource<Light>SpotExponent;' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'/* Multiplied colors of light source and material. */' + LineEnding +
'#ifdef LIGHT_HAS_AMBIENT' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Ambient;' + LineEnding +
'#endif' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'uniform vec4 castle_LightSource<Light>Diffuse;' + LineEnding +
'#else' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Diffuse;' + LineEnding +
'#endif' + LineEnding +
'#ifdef LIGHT_HAS_SPECULAR' + LineEnding +
'uniform vec4 castle_SideLightProduct<Light>Specular;' + LineEnding +
'#endif' + LineEnding +
'#ifdef LIGHT_HAS_ATTENUATION' + LineEnding +
'/* Attenuation: constant, linear, quadratic. */' + LineEnding +
'uniform vec3 castle_LightSource<Light>Attenuation;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_RADIUS' + LineEnding +
'uniform float castle_LightSource<Light>Radius;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_SEPARATE_DIFFUSE_TEXTURE' + LineEnding +
'#ifndef GL_ES' + LineEnding +
'void separate_diffuse_apply_texture(inout vec4 fragment_color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye);' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'void PLUG_add_light_contribution(inout vec4 color,' + LineEnding +
'  const in vec4 vertex_eye,' + LineEnding +
'  const in vec3 normal_eye,' + LineEnding +
'  in float material_shininess,' + LineEnding +
'  in vec4 color_per_vertex)' + LineEnding +
'{' + LineEnding +
'  vec3 light_dir;' + LineEnding +
'' + LineEnding +
'/* Calculate light_dir */' + LineEnding +
'#ifdef LIGHT_TYPE_POSITIONAL' + LineEnding +
'  light_dir = castle_LightSource<Light>Position - vec3(vertex_eye);' + LineEnding +
'  float distance_to_light = length(light_dir);' + LineEnding +
'  light_dir /= distance_to_light;' + LineEnding +
'#else' + LineEnding +
'  light_dir = normalize(castle_LightSource<Light>Position);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
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
'#ifdef LIGHT_TYPE_SPOT' + LineEnding +
'#ifdef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'  /* calculate spot following VRML 2.0/X3D idea of beamWidth */' + LineEnding +
'  float cutOffAngle = castle_LightSource<Light>SpotCutoff;' + LineEnding +
'  scale *= clamp(' + LineEnding +
'    (                    acos(spot_cos) - cutOffAngle) /' + LineEnding +
'    (castle_LightSource<Light>BeamWidth - cutOffAngle),' + LineEnding +
'    0.0, 1.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_SPOT_EXPONENT' + LineEnding +
'  /* calculate spot like fixed-function pipeline, using exponent */' + LineEnding +
'  scale *= pow(spot_cos, castle_LightSource<Light>SpotExponent);' + LineEnding +
'#endif' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_ATTENUATION' + LineEnding +
'  scale /= max(1.0,' + LineEnding +
'           castle_LightSource<Light>Attenuation.x +' + LineEnding +
'           castle_LightSource<Light>Attenuation.y * distance_to_light +' + LineEnding +
'           castle_LightSource<Light>Attenuation.z * distance_to_light * distance_to_light);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_RADIUS' + LineEnding +
'  if (distance_to_light >= castle_LightSource<Light>Radius)' + LineEnding +
'    scale = 0.0;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add ambient term */' + LineEnding +
'  vec4 light_color =' + LineEnding +
'#ifdef LIGHT_HAS_AMBIENT' + LineEnding +
'  castle_SideLightProduct<Light>Ambient;' + LineEnding +
'  /* PLUG: material_light_ambient (light_color) */' + LineEnding +
'#else' + LineEnding +
'  vec4(0.0);' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  /* add diffuse term */' + LineEnding +
'  vec4 diffuse =' + LineEnding +
'#ifdef COLOR_PER_VERTEX' + LineEnding +
'  castle_LightSource<Light>Diffuse * color_per_vertex;' + LineEnding +
'#else' + LineEnding +
'  castle_SideLightProduct<Light>Diffuse;' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  #ifdef CASTLE_SEPARATE_DIFFUSE_TEXTURE' + LineEnding +
'  separate_diffuse_apply_texture(diffuse, vertex_eye, normal_eye);' + LineEnding +
'  #endif' + LineEnding +
'' + LineEnding +
'  /* PLUG: material_light_diffuse (diffuse, vertex_eye, normal_eye) */' + LineEnding +
'  float diffuse_factor = max(dot(normal_eye, light_dir), 0.0);' + LineEnding +
'  light_color += diffuse * diffuse_factor;' + LineEnding +
'' + LineEnding +
'#ifdef LIGHT_HAS_SPECULAR' + LineEnding +
'  /* add specular term */' + LineEnding +
'  /* halfVector is an average of' + LineEnding +
'     - normalize(light position - vertex_eye) (we already have this' + LineEnding +
'       in light_dir) and' + LineEnding +
'     - normalize(camera position - vertex_eye)' + LineEnding +
'       (and camera position == zero in camera space). */' + LineEnding +
'  vec3 halfVector = normalize(light_dir - normalize(vec3(vertex_eye)));' + LineEnding +
'  if (diffuse_factor != 0.0) {' + LineEnding +
'    vec4 specular_color = castle_SideLightProduct<Light>Specular;' + LineEnding +
'    /* PLUG: material_light_specular (specular_color) */' + LineEnding +
'    /* PLUG: material_shininess (material_shininess) */' + LineEnding +
'    light_color += specular_color *' + LineEnding +
'      pow(max(dot(halfVector, normal_eye),' + LineEnding +
'        0.0), material_shininess);' + LineEnding +
'  }' + LineEnding +
'#endif' + LineEnding +
'' + LineEnding +
'  color += light_color * scale;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Undefine lights symbols, since for OpenGL ES all the shader parts' + LineEnding +
'   are concatenated into a single string. */' + LineEnding +
'#undef LIGHT_TYPE_POSITIONAL' + LineEnding +
'#undef LIGHT_TYPE_SPOT' + LineEnding +
'#undef LIGHT_HAS_AMBIENT' + LineEnding +
'#undef LIGHT_HAS_SPECULAR' + LineEnding +
'#undef LIGHT_HAS_ATTENUATION' + LineEnding +
'#undef LIGHT_HAS_RADIUS' + LineEnding +
'#undef LIGHT_HAS_BEAM_WIDTH' + LineEnding +
'#undef LIGHT_HAS_SPOT_EXPONENT' + LineEnding +
''
