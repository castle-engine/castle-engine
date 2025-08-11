{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain_heights_in_texture.vs" }
'/* Shader effect for a terrain.' + LineEnding +
'   Modified version of terrain.vs that uses heightTexture' + LineEnding +
'   to move the vertexes to show the actual geometry of the terrain. */' + LineEnding +
'' + LineEnding +
'varying vec3 terrain_position;' + LineEnding +
'varying vec3 terrain_normal;' + LineEnding +
'' + LineEnding +
'uniform sampler2D heightTexture;' + LineEnding +
'uniform vec2 terrain_size;' + LineEnding +
'' + LineEnding +
'void PLUG_vertex_object_space_change(' + LineEnding +
'  inout vec4 vertex_object,' + LineEnding +
'  inout vec3 normal_object)' + LineEnding +
'{' + LineEnding +
'  vec2 texCoord = vec2(vertex_object.x/terrain_size.x, 1 - vertex_object.z/terrain_size.y);' + LineEnding +
'  vertex_object.y = texture2D(heightTexture, texCoord).r * 10.0;' + LineEnding +
'' + LineEnding +
'  terrain_position = vec3(vertex_object);' + LineEnding +
'  terrain_normal = normal_object;' + LineEnding +
'}' + LineEnding +
''
