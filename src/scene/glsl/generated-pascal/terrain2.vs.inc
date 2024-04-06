{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain2.vs" }
'/* OpenGL shader effect (used to enhance the Castle Game Engine shaders,' + LineEnding +
'   see https://castle-engine.io/compositing_shaders.php ),' + LineEnding +
'   applied over terrain.' + LineEnding +
'' + LineEnding +
'   This simply saves position and normal in object space,' + LineEnding +
'   to be used by terrain.fs code. */' + LineEnding +
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
'  //if (vertex_object.x < 50.)' + LineEnding +
'  //  vertex_object.y = 32.0;' + LineEnding +
'  vec2 texCoord = vec2(vertex_object.x/terrain_size.x, 1 - vertex_object.z/terrain_size.y);' + LineEnding +
'  vertex_object.y = texture2D(heightTexture, texCoord).r * 10.0;' + LineEnding +
'  ' + LineEnding +
'' + LineEnding +
'  terrain_position = vec3(vertex_object);' + LineEnding +
'  terrain_normal = normal_object;' + LineEnding +
'}' + LineEnding +
''
