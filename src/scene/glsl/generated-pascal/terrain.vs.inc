{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/terrain.vs" }
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
'void PLUG_vertex_object_space(' + LineEnding +
'  const in vec4 vertex_object, const in vec3 normal_object)' + LineEnding +
'{' + LineEnding +
'  terrain_position = vec3(vertex_object);' + LineEnding +
'  terrain_normal = normal_object;' + LineEnding +
'}' + LineEnding +
''
