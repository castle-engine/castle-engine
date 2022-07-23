#version 130

uniform mat4 castle_ModelViewMatrix;
uniform mat4 castle_ProjectionMatrix;

attribute vec4 castle_Vertex;
attribute vec4 castle_MultiTexCoord0;

varying vec4 coord;

void main(void) {
  coord = castle_MultiTexCoord0;
  gl_Position = castle_ProjectionMatrix * (castle_ModelViewMatrix * castle_Vertex);
}