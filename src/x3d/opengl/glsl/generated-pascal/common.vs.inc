{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "common.vs" }
'/* Vertex shader utilities used by both Gouraud and Phong shading.' + LineEnding +
'' + LineEnding +
'   Used by ../castlerendererinternalshader.pas to construct the final shader.' + LineEnding +
'   When you change this file, rerun `make'' and then recompile Pascal sources.' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'#ifdef CASTLE_NEEDS_MIRROR_PLANE_TEX_COORDS' + LineEnding +
'uniform vec4 castle_NormalizedPlane;' + LineEnding +
'uniform vec3 castle_CameraPositionOnPlane;' + LineEnding +
'uniform vec3 castle_CameraSide;' + LineEnding +
'uniform vec3 castle_CameraUp;' + LineEnding +
'uniform vec4 castle_FrustumDimensions;' + LineEnding +
'' + LineEnding +
'/* Calculate texture coordinates matching ViewpointMirror texture projection. */' + LineEnding +
'vec3 castle_mirror_plane_tex_coords(const in vec4 vertex_world)' + LineEnding +
'{' + LineEnding +
'  /* The same implemented on CPU in Pascal (with Coord = vertex_world):' + LineEnding +
'' + LineEnding +
'      PlaneCoord := PointOnPlaneClosestToPoint(Plane, Coord) - CameraPositionOnPlane;' + LineEnding +
'      PlaneCoordProjected := Vector2(' + LineEnding +
'        TVector3.DotProduct(PlaneCoord, CameraSide),' + LineEnding +
'        TVector3.DotProduct(PlaneCoord, CameraUp)' + LineEnding +
'      );' + LineEnding +
'      Exit(Vector2(' + LineEnding +
'        (PlaneCoordProjected.Data[0] - FrustumDimensions.Left) / FrustumDimensions.Width,' + LineEnding +
'        (PlaneCoordProjected.Data[1] - FrustumDimensions.Bottom) / FrustumDimensions.Height' + LineEnding +
'      ));' + LineEnding +
'  */' + LineEnding +
'' + LineEnding +
'  float plane_d =' + LineEnding +
'    - dot(castle_NormalizedPlane.xyz, vertex_world.xyz)' + LineEnding +
'    - castle_NormalizedPlane.w;' + LineEnding +
'  vec3 vertex_on_plane = vertex_world.xyz + castle_NormalizedPlane.xyz * plane_d;' + LineEnding +
'' + LineEnding +
'  vertex_on_plane -= castle_CameraPositionOnPlane;' + LineEnding +
'' + LineEnding +
'  vec2 projected = vec2(' + LineEnding +
'    dot(vertex_on_plane, castle_CameraSide),' + LineEnding +
'    dot(vertex_on_plane, castle_CameraUp));' + LineEnding +
'' + LineEnding +
'  return vec3(' + LineEnding +
'    (projected.x - castle_FrustumDimensions.x) / castle_FrustumDimensions.z,' + LineEnding +
'    (projected.y - castle_FrustumDimensions.y) / castle_FrustumDimensions.w,' + LineEnding +
'    0.0);' + LineEnding +
'}' + LineEnding +
'#endif' + LineEnding +
''
