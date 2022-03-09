{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/common.vs" }
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
'vec3 castle_generate_tex_coords_mirror_plane(const in vec4 vertex_world)' + LineEnding +
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
'' + LineEnding +
'/* Sphere mapping in GLSL adapted from' + LineEnding +
'   http://www.ozone3d.net/tutorials/glsl_texturing_p04.php#part_41' + LineEnding +
'   by Jerome Guinot aka ''JeGX'', many thanks!' + LineEnding +
'*/' + LineEnding +
'vec2 castle_generate_tex_coords_sphere(const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  vec3 r = reflect( normalize(vec3(vertex_eye)), normal_eye );' + LineEnding +
'  float m = 2.0 * sqrt( r.x*r.x + r.y*r.y + (r.z+1.0)*(r.z+1.0) );' + LineEnding +
'  /* Using 1.0 / 2.0 instead of 0.5 to workaround fglrx bugs */' + LineEnding +
'  return r.xy / m + vec2(1.0, 1.0) / 2.0;' + LineEnding +
'}' + LineEnding +
''
