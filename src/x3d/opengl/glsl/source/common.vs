/* Vertex shader utilities used by both Gouraud and Phong shading.

   Used by ../castlerendererinternalshader.pas to construct the final shader.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

#ifdef CASTLE_NEEDS_MIRROR_PLANE_TEX_COORDS
uniform vec4 castle_NormalizedPlane;
uniform vec3 castle_CameraPositionOnPlane;
uniform vec3 castle_CameraSide;
uniform vec3 castle_CameraUp;
uniform vec4 castle_FrustumDimensions;

/* Calculate texture coordinates matching ViewpointMirror texture projection. */
vec3 castle_mirror_plane_tex_coords(const in vec4 vertex_world)
{
  /* The same implemented on CPU in Pascal (with Coord = vertex_world):

      PlaneCoord := PointOnPlaneClosestToPoint(Plane, Coord) - CameraPositionOnPlane;
      PlaneCoordProjected := Vector2(
        TVector3.DotProduct(PlaneCoord, CameraSide),
        TVector3.DotProduct(PlaneCoord, CameraUp)
      );
      Exit(Vector2(
        (PlaneCoordProjected.Data[0] - FrustumDimensions.Left) / FrustumDimensions.Width,
        (PlaneCoordProjected.Data[1] - FrustumDimensions.Bottom) / FrustumDimensions.Height
      ));
  */

  float plane_d =
    - dot(castle_NormalizedPlane.xyz, vertex_world.xyz)
    - castle_NormalizedPlane.w;
  vec3 vertex_on_plane = vertex_world.xyz + castle_NormalizedPlane.xyz * plane_d;

  vertex_on_plane -= castle_CameraPositionOnPlane;

  vec2 projected = vec2(
    dot(vertex_on_plane, castle_CameraSide),
    dot(vertex_on_plane, castle_CameraUp));

  return vec3(
    (projected.x - castle_FrustumDimensions.x) / castle_FrustumDimensions.z,
    (projected.y - castle_FrustumDimensions.y) / castle_FrustumDimensions.w,
    0.0);
}
#endif
