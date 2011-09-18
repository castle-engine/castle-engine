This program allows you to design a Bezier surface.
You can choose the surface dimensions, save and load it from an ultra-simple
text file format, and finally edit it interactively by selecting and
moving control points with mouse and keyboard.

Keys and mouse commands not available from the menu:

- First of all, all the keys from view3dscene Examine mode
  are available here to rotate and move the surface view.
  See [http://castle-engine.sourceforge.net/view3dscene.php].
  Summary:
  - Arrows / PageUp / PageDown         rotate scene
  - Space                              stop rotating scene
  - Ctrl + Arrows / PageUp / PageDown  move scene
  - + / -                              scale scene
  - Home                               restore default rotation,
                                       translation and scale

- Left mouse button click: select closest point (closest to the clicked
  place). You can also select point using the keys (AWSD, see menu),
  but usually mouse is more comfortable.

- Digits 28 46 93 (on numpad or not, to support laptops where numpad is
  difficult) move the currently selected point along one of
  the basic axes (X, Y or Z). Usually it's more comfortable to use
  mouse dragging for this, see below...

- Drag while holding right mouse button to move the currently
  selected point. (*right* mouse button; this is to be consistent with
  my other program, for Bezier curves,
  [http://castle-engine.sourceforge.net/bezier_curves.php]).

TODO:
- Internally, the code is very flexible and actually most of the code
  can run using any TCurve descendant (which means that various curve
  types implemented inside Curve and BezierCurve unit could be used).
  At least adding a support for weights is a matter of only the UI
  (as the underlying curve class is TRationalBezierCurve anyway,
  so we got weights anyway... for now we just set them all to 1.0).

- Surface should be rendered with smooth shading, so every point
  should get it's own normal. I was too lazy to implement that for now.
  You have to modify TSurface.Render to keep track of CurveNext
  (in addition to CurvePrev and CurveNext) to be able to calculate
  normal vectors averaged between all adjacent faces.

- Glw.NavExaminer.MouseNavigation is just turned off, to allow editing
  with mouse. Some better UI would be useful, to keep mouse navigation
  and at the same time allow mouse editing.
