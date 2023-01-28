Test two approaches to implement dragging in CGE:

1. Just watch Motion events (e.g. by overriding TCastleView.Motion),
   and subtract `new mouse position - old mouse position`.

   This is simple, and makes sense if

   - you leave the mouse cursor visible,
   - and the dragging area is naturally limited to the window.

   E.g. this makes sense when you implement drag-and-drop.

2. Use the same logic as MouseLook.
   In this case the mouse cursor should be hidden,
   and you use TCastleContainer.MouseLookDelta and friends to

   - read mouse motion movement,
   - and keep the mouse position around the middle of the window
     (to pretend that the dragging area is infinite).

   This makes sense if you want the movement of something to *not*
   be limited by window borders.
