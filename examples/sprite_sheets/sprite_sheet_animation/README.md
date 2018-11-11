Demo of using TSprite to display an animation.
You load an animation to TSprite, and render it using TSprite.Draw,
e.g. in TCastleWindow.OnRender event.

Note that there exists an alternative way for sprite animation:
Convert them to X3D using Castle Game Engine tool `sprite-sheet-to-x3d`,
and then load, display and animate them using TCastle2DScene.
See https://github.com/castle-engine/castle-engine/wiki/2D-Games
for a discussion and comparison of these 2 approaches.
