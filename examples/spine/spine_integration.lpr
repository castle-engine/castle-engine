uses CastleVectors, CastleWindow, CastleControls, CastleOnScreenMenu,
  CastleControlsImages, CastleImages, CastleFilesUtils, CastleColors,
  CastleUIControls, CastleSpine;
var
  Window: TCastleWindowCustom;
  Background: TCastleSimpleBackground;
  SpineAnim: TSpineAnimation;
begin
  Window := TCastleWindowCustom.Create(Application);

  Background := TCastleSimpleBackground.Create(Window);
  Background.Color := Vector4Single(0.5, 0.5, 1.0, 1.0);
  Window.Controls.InsertBack(Background);

  SpineAnim := TSpineAnimation.Create(Window);
  SpineAnim.Load('file:///home/michalis/installed/SpineTrial/examples/dragon/export/dragon.json');

  Window.OpenAndRun;
end.