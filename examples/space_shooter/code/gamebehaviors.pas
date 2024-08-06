{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Small useful behaviors (TCastleBehavior descendants) used in this game. }
unit GameBehaviors;

interface

uses CastleVectors, CastleTransform;

type
  { Remove the parent when it goes outside of the possibly visible space. }
  TAutoRemoveBehavior = class(TCastleBehavior)
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses CastleRectangles;

procedure TAutoRemoveBehavior.Update(const SecondsPassed: Single;
  var RemoveMe: TRemoveType);
const
  { Rockets are removed when they go far away.
    This rectangle was chosen to be large enough to cover the whole screen.

    Vertically, it covers the screen for 100%:
    Height is 1200, for sure larger than Viewport.Camera.Orthographic.Height = 1000.

    Horizontally, it covers the screen as long as aspect ratio is <= 2:1.

    We *could* adjust it to Viewport.Camera.Orthographic.EffectiveRect
    sizes, and be more precise. But this would mean that game balance works
    a bit differently (rockets reach more or less further) depending on
    the window size aspect ratio. }
  RocketAllowedPositions: TFloatRectangle = (
    Left: -1000; Bottom: -600; Width: 2000; Height: 1200
  );
begin
  inherited;
  if not RocketAllowedPositions.Contains(Parent.WorldTranslation.XY) then
    Parent.Parent.RemoveDelayed(Parent, true);
end;

end.
