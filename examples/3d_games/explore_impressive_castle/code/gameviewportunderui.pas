{
  Copyright 2022-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Maintain display of the viewport with 3D contents under the UI.

  We could just use TCastleDesign to instantiate it
  from castle-data:/viewport_under_ui.castle-user-interface in
  each UI design, but this would mean we reload the model when switching
  views. }
unit GameViewportUnderUi;

interface

uses CastleControls;

var
  ViewportUnderUi: TCastleDesign;

procedure LoadViewportUnderUi;

implementation

uses CastleWindow;

procedure LoadViewportUnderUi;
begin
  ViewportUnderUi := TCastleDesign.Create(Application);
  ViewportUnderUi.FullSize := true;
  ViewportUnderUi.Url := 'castle-data:/viewport_under_ui.castle-user-interface';
end;

end.