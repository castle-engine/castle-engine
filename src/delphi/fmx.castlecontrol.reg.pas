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

{ Control with OpenGL context on a Delphi FMX form. }
unit fmx.castlecontrol.reg;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Windows,
  FMX.Controls, FMX.Controls.Presentation, FMX.Presentation.Win, FMX.Memo,
  FMX.Types, UITypes,
  CastleGLVersion, CastleGLUtils, CastleVectors, CastleKeysMouse,
  CastleInternalContextBase, CastleInternalContextWgl, CastleInternalContainer;

{ Designtime Only }
procedure Register;

implementation

uses FMX.Presentation.Factory, Types, FMX.Graphics,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUtils, CastleUIControls, CastleInternalDelphiUtils,
  CastleLog, FMX.CastleControl;

{ Designtime Only }
procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

end.
