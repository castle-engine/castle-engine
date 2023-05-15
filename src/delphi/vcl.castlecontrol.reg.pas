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

{ Control with OpenGL context on a Delphi VCL form. }
unit vcl.castlecontrol.reg;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Vcl.Controls, Vcl.ExtCtrls, Types,  WinApi.Messages,
  CastleGLVersion, CastleGLUtils,
  CastleInternalContextBase, CastleInternalContextWgl, CastleInternalContainer,
  CastleVectors, CastleKeysMouse, VCL.CastleControl;

{ Designtime Only }
procedure Register;

implementation

uses Windows,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUIControls, CastleInternalDelphiUtils;

{ Designtime Only }
procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

end.
