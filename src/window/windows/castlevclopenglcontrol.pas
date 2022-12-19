{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Control with OpenGL context on a Delphi VCL form. }
unit CastleVclOpenGlControl;

interface

uses SysUtils, Classes, Vcl.Controls,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  TCastleVclOpenGlControl = class(TCustomControl)
  public
    // TODO: For now, create and assign Context from the outside before using
    Context: TGLContextWGL;
    OnGlPaint: TNotifyEvent;
    procedure Paint; override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleVclOpenGlControl
  ]);
end;

procedure TCastleVclOpenGlControl.Paint;
begin
  inherited;
  if (Context <> nil) and Assigned(OnGlPaint) then
  begin
    Context.MakeCurrent;
    OnGlPaint(Self);
    Context.SwapBuffers;
  end;
end;

end.
