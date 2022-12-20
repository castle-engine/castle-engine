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

{$I castleconf.inc}

interface

uses SysUtils, Classes, Vcl.Controls,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  { Control rendering OpenGL on VCL form. }
  TCastleVclOpenGlControl = class(TCustomControl)
  private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FOnGlPaint: TNotifyEvent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Paint; override;
    procedure GLContextOpen;
    property Requirements: TGLContextRequirements read FRequirements;
    property OnGlPaint: TNotifyEvent read FOnGlPaint write FOnGlPaint;
  end;

procedure Register;

implementation

uses Windows,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleVclOpenGlControl
  ]);
end;

{ TCastleVclOpenGlControl ---------------------------------------------------- }

constructor TCastleVclOpenGlControl.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK
end;

procedure TCastleVclOpenGlControl.GLContextOpen;
begin
  // Handle not available before Parent is assigned
  FContext.WndPtr := Handle;
  FContext.h_Dc := GetWindowDC(FContext.WndPtr);

  FContext.ContextCreate(FRequirements);

  // initialize CGE OpenGL resources
  FContext.MakeCurrent;
  ApplicationProperties._GLContextEarlyOpen;
  ApplicationProperties._GLContextOpen;
  GLInformationInitialize;

  // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;
  RenderContext.Viewport := Rectangle(0, 0, Width, Height);
end;

procedure TCastleVclOpenGlControl.Paint;
begin
  inherited;
  if (FContext <> nil) and Assigned(OnGlPaint) then
  begin
    FContext.MakeCurrent;
    OnGlPaint(Self);
    FContext.SwapBuffers;
  end;
end;

end.
