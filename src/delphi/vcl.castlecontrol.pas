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
unit Vcl.CastleControl;

{$I castleconf.inc}

interface

uses SysUtils, Classes, Vcl.Controls,
  CastleGLVersion, CastleGLUtils, CastleGLContextWGL;

type
  { Control rendering OpenGL on VCL form. }
  TCastleControl = class(TCustomControl)
  private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FOnGlPaint: TNotifyEvent;
    FOnGlOpen: TNotifyEvent;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;

    { Render callback. }
    property OnGlPaint: TNotifyEvent read FOnGlPaint write FOnGlPaint;

    { When OpenGL context is initialized. }
    property OnGlOpen: TNotifyEvent read FOnGlOpen write FOnGlOpen;
  end;

procedure Register;

implementation

uses Windows,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

{ TCastleControl ---------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK
end;

destructor TCastleControl.Destroy;
begin
  FreeAndNil(FContext);
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  inherited;

  // Handle is only available now, in CreateHandle
  FContext.WndPtr := Handle;
  FContext.h_Dc := GetWindowDC(FContext.WndPtr);

  FContext.ContextCreate(FRequirements);

  // TODO: code below should be done by TCastleContainer

  // initialize CGE OpenGL resources
  FContext.MakeCurrent;
  ApplicationProperties._GLContextEarlyOpen;
  ApplicationProperties._GLContextOpen;
  GLInformationInitialize;

  // CGE needs this to be assigned, typically done by container
  RenderContext := TRenderContext.Create;

  if Assigned(OnGlOpen) then
    OnGlOpen(Self);
end;

procedure TCastleControl.DestroyHandle;
begin
  if FContext <> nil then
    FContext.ContextDestroy;
  inherited;
end;

procedure TCastleControl.Paint;
begin
  inherited;
  if (FContext <> nil) and Assigned(OnGlPaint) then
  begin
    FContext.MakeCurrent;
    RenderContext.Viewport := Rectangle(0, 0, Width, Height);
    OnGlPaint(Self);
    FContext.SwapBuffers;
  end;
end;

end.
