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
  CastleGLVersion, CastleGLUtils, CastleInternalContextWgl, CastleInternalContainer,
  CastleVectors, CastleKeysMouse;

type
  { Control rendering OpenGL on VCL form. }
  TCastleControl = class(TCustomControl)
  strict private
    type
      { Non-abstract implementation of TCastleContainer that cooperates with
        TCastleControl. }
      TContainer = class(TCastleContainerEasy)
      private
        Parent: TCastleControl;
      protected
        function GetMousePosition: TVector2; override;
        procedure SetMousePosition(const Value: TVector2); override;
        procedure AdjustContext(const AContext: TGLContextWGL); override;
      public
        constructor Create(AParent: TCastleControl); reintroduce;
        procedure Invalidate; override;
        function Width: Integer; override;
        function Height: Integer; override;
        procedure SetInternalCursor(const Value: TMouseCursor); override;
        function Dpi: Single; override;
      end;

    var
      FContainer: TContainer;
  protected
    procedure CreateHandle; override;
    procedure DestroyHandle; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;

    { Access Castle Game Engine container properties and events,
      not specific for FMX. }
    property Container: TContainer read FContainer;
  end;

procedure Register;

implementation

uses Windows,
  CastleRenderOptions, CastleApplicationProperties, CastleRenderContext,
  CastleRectangles, CastleUIControls;

procedure Register;
begin
  RegisterComponents('Castle', [
    TCastleControl
  ]);
end;

{ TCastleControl.TContainer ---------------------------------------------------}

constructor TCastleControl.TContainer.Create(AParent: TCastleControl);
begin
  inherited Create(AParent); // AParent must be a component Owner to show published properties of container in LFM
  Parent := AParent;
end;

procedure TCastleControl.TContainer.AdjustContext(const AContext: TGLContextWGL);
begin
  inherited;
  AContext.WndPtr := Parent.Handle;
  AContext.h_Dc := GetWindowDC(AContext.WndPtr);
end;

function TCastleControl.TContainer.Dpi: Single;
begin
  Result := DefaultDpi;
end;

function TCastleControl.TContainer.GetMousePosition: TVector2;
begin
  // TODO
  Result := TVector2.Zero;
end;

procedure TCastleControl.TContainer.SetMousePosition(const Value: TVector2);
begin
  // TODO
end;

function TCastleControl.TContainer.Width: Integer;
begin
  Result := Parent.Width;
end;

function TCastleControl.TContainer.Height: Integer;
begin
  Result := Parent.Height;
end;

procedure TCastleControl.TContainer.Invalidate;
begin
  Parent.Invalidate;
end;

procedure TCastleControl.TContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  // TODO
end;

{ TCastleControl ---------------------------------------------------- }

constructor TCastleControl.Create(AOwner: TComponent);
begin
  inherited;
  FContainer := TContainer.Create(Self);
end;

destructor TCastleControl.Destroy;
begin
  inherited;
end;

procedure TCastleControl.CreateHandle;
begin
  inherited;
  { Handle is only available now, in CreateHandle.
    So only now call FContainer.CreateContext that does FContainer.AdjustContext. }
  FContainer.CreateContext;
end;

procedure TCastleControl.DestroyHandle;
begin
  FContainer.DestroyContext;
  inherited;
end;

procedure TCastleControl.Paint;
begin
  inherited;
  FContainer.DoRender;
end;

end.
