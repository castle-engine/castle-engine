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
{ Main view, where most of the application logic takes place. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start.
      TODO: No, for now this is created by hand. }
    LabelFps: TCastleLabel;
  private
    LifeTime: Double;
    HasLastEventPosition: Boolean;
    LastEventPosition: TVector2;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    function Release(const Event: TInputPressRelease): Boolean; override;
    function Motion(const Event: TInputMotion): Boolean; override;
    procedure Render; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils,
  CastleGLUtils, CastleRectangles, CastleColors, CastleLog;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  // TODO: No file reading with WebGL
  // DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  LabelFps := TCastleLabel.Create(Self);
  LabelFps.Anchor(hpLeft, 5);
  LabelFps.Anchor(vpTop, -5);
  LabelFps.Color := Gray;
  InsertFront(LabelFps);
end;

procedure TViewMain.Render;
begin
  inherited;

  // visualize something moving (frames are processed)
  DrawCircle(
    Vector2(
      Container.PixelsWidth / 2 + 100 * Sin(LifeTime * 5),
      Container.PixelsHeight / 2 + 100 * Cos(LifeTime * 5)
    ),
    20, 20, Yellow
  );

  // visualize LastEventPosition, so mouse positions are correct
  if HasLastEventPosition then
    DrawCircle(LastEventPosition, 20, 20, Navy);

  // visualize screen corners and rectangles, to show we read container size correctly
  DrawRectangle(FloatRectangle(5, 5, 30, 30), Teal);
  DrawRectangle(FloatRectangle(
    Container.PixelsWidth - 35,
    Container.PixelsHeight - 35,
    30,
    30), Blue);

  // visualize current time (TODO: this is UTC)
  FallbackFont.Print(5, 40, Red, FormatDateTime('yyyy-mm-dd, hh:nn:ss', Now));

  // visualize text
  FallbackFont.Print(5, 80, Yellow, 'Hello Castle Game Engine on the Web!');
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LifeTime := LifeTime + SecondsPassed;
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.EventType = itMouseButton then
  begin
    HasLastEventPosition := true;
    // Note: We don't use UI scaling in this project now,
    // (no CastleSettings.xml loaded, and direct Render routines ignore scaling anyway).
    LastEventPosition := Vector2(Event.Position.X, Event.Position.Y);
  end;

  WritelnLog('Press: ' + Event.ToString);
end;

function TViewMain.Release(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  if Event.EventType = itMouseButton then
  begin
    HasLastEventPosition := true;
    // Note: We don't use UI scaling in this project now,
    // (no CastleSettings.xml loaded, and direct Render routines ignore scaling anyway).
    LastEventPosition := Vector2(Event.Position.X, Event.Position.Y);
  end;

  WritelnLog('Release: ' + Event.ToString);
end;

function TViewMain.Motion(const Event: TInputMotion): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  HasLastEventPosition := true;
  LastEventPosition := Vector2(Event.Position.X, Event.Position.Y);

  // too verbose
  //WritelnLog('Motion: ' + Event.ToString);
end;

end.
