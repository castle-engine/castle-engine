{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
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
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  CastleViewport;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps, LabelInfo: TCastleLabel;
    MainViewport: TCastleViewport;
    SceneSvg: TCastleScene;
    ButtonPrevious, ButtonNext, ButtonOpen: TCastleButton;
  strict private
    SampleIndex: Integer;
    LastUrl: String;
    procedure LoadSvg(const Url: String);
    { Adjust camera to show the whole SceneSvg. }
    procedure FitCamera;
    procedure ClickPrevious(Sender: TObject);
    procedure ClickNext(Sender: TObject);
    procedure ClickOpen(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, Math,
  CastleUtils, CastleTimeUtils, CastleUriUtils, CastleWindow, CastleBoxes,
  X3DLoad, X3DLoadInternalSvg;

const
  SampleUrls: array [0..5] of String = (
    'castle-data:/castle_game_engine_icon_less_overlap.svg',
    'castle-data:/fpc/tiger.svg',
    'castle-data:/fpc/lady_bug.svg',
    'castle-data:/fpc/lady_bug_flying.svg',
    'castle-data:/fpc/pas2js.svg',
    'castle-data:/fpc/splash.svg'
  );

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonPrevious.OnClick := {$ifdef FPC}@{$endif} ClickPrevious;
  ButtonNext.OnClick := {$ifdef FPC}@{$endif} ClickNext;
  ButtonOpen.OnClick := {$ifdef FPC}@{$endif} ClickOpen;
  LoadSvg(SampleUrls[SampleIndex]);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.LoadSvg(const Url: String);
var
  Stats: TSvgToX3DStats;
  TimeStart: TTimerResult;
begin
  LastUrl := Url;
  TimeStart := Timer;
  try
    { Load SVG into TCastleScene, just like any other model format.
      The engine registers SVG model format,
      if only it was compiled with FPC that includes fcl-svg package. }
    SceneSvg.Url := Url;
  except
    { Explain why SVG format may be unknown. }
    on E: EModelFormatUnknown do
    begin
      SceneSvg.Url := ''; // clear previous SVG
      LabelInfo.Caption := 'Cannot load ' + UriDisplay(Url) + ':' + NL +
        'File format not supported.' + NL +
        NL +
        'Note: Loading SVG requires compiling with the latest FPC' + NL +
        '(from "main" branch), that includes fcl-svg package. See README.md.';
      Exit;
    end;
    { Show other errors (e.g. invalid SVG) to the user, as the user can choose
      any file using ButtonOpen. }
    on E: Exception do
    begin
      SceneSvg.Url := ''; // clear previous SVG
      LabelInfo.Caption := 'Error loading ' + UriDisplay(Url) + ':' + NL +
        E.ClassName + ': ' + E.Message;
      Exit;
    end;
  end;

  Stats := InternalLastSvgStats;
  LabelInfo.Caption := Format(
    'File: %s' + NL +
    'SVG size: %d x %d' + NL +
    'fcl-svg asked to draw: %d fills, %d strokes, %d text runs (and %d images, ignored)' + NL +
    'Created X3D: %d shapes, %d faces, %d points' + NL +
    'Loaded in %f seconds' + NL +
    NL +
    'Drag with mouse to move, use mouse wheel to zoom.', [
    UriDisplay(Url),
    Stats.Width, Stats.Height,
    Stats.Fills, Stats.Strokes, Stats.GlyphRuns, Stats.IgnoredImages,
    Stats.Shapes, Stats.Faces, Stats.Points,
    TimeStart.ElapsedTime
  ]);

  FitCamera;
end;

procedure TViewMain.FitCamera;
var
  ViewportAspect: Single;
  Box: TBox3D;
begin
  Box := SceneSvg.LocalBoundingBox;
  { Box is empty if SVG didn't produce any shapes. }
  if Box.IsEmpty then
    Exit;

  { Undo the movement and zoom done by TCastle2DNavigation.
    Camera looks in the default direction (-Z), so place it
    in front of all the SVG layers (see SvgLayersZDistance),
    centered on the drawing. }
  MainViewport.Camera.Translation := Vector3(
    Box.Center.X,
    Box.Center.Y,
    Box.Max.Z + 1);
  MainViewport.Camera.Orthographic.Width := 0;

  { Set camera's Orthographic.Height to see it all, with some margin. }
  ViewportAspect := MainViewport.EffectiveWidth / MainViewport.EffectiveHeight;
  MainViewport.Camera.Orthographic.Height :=
    Max(Box.Size.Y, Box.Size.X / ViewportAspect) * 1.4;
end;

procedure TViewMain.ClickPrevious(Sender: TObject);
begin
  SampleIndex := ChangeIntCycle(SampleIndex, -1, High(SampleUrls));
  LoadSvg(SampleUrls[SampleIndex]);
end;

procedure TViewMain.ClickNext(Sender: TObject);
begin
  SampleIndex := ChangeIntCycle(SampleIndex, 1, High(SampleUrls));
  LoadSvg(SampleUrls[SampleIndex]);
end;

procedure TViewMain.ClickOpen(Sender: TObject);
var
  Url: String;
begin
  Url := LastUrl;
  if Application.MainWindow.FileDialog('Open SVG File', Url, true,
    'All Files|*|*SVG Files|*.svg;*.svgz') then
    LoadSvg(Url);
end;

end.
