{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Create and edit curves.
  Docs are at https://github.com/castle-engine/castle-engine/wiki/Curves-tool }

{$apptype GUI}
{$I castleconf.inc}

uses SysUtils, Classes, Math,
  CastleGL, CastleWindow, CastleImages, CastleGLUtils, CastleLog,
  CastleUtils, CastleMessages, CastleCurves, CastleVectors, CastleFonts,
  CastleKeysMouse, CastleParameters, CastleClassUtils,
  CastleFilesUtils, CastleStringUtils, CastleColors, CastleURIUtils,
  CastleUIControls, CastleControls, CastleGLImages, CastleOpenDocument,
  CastleApplicationProperties;

var
  Window: TCastleWindowCustom;

  Curves: TControlPointsCurveList;
  { -1 (none selected) or in [0 .. Curves.Count-1].
    Always set using SetSelectedCurve. }
  SelectedCurve: Integer = -1;
  { -1 (none selected) or (only if SelectedCurve <> -1)
    in [0 .. Curves[SelectedCurve].ControlPoints.Count-1].
    Always set using SetSelectedPoint. }
  SelectedPoint: Integer = -1;

  RenderSegments: Cardinal = 500;
  ShowPoints: boolean = true;
  ShowSelectedCurveConvexHull: boolean = false;
  LineWidth: Float = 1;

  { Indicates dragging with mouse.
    So SelectedPoint and SelectedCurve never change during dragging
    (changing them by any means, like a menu command, breaks dragging).
    Always SelectedPoint <> -1 (so SelectedCurve also <>-1) when Dragging. }
  Dragging: boolean = false;
  DraggingFarEnoughToBeActive: boolean;
  DraggingStartPosition: TVector2Single;

  { Just an indication of from what URL we loaded these Curves /
    where we saved them last time / etc.
    Set only using SetCurvesURL. }
  CurvesURL: string;

  NewCurvesClass: TControlPointsCurveClass = TPiecewiseCubicBezier;

  ColorConvexHull: TCastleColor;
  ColorCurveSelected: TCastleColor;
  ColorCurveNotSelected: TCastleColor;
  ColorPointSelected: TCastleColor;

  BackgroundImage: TGLImage;
  BackgroundImageURL: string;

  Zoom: Single = 1;

const
  Version = '2.0.0';
  CurvesToolURL = 'https://github.com/castle-engine/castle-engine/wiki/Curves-tool';
  DonateURL = 'http://castle-engine.sourceforge.net/donate.php';

{ Call this always when SelectedPoint or SelectedCurve or (any) contents of
  Curves[SelectedCurve] changes. It is always called from
  SetSelectedXxx so if you change something and then call SetSelectedXxx,
  you don't have to call this. }
procedure SelectedChanged;
begin
  Dragging := false;
end;

procedure SetSelectedPoint(Value: Integer);
begin
  SelectedPoint := Value;
  SelectedChanged;
end;

procedure SetSelectedCurve(Value: Integer);
begin
  SelectedCurve := Value;
  SelectedChanged;
end;

procedure SetCurvesURL(const Value: string);
begin
  CurvesURL := Value;
  Window.Caption := Value + ' - Curves plotting';
end;

function RationalBezierCurveSelected: boolean;
begin
  Result := (SelectedCurve <> -1) and
            {$warnings off}
            (Curves[SelectedCurve] is TRationalBezierCurve);
            {$warnings on}
end;

function PiecewiseCubicBezierCurveSelected: boolean;
begin
  Result := (SelectedCurve <> -1) and
            (Curves[SelectedCurve] is TPiecewiseCubicBezier);
end;

function RationalBezierCurvePointSelected: boolean;
begin
  Result := (SelectedPoint <> -1) and
            {$warnings off}
            (Curves[SelectedCurve] is TRationalBezierCurve);
            {$warnings on}
end;

const
  SErrSelectCurve = 'You must select some curve.';
  SErrSelectRationalCurve = 'You must select a Rational Bezier curve.';
  SErrSelectPiecewiseCubicBezierCurve = 'You must select a Piecewise Cubic Bezier curve.';
  SErrSelectRationalCurvePoint =
    'You must select some point on a Rational Bezier curve.';

procedure LoadCurves(const NewURL: string);
var
  ErrMessage: string;
  NewCurves: TCurveList;
  I: Integer;
begin
  NewCurves := nil;
  try
    NewCurves := TCurveList.Create(false);
    NewCurves.LoadFromFile(NewURL);
  except
    on E: Exception do
    begin
      ErrMessage := 'Error while loading file "' + NewURL + '" : ' + E.Message;
      if Window.Closed then
        WritelnWarning('Loading', ErrMessage) else
        MessageOK(Window, ErrMessage);
      FreeAndNil(NewCurves); // avoid memory leaks
      Exit;
    end;
  end;

  { move curve instances from NewCurves to Curves.
    Potentially, various curve types may be stored in curves file,
    but right now we support only TControlPointsCurve. }
  Curves.Clear;
  for I := 0 to NewCurves.Count - 1 do
    if NewCurves[I] is TControlPointsCurve then
      Curves.Add(NewCurves[I] as TControlPointsCurve);
  FreeAndNil(NewCurves);

  { select stuff after Curves is updated }
  SetCurvesURL(NewURL);
  if Curves.Count <> 0 then { select first curve, if available }
    SetSelectedCurve(0) else
    SetSelectedCurve(-1);
  if SelectedCurve = -1 then
    SetSelectedPoint(-1) else
    SetSelectedPoint(0);
end;

{ TStatusText ---------------------------------------------------------------- }

type
  TStatusText = class(TCastleLabel)
    procedure Render; override;
  end;

procedure TStatusText.Render;

  function IntToStrOrNone(i: Integer): string;
  begin
    if i <> -1 then Result := IntToStr(i) else Result := 'none';
  end;

begin
  if not GetExists then Exit;

  { regenerate Text contents at every Render call }
  Text.Clear;
  Text.Append(Format('Selected curve : %s', [IntToStrOrNone(SelectedCurve)]));
  if SelectedCurve <> -1 then
    Text.Append(Format('  Curve type : %s', [Curves[SelectedCurve].ClassName]));
  Text.Append(Format('Selected point : %s', [IntToStrOrNone(SelectedPoint)]));
  {$warnings off}
  if RationalBezierCurvePointSelected then
    Text.Append(Format('  Weight : %f',
      [TRationalBezierCurve(Curves[SelectedCurve]).Weights[SelectedPoint]]));
  {$warnings on}
  Text.Append('');
  Text.Append(Format('Rendering segments = %d', [RenderSegments]));
  Text.Append(Format('New curves type = %s', [NewCurvesClass.ClassName]));

  inherited;
end;

var
  StatusText: TStatusText;

{ TCurvesDisplay ------------------------------------------------------------- }

type
  TCurvesDisplay = class(TUIControl)
    procedure Render; override;
  end;

procedure TCurvesDisplay.Render;
var
  I: Integer;
begin
  glLoadIdentity;

  RenderContext.Clear([cbColor], Black);
  if BackgroundImage <> nil then
    BackgroundImage.Draw(BackgroundImage.Rect.ScaleAround0(Zoom));

  glPushMatrix;
    glScalef(Zoom, Zoom, Zoom);

    { draw convex hull of SelectedCurve }
    if ShowSelectedCurveConvexHull and (SelectedCurve <> -1) then
    begin
      {$warnings off}
      Curves[SelectedCurve].ConvexHullColor := ColorConvexHull;
      Curves[SelectedCurve].RenderConvexHull;
      {$warnings on}
    end;

    { draw all curves and their control points }
    for i := 0 to Curves.Count-1 do
    begin
      {$warnings off}
      { this program knowingly uses a lot of deprecated CastleCurves stuff
        for rendering... }
      Curves[i].LineWidth := LineWidth;
      if i = SelectedCurve then
        Curves[i].Color := ColorCurveSelected else
        Curves[i].Color := ColorCurveNotSelected;
      Curves[i].ControlPointsColor := Curves[i].Color;
      if ShowPoints then Curves[i].RenderControlPoints;
      Curves[i].Render(RenderSegments);
      {$warnings on}
    end;

    { draw SelectedPoint }
    if SelectedPoint <> -1 then
    begin
      glColorv(ColorPointSelected);
      glBegin(GL_POINTS);
        glVertexv(Curves[SelectedCurve].ControlPoints.Items[SelectedPoint]);
      glEnd;
    end;

  glPopMatrix;
end;

{ Add new curve point and select it. }
procedure AddNewPoint(const Position: TVector2Single);
var
  NewPoint: TVector3Single;
begin
  NewPoint := Vector3Single(Position[0], Position[1], 0);

  if SelectedCurve = -1 then
  begin
    Curves.Add(NewCurvesClass.Create(nil));
    SetSelectedCurve(Curves.Count - 1);
  end;

  if SelectedPoint = -1 then
  begin
    Curves[SelectedCurve].ControlPoints.Add(NewPoint);
    SetSelectedPoint(Curves[SelectedCurve].ControlPoints.Count-1);
  end else
  begin
    Curves[SelectedCurve].ControlPoints.Insert(SelectedPoint+1, NewPoint);
    SetSelectedPoint(SelectedPoint+1);
  end;

  { set Weight of new point }
  {$warnings off}
  if Curves[SelectedCurve] is TRationalBezierCurve then
    TRationalBezierCurve(Curves[SelectedCurve]).Weights.Insert(SelectedPoint, 1);
  {$warnings on}

  Curves[SelectedCurve].UpdateControlPoints;
end;

procedure ChangeZoom(const Multiply: Single);
begin
  Zoom *= Multiply;
  ClampVar(Zoom, 0.01, 100);
end;

procedure Press(Container: TUIContainer; const Event: TInputPressRelease);

  procedure ClosestControlPoint(const Point: TVector2Single;
    var CurveNum, PointNum: Integer);
  { return such CurveNum, PointNum that
    Curves[CurveNum].ControlPoints[PointNum] is the closest control point
    (from all ControlPoints from all Curves) to x, y.
    Returns -1, -1 if Curves.Count = 0. }
  var
    SqrDist, SqrBestDist: Single;
    i, j: Integer;
  begin
    SqrBestDist := MaxSingle;
    CurveNum := -1;
    PointNum := -1;
    for i := 0 to Curves.Count-1 do
      for j := 0 to Curves[i].ControlPoints.Count-1 do
      begin
        SqrDist := Sqr(Curves[i].ControlPoints.Items[j][0] - Point[0]) +
                   Sqr(Curves[i].ControlPoints.Items[j][1] - Point[1]);
        if SqrDist < SqrBestDist then
        begin
          SqrBestDist := SqrDist;
          CurveNum := i;
          PointNum := j;
        end;
      end;
  end;

  procedure SelectClosestPoint(const Point: TVector2Single);
  var
    NewSelectedCurve, NewSelectedPoint: Integer;
  begin
    ClosestControlPoint(Point, NewSelectedCurve, NewSelectedPoint);
    SetSelectedCurve(NewSelectedCurve);
    SetSelectedPoint(NewSelectedPoint);
  end;

  procedure StartDragging;
  begin
    if SelectedPoint <> -1 then
    begin
      Dragging := true;
      DraggingFarEnoughToBeActive := false;
      DraggingStartPosition := Event.Position;
    end;
  end;

const
  ZoomFactor = 1.05;
var
  Pos: TVector2Single;
begin
  Pos := Event.Position / Zoom;
  if Event.IsMouseButton(mbLeft) then
  begin
    SelectClosestPoint(Pos);
    StartDragging;
  end else
  if Event.IsMouseButton(mbRight) then
    AddNewPoint(Pos) else
  if Event.IsMouseWheel(mwUp) or Event.IsMouseWheel(mwLeft) then
    ChangeZoom(ZoomFactor) else
  if Event.IsMouseWheel(mwDown) or Event.IsMouseWheel(mwRight) then
    ChangeZoom(1/ZoomFactor) else
    Exit;

  Window.Invalidate;
end;

procedure Release(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) then
  begin
    Dragging := false;
    Window.Invalidate;
  end;
end;

procedure Motion(Container: TUIContainer; const Event: TInputMotion);
const
  DraggingFarEnough = 5;
var
  Change: TVector2Single;
  Change3D: TVector3Single;
  I: Integer;
begin
  if Dragging then
  begin
    if not DraggingFarEnoughToBeActive then
    begin
      DraggingFarEnoughToBeActive :=
        PointsDistanceSqr(DraggingStartPosition, Event.Position) > Sqr(DraggingFarEnough);
      if DraggingFarEnoughToBeActive then
        Change := Event.Position - DraggingStartPosition else
        // do not drag until you move mouse definitely away, to avoid accidental dragging
        Exit;
    end else
      Change := Event.Position - Event.OldPosition;

    Change3D := Vector3Single(Change, 0);
    if not (mkShift in Window.Pressed.Modifiers) then
    begin
      Curves[SelectedCurve].ControlPoints.L[SelectedPoint] += Change3D;
    end else
    begin
      for i := 0 to Curves[SelectedCurve].ControlPoints.Count-1 do
        Curves[SelectedCurve].ControlPoints.L[i] += Change3D;
    end;

    Curves[SelectedCurve].UpdateControlPoints;
    Window.Invalidate;
  end;
end;

procedure Open(Container: TUIContainer);
begin
  RenderContext.PointSize := 10;
end;

procedure Update(Container: TUIContainer);
const
  ZoomFactor = 2;
begin
  if Container.Pressed.Characters['+'] then
    ChangeZoom(Power(ZoomFactor, Container.Fps.UpdateSecondsPassed));
  if Container.Pressed.Characters['-'] then
    ChangeZoom(Power(1 / ZoomFactor, Container.Fps.UpdateSecondsPassed));
end;

{ menu ------------------------------------------------------------ }

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);

  procedure ChangeSelectedCurve(Next: boolean);
  var
    Change: Integer;
  begin
    if SelectedCurve = -1 then
    begin
      if Curves.Count <> 0 then
      begin
        if Next then SetSelectedCurve(0) else SetSelectedCurve(Curves.Count-1);
        SetSelectedPoint(0);
      end;
    end else
    begin
      if Curves.Count > 1 then
      begin
        if Next then Change:=+1 else Change := -1;
        SetSelectedCurve(ChangeIntCycle(SelectedCurve, Change, Curves.Count-1));
        SetSelectedPoint(0);
      end;
    end;
  end;

  procedure ChangeSelectedPoint(Next: boolean);
  var
    Change: Integer;
  begin
    if SelectedCurve <> -1 then
    begin
      if SelectedPoint = -1 then
      begin
        if Next then
          SetSelectedPoint(0) else
          SetSelectedPoint(Curves[SelectedCurve].ControlPoints.Count-1);
      end else
      begin
        if Next then Change:=+1 else Change := -1;
        SetSelectedPoint(ChangeIntCycle(SelectedPoint, Change,
          Curves[SelectedCurve].ControlPoints.Count-1));
      end;
    end;
  end;

  procedure DeleteSelectedCurve;
  begin
    if SelectedCurve <> -1 then
    begin
      { remove Curves[SelectedCurve] }
      Curves.Delete(SelectedCurve);
      { update SelectedCurve and SelectedPoint }
      if SelectedCurve >= Curves.Count then
      begin
        if Curves.Count = 0 then SetSelectedCurve(-1) else SetSelectedCurve(0);
      end;
      if SelectedCurve = -1 then
        SetSelectedPoint(-1) else
        SetSelectedPoint(0);
    end;
  end;

  procedure DeleteSelectedPoint;
  begin
    if SelectedPoint <> -1 then
    begin
      if Curves[SelectedCurve].ControlPoints.Count = 1 then
        DeleteSelectedCurve else
      begin
        { remove point }
        Curves[SelectedCurve].ControlPoints.Delete(SelectedPoint);
        {$warnings off}
        if Curves[SelectedCurve] is TRationalBezierCurve then
          TRationalBezierCurve(Curves[SelectedCurve]).Weights.Delete(SelectedPoint);
        {$warnings on}
        Curves[SelectedCurve].UpdateControlPoints;
        { update SelectedPoint }
        if SelectedPoint >= Curves[SelectedCurve].ControlPoints.Count then
          SetSelectedPoint(Curves[SelectedCurve].ControlPoints.Count-1);
      end;
    end;
  end;

  procedure OpenFile;
  var
    S: string;
  begin
    S := CurvesURL;
    if Window.FileDialog('Open curves from XML file', S, true,
      'All Files|*|*XML files|*.xml') then
      LoadCurves(S);
  end;

  procedure SaveFile;
  var
    S: string;
    NewCurves: TCurveList;
    I: Integer;
  begin
    s := CurvesURL;
    if Window.FileDialog('Save curves to XML file', s, false,
      'All Files|*|*XML files|*.xml') then
    begin
      try
        NewCurves := TCurveList.Create(false);
        for I := 0 to Curves.Count - 1 do
          NewCurves.Add(Curves[I]);
        NewCurves.SaveToFile(S);
      finally FreeAndNil(NewCurves) end;

      SetCurvesURL(S);
    end;
  end;

  procedure SetSelectedWeight;
  var
    NewWeight: Single;
  begin
    if RationalBezierCurvePointSelected then
    begin
      {$warnings off}
      NewWeight := TRationalBezierCurve(Curves[SelectedCurve]).
        Weights[SelectedPoint];
      {$warnings on}
      if MessageInputQuery(Window, 'Enter new weight:', NewWeight) then
      begin
        if NewWeight <= 0 then
          MessageOK(Window, 'Invalid weight, each weight must be > 0') else
        begin
          {$warnings off}
          TRationalBezierCurve(Curves[SelectedCurve]).
            Weights[SelectedPoint] := NewWeight;
          {$warnings on}
          Curves[SelectedCurve].UpdateControlPoints;
          SelectedChanged;
        end;
      end;
    end else
      MessageOK(Window, SErrSelectRationalCurvePoint);
  end;

  procedure ScaleCurve;
  var
    Scale: Single;
    I: Integer;
  begin
    if SelectedCurve <> -1 then
    begin
      Scale := 1;
      if MessageInputQuery(Window, 'Enter scale:', Scale) then
      begin
        for I := 0 to Curves[SelectedCurve].ControlPoints.Count - 1 do
          Curves[SelectedCurve].ControlPoints.L[I] *= Scale;
        Curves[SelectedCurve].UpdateControlPoints;
      end;
    end else
      MessageOK(Window, SErrSelectCurve);
  end;

var
  S: string;
begin
  case MenuItem.IntData of
    4:  OpenFile;
    6:  SaveFile;
    8:  Window.SaveScreenDialog(FileNameAutoInc('castle-curves_%d.png'));
    10: Window.Close;

    201: begin
           S := BackgroundImageURL;
           if Window.FileDialog('Open background image', S, true,
             LoadImage_FileFilters) then
           begin
             BackgroundImageURL := S;
             FreeAndNil(BackgroundImage);
             BackgroundImage := TGLImage.Create(BackgroundImageURL);
           end;
         end;
    202: FreeAndNil(BackgroundImage);

    305: StatusText.Exists := not StatusText.Exists;
    310: ShowPoints := not ShowPoints;
    320: ShowSelectedCurveConvexHull := not ShowSelectedCurveConvexHull;
    326: Zoom := 1;
    330: RenderSegments := Max(1, MessageInputCardinal(Window,
           'Render curves as ... segments ?', RenderSegments));
    331: RenderSegments *= 2;
    332: RenderSegments := Max(1, RenderSegments div 2);
    336: MessageInputQuery(Window, 'Input line width:', LineWidth);

    341: Window.ColorDialog(ColorConvexHull);
    342: Window.ColorDialog(ColorCurveSelected);
    343: Window.ColorDialog(ColorCurveNotSelected);
    344: Window.ColorDialog(ColorPointSelected);

    401: ScaleCurve;
    403: begin
           SetSelectedCurve(-1);
           SetSelectedPoint(-1);
         end;
    404: ChangeSelectedCurve(true);
    405: ChangeSelectedCurve(false);
    406: ChangeSelectedPoint(true);
    407: ChangeSelectedPoint(false);
    408: DeleteSelectedPoint;
    409: DeleteSelectedCurve;
    411: SetSelectedWeight;

    412: if RationalBezierCurvePointSelected then
           {$warnings off}
           with TRationalBezierCurve(Curves[SelectedCurve]).Weights do
           {$warnings on}
             L[SelectedPoint] *= 2 else
           MessageOK(Window, SErrSelectRationalCurvePoint);
    413: if RationalBezierCurvePointSelected then
           {$warnings off}
           with TRationalBezierCurve(Curves[SelectedCurve]).Weights do
           {$warnings on}
             L[SelectedPoint] /= 2 else
           MessageOK(Window, SErrSelectRationalCurvePoint);
    431: NewCurvesClass := TPiecewiseCubicBezier;
    {$warnings off}
    432: NewCurvesClass := TRationalBezierCurve;
    {$warnings on}

    1010:if not OpenURL(CurvesToolURL) then
           Window.MessageOk(SCannotOpenURL, mtError);
    1020:if not OpenURL(DonateURL) then
           Window.MessageOk(SCannotOpenURL, mtError);
    1030:begin
           MessageOk(Window,
             'castle-curves: curves editor for Castle Game Engine.' +nl+
             'Version ' + Version + '.' + NL +
             NL +
             CurvesToolURL + NL +
             NL +
             'Compiled with ' + SCompilerDescription +'.');
         end;
    else raise EInternalError.Create('not impl menu item');
  end;
  Window.Invalidate;
end;

function CreateMainMenu: TMenu;
var
  M, M2: TMenu;
begin
  Result := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    { swapping fullscreen turned off for simplicity in implementation
      BGImage }
    { M.Append(TMenuItemKey.Create('FullScreen on/off', F11));
    }
    M.Append(TMenuItem.Create('_Open ...',            4, CtrlO));
    M.Append(TMenuItem.Create('_Save ...',            6, CtrlS));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Save screen ...', 8, K_F5));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',                10, CtrlW));
    Result.Append(M);
  M := TMenu.Create('_Background');
    M.Append(TMenuItem.Create('_Load ...', 201));
    M.Append(TMenuItem.Create('_Clear',              202));
    Result.Append(M);
  M := TMenu.Create('_View');
    M.Append(TMenuItemChecked.Create('Show / Hide _status', 305, K_F1,
      StatusText.Exists, true));
    M.Append(TMenuItemChecked.Create(
      'Show / Hide non-selected _points',                   310,
      ShowPoints, true));
    M.Append(TMenuItemChecked.Create(
      'Show / Hide _convex hull of selected curve',         320,
      ShowSelectedCurveConvexHull, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('No Zoom' , 326, K_Home));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set curves _rendering segments ...', 330));
    M.Append(TMenuItem.Create('Curves rendering segments x 2',      331, 's'));
    M.Append(TMenuItem.Create('Curves rendering segments / 2',      332, 'S'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set line width ...',                 336));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Change convex hull color ...',         341));
    M.Append(TMenuItem.Create('Change selected curve color ...',      342));
    M.Append(TMenuItem.Create('Change non-selected curves color ...', 343));
    M.Append(TMenuItem.Create('Change selected point color ...',      344));
    Result.Append(M);
  M := TMenu.Create('_Select');
    M.Append(TMenuItem.Create('_Nothing selected',                   403, 'n'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Select _next curve',                  404, 'c'));
    M.Append(TMenuItem.Create('Select _previous curve',              405, 'C'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Select next point on this curve',     406, 'p'));
    M.Append(TMenuItem.Create('Select previous point on this curve', 407, 'P'));
    M.Append(TMenuSeparator.Create);
      M2 := TMenu.Create('Set new curves _type');
      M2.Append(TMenuItem.Create('_Piecewise Cubic Bezier curve',       431));
      M2.Append(TMenuItem.Create('_Rational Bezier curve (Deprecated)', 432));
    M.Append(M2);
    Result.Append(M);
  M := TMenu.Create('_Edit');
    M.Append(TMenuItem.Create('_Scale curve...',                     401));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Delete selected _point',              408, 'd'));
    M.Append(TMenuItem.Create('Delete selected _curve',              409, 'D'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set _weight of selected point ...',   411));
    M.Append(TMenuItem.Create('Weight of selected point x 2',        412, 'w'));
    M.Append(TMenuItem.Create('Weight of selected point / 2',        413, 'W'));
    Result.Append(M);
  M := TMenu.Create('_Help');
    M.Append(TMenuItem.Create('Visit castle-curves website',  1010));
    M.Append(TMenuItem.Create('Donate to Castle Game Engine', 1020));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('About castle-curves',          1030));
    Result.Append(M);
end;

{ options ------------------------------------------------------------ }

const
  Options: array[0..1] of TOption = (
    (Short: 'h'; Long: 'help'; Argument: oaNone),
    (Short: 'v'; Long: 'version'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: begin
         InfoWrite(
           'castle-curves: create and edit curves for Castle Game Engine.' + NL +
           nl+
           'Available options are:' +nl+
           HelpOptionHelp +nl+
           VersionOptionHelp +nl+
           nl+
           TCastleWindowCustom.ParseParametersHelp(StandardParseOptions, true) +nl+
           nl+
           SCastleEngineProgramHelpSuffix('castle-curves', Version, true));
         Halt;
       end;
    1: begin
         WritelnStr(Version);
         Halt;
       end;
    else raise EInternalError.Create('OptionProc');
  end;
end;

{ main ------------------------------------------------------------ }

begin
  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Window := TCastleWindowCustom.Create(Application);

  Window.ParseParameters(StandardParseOptions);
  Parameters.Parse(Options, @OptionProc, nil);
  Parameters.CheckHighAtMost(1);

  { initialize variables }
  ColorConvexHull := Gray;
  ColorCurveSelected := Yellow;
  ColorCurveNotSelected := Green;
  ColorPointSelected := White;
  Theme.DialogsLight;

  if URIFileExists(ApplicationData('grid.png')) then
  begin
    BackgroundImageURL := ApplicationData('grid.png');
    BackgroundImage := TGLImage.Create(BackgroundImageURL);
  end;

  Curves := TControlPointsCurveList.Create(true);
  try
    StatusText := TStatusText.Create(Window);
    StatusText.Padding := 5;
    StatusText.Left := 5;
    StatusText.Bottom := 5;
    StatusText.Frame := true;
    Window.Controls.InsertFront(StatusText);

    Window.Controls.InsertBack(TCurvesDisplay.Create(Window));

    { init menu }
    Window.OnMenuClick := @MenuClick;
    Window.MainMenu := CreateMainMenu;

    { SetCurvesURL also initializes Window.Caption }
    if Parameters.High = 1 then
      LoadCurves(Parameters[1]) else
      SetCurvesURL('my_curves.xml');

    Window.OnPress := @Press;
    Window.OnRelease := @Release;
    Window.OnMotion := @Motion;
    Window.OnOpen := @Open;
    Window.OnUpdate := @Update;
    Window.OpenAndRun;
  finally
    FreeAndNil(Curves);
    FreeAndNil(BackgroundImage);
  end;
end.
