{
  Copyright 2004-2016 Michalis Kamburelis.

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
  CastleGL, CastleWindow, CastleImages, CastleGLUtils,
  CastleUtils, CastleMessages, CastleCurves, CastleVectors, CastleFonts,
  CastleKeysMouse, CastleParameters, CastleClassUtils,
  CastleFilesUtils, CastleStringUtils, CastleColors, CastleURIUtils,
  CastleUIControls, CastleControls, CastleGLImages;

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

  RenderSegments: Cardinal = 100;
  ShowPoints: boolean = true;
  ShowSelectedCurveConvexHull: boolean = false;
  LineWidth: Float = 1;

  { Indicates dragging with mouse.
    So SelectedPoint and SelectedCurve never change during dragging
    (changing them by any means, like a menu command, breaks dragging).
    Always SelectedPoint <> -1 (so SelectedCurve also <>-1) when Dragging. }
  Dragging: boolean = false;
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

  BackgroundImage: TGLImageManaged;

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
  SErrSelectRationalCurve = 'You must select a Rational Bezier curve.';
  SErrSelectPiecewiseCubicBezierCurve = 'You must select a Piecewise Cubic Bezier curve.';
  SErrSelectRationalCurvePoint =
    'You must select some point on a Rational Bezier curve.';

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
    Text.Append(Format('  Curve type : %s', [Curves[SelectedCurve].NiceClassName]));
  Text.Append(Format('Selected point : %s', [IntToStrOrNone(SelectedPoint)]));
  {$warnings off}
  if RationalBezierCurvePointSelected then
    Text.Append(Format('  Weight : %f',
      [TRationalBezierCurve(Curves[SelectedCurve]).Weights[SelectedPoint]]));
  {$warnings on}
  Text.Append('');
  Text.Append(Format('Rendering segments = %d', [RenderSegments]));
  Text.Append(Format('New curves type = %s', [NewCurvesClass.NiceClassName]));

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

  GLClear([cbColor], Black);
  if BackgroundImage <> nil then
    BackgroundImage.Draw(Window.Rect);

  { draw convex hull of SelectedCurve }
  if ShowSelectedCurveConvexHull and (SelectedCurve <> -1) then
  begin
    Curves[SelectedCurve].ConvexHullColor := ColorConvexHull;
    Curves[SelectedCurve].RenderConvexHull;
  end;

  { draw all curves and their control points }
  for i := 0 to Curves.Count-1 do
  begin
    Curves[i].LineWidth := LineWidth;
    if i = SelectedCurve then
      Curves[i].Color := ColorCurveSelected else
      Curves[i].Color := ColorCurveNotSelected;
    Curves[i].ControlPointsColor := Curves[i].Color;
    if ShowPoints then Curves[i].RenderControlPoints;
    {$warnings off}
    { this program knowingly uses a lot of deprecated CastleCurves stuff }
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
end;

{ Add new curve point and select it. }
procedure AddNewPoint(const Event: TInputPressRelease);
var
  NewPoint: TVector3Single;
begin
  NewPoint := Vector3Single(Event.Position[0], Event.Position[1], 0);

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
      DraggingStartPosition := Event.Position;
    end;
  end;

begin
  if Event.IsMouseButton(mbLeft) then
  begin
    if (mkCtrl in Window.Pressed.Modifiers) or
       (SelectedPoint = -1) then
      AddNewPoint(Event) else
      StartDragging;
  end else
  if Event.IsMouseButton(mbRight) then
    SelectClosestPoint(Window.MousePosition) else
    Exit;

  Window.Invalidate;
end;

procedure Release(Container: TUIContainer; const Event: TInputPressRelease);
begin
  if Event.IsMouseButton(mbLeft) then
  begin
    Dragging := false;
    if VectorsEqual(DraggingStartPosition, Event.Position) then
      AddNewPoint(Event);
    Window.Invalidate;
  end;
end;

procedure Motion(Container: TUIContainer; const Event: TInputMotion);
var
  Move: TVector3Single;
  I: Integer;
begin
  if Dragging then
  begin
    Move[0] := Event.Position[0] - Event.OldPosition[0];
    Move[1] := Event.Position[1] - Event.OldPosition[1];
    Move[2] := 0;

    if not (mkShift in Window.Pressed.Modifiers) then
    begin
      Curves[SelectedCurve].ControlPoints.L[SelectedPoint] += Move;
    end else
    begin
      for i := 0 to Curves[SelectedCurve].ControlPoints.Count-1 do
        Curves[SelectedCurve].ControlPoints.L[i] += Move;
    end;

    Curves[SelectedCurve].UpdateControlPoints;
    Window.Invalidate;
  end;
end;

procedure Open(Container: TUIContainer);
begin
  glPointSize(10);
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

var
  s: string;
  NewWeight: Float;
  NewCurves: TControlPointsCurveList;
begin
  case MenuItem.IntData of
    4:  begin
          s := CurvesURL;
          if Window.FileDialog('Open curves from XML file', s, true,
            'All Files|*|*XML files|*.xml') then
          begin
            NewCurves := nil;
            try
              NewCurves := TControlPointsCurveList.Create(true);
              NewCurves.LoadFromFile(s);
            except
              on E: Exception do
              begin
                MessageOK(Window, 'Error while loading file "' +s +'" : ' + E.Message);
                FreeAndNil(NewCurves); // avoid memory leaks
                Exit;
              end;
            end;
            { only when loading NewCurves succeded -- then we free Curves }
            FreeAndNil(Curves);
            Curves := NewCurves;
            SetCurvesURL(s);
            if Curves.Count <> 0 then { select first curve, if available }
              SetSelectedCurve(0) else
              SetSelectedCurve(-1);
            if SelectedCurve = -1 then
              SetSelectedPoint(-1) else
              SetSelectedPoint(0);
          end;
        end;
    6:  begin
          s := CurvesURL;
          if Window.FileDialog('Save curves to XML file', s, false,
            'All Files|*|*XML files|*.xml') then
          begin
            Curves.SaveToFile(s);
            SetCurvesURL(s);
          end;
        end;
    8:  Window.SaveScreenDialog(FileNameAutoInc('castle-curves_%d.png'));
    10: Window.Close;

    201: FreeAndNil(BackgroundImage);
    202: begin
           S := '';
           if Window.FileDialog('Open background image', S, true,
             LoadImage_FileFilters) then
           begin
             FreeAndNil(BackgroundImage);
             BackgroundImage := TGLImageManaged.Create(S);
           end;
         end;

    305: StatusText.Exists := not StatusText.Exists;
    310: ShowPoints := not ShowPoints;
    320: ShowSelectedCurveConvexHull := not ShowSelectedCurveConvexHull;
    330: RenderSegments := Max(1, MessageInputCardinal(Window,
           'Render curves as ... segments ?', RenderSegments));
    331: RenderSegments *= 2;
    332: RenderSegments := Max(1, RenderSegments div 2);
    336: MessageInputQuery(Window, 'Input line width:', LineWidth);

    341: Window.ColorDialog(ColorConvexHull);
    342: Window.ColorDialog(ColorCurveSelected);
    343: Window.ColorDialog(ColorCurveNotSelected);
    344: Window.ColorDialog(ColorPointSelected);

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
    411: if RationalBezierCurvePointSelected then
         begin
           {$warnings off}
           s := FloatToStr(TRationalBezierCurve(Curves[SelectedCurve]).
             Weights[SelectedPoint]);
           {$warnings on}
           if MessageInputQuery(Window, 'Enter new weight:', s) then
           begin
             try
               NewWeight := StrToFloat(s);
             except
               on E: EConvertError do
               begin
                 MessageOK(Window, 'Invalid weight : '+E.Message);
                 Exit;
               end;
             end;

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
    M.Append(TMenuItem.Create('Save screen to _file ...', 8, K_F5));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('_Exit',                10, CharEscape));
    Result.Append(M);
  M := TMenu.Create('_Background');
    M.Append(TMenuItem.Create('_Clear',              201));
    M.Append(TMenuItem.Create('_Load from file ...', 202));
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
    M.Append(TMenuItem.Create('Set curves _rendering segments ...', 330));
    M.Append(TMenuItem.Create('Curves rendering segments x 2',     331, 's'));
    M.Append(TMenuItem.Create('Curves rendering segments / 2',     332, 'S'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set line width ...',                336));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Change convex hull color ...',         341));
    M.Append(TMenuItem.Create('Change selected curve color ...',      342));
    M.Append(TMenuItem.Create('Change non-selected curves color ...', 343));
    M.Append(TMenuItem.Create('Change selected point color ...',      344));
    Result.Append(M);
  M := TMenu.Create('_Edit');
    M.Append(TMenuItem.Create('_Nothing selected',                    403, 'n'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Select _next curve',                   404, 'c'));
    M.Append(TMenuItem.Create('Select _previous curve',               405, 'C'));
    M.Append(TMenuSeparator.Create);
      M2 := TMenu.Create('Set new curves _type');
      M2.Append(TMenuItem.Create('_Piecewise Cubic Bezier curve',       431));
      M2.Append(TMenuItem.Create('_Rational Bezier curve (Deprecated)', 432));
    M.Append(M2);
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Delete selected _point',               408, 'd'));
    M.Append(TMenuItem.Create('Delete selected _curve',               409, 'D'));
    Result.Append(M);
  M := TMenu.Create('Edit _points');
    M.Append(TMenuItem.Create('Select _next point on this curve',     406, 'p'));
    M.Append(TMenuItem.Create('Select _previous point on this curve', 407, 'P'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set _weight of selected point ...',    411));
    M.Append(TMenuItem.Create('Weight of selected point x 2',        412, 'w'));
    M.Append(TMenuItem.Create('Weight of selected point / 2',        413, 'W'));
    Result.Append(M);
end;

{ options ------------------------------------------------------------ }

const
  Version = '2.0.0';
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
  Window := TCastleWindowCustom.Create(Application);

  Window.ParseParameters(StandardParseOptions);
  Parameters.Parse(Options, @OptionProc, nil);

  { initialize variables }
  ColorConvexHull := Gray;
  ColorCurveSelected := Yellow;
  ColorCurveNotSelected := Green;
  ColorPointSelected := White;
  Theme.DialogsLight;

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

    { This also initializes Window.Caption }
    SetCurvesURL('my_curves.xml');

    Window.OnPress := @Press;
    Window.OnRelease := @Release;
    Window.OnMotion := @Motion;
    Window.OnOpen := @Open;
    Window.OpenAndRun;
  finally FreeAndNil(Curves) end;
end.
