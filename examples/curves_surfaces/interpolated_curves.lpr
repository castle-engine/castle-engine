{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "interpolated_curves".

  "interpolated_curves" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "interpolated_curves" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "interpolated_curves"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ A demo of interpolated curves (polynomial curves that try to approximate
  any shape). See e.g. [http://www.ibiblio.org/e-notes/Splines/Lagrange.htm].

  Also, a practical demo of some CastleWindow and CastleCurves unit possibilities
  of Castle Game Engine. }

program interpolated_curves;

{$I castleconf.inc}

uses CastleGL, CastleWindow, CastleGLUtils, SysUtils, CastleVectors,
  CastleCurves, CastleScript, CastleMessages, CastleUIControls, CastleKeysMouse,
  CastleUtils, CastleScriptParser, CastleScriptCoreFunctions,
  CastleSceneManager, CastleControls, CastleInterpolatedCurves,
  Classes, CastleStringUtils, Castle3D, CastleColors;

{ global vars ------------------------------------------------------------ }

var
  Window: TCastleWindowCustom;

  { For the whole lifetime of a program we can have two curves.
    Precise one (must be TMathExprCurve) and approximating one
    (must be TControlPointsCurve).

    Basically, they don't have to be related. But usually they will,
    as this can be useful to see e.g. how some polynomial curve approximates
    some strictly defined TMathExprCurve. Some parts of the code will work
    "better" (better default point of view, better perspective near/far
    (so better depth buffer precision)) if PreciseCurve and ApproxCurv are
    similiar (at least, their BoundingBoxes are similiar).

    Set PreciseCurve only with SetPreciseCurve.
    Set ApproxCurve only with SetApproxCurve. }
  PreciseCurve: TCasScriptCurve;
  ApproxCurve: TControlPointsCurve;

  CurvesRenderSegments: Cardinal = 100;

  { Note: it's a bad idea to move ApproxCurveControlPointsVisible to
    a field inside TControlPointsCurve. That would make SetApproxCurve
    always reset ApproxCurveControlPointsVisible to true - and this would be
    not comfortable for user.
    Same thing goes with PreciseCurveVisible, ApproxCurveVisible. }
  PreciseCurveVisible: boolean = true;
  ApproxCurveControlPointsVisible: boolean = true;
  ApproxCurveVisible: boolean = true;

  { Those are attributes that determine some properties of newly created
    ApproxCurve. They are used in SetApproxCurve.
    Always after changing them we call SetApproxCurve, so these attributes
    correspond to ApproxCurve attributes. }
  ApproxCurveControlPointsCount: Cardinal = 10;
  ApproxCurveClass: TControlPointsCurveClass { = TLagrangeInterpolatedCurve };

  { TVariable used in all TCasScriptCurve curves (sharing doesn't do any
    harm in this case, as each curve will set it from beginnig to end
    anyway). }
  TVariable: TCasScriptFloat;

{ scene manager -------------------------------------------------------------- }

type
  TMySceneManager = class(TCastleSceneManager)
  protected
    procedure Render3D(const Params: TRenderParams); override;
  end;

var
  SceneManager: TMySceneManager;

procedure TMySceneManager.Render3D(const Params: TRenderParams);
begin
  { Do not call "inherited", i.e. do not let default PreciseCurve, ApproxCurve
    rendering, because we want to take into account our variables
    (PreciseCurveVisible, CurvesRenderSegments, our colors etc.) }

  if (not Params.Transparent) and Params.ShadowVolumesReceivers then
  begin
    if PreciseCurveVisible then
    begin
      glColorv(White);
      PreciseCurve.Render(CurvesRenderSegments);
    end;
    if ApproxCurveVisible then
    begin
      glColorv(Yellow);
      ApproxCurve.Render(CurvesRenderSegments);
    end;
    if ApproxCurveControlPointsVisible then
    begin
      glColorv(LightBlue);
      ApproxCurve.RenderControlPoints;
    end;
  end;
end;

{ SetXxxCurve ------------------------------------------------------------ }

procedure SetApproxCurve; forward;

{ Frees PreciseCurve,
  Frees ApproxCurve,
  Creates new PreciseCurve,
  does SetApproxCurve,
  then (because PreciseCurve.BoundingBox changed) releases SceneManager.Camera.
    This will cause SceneManager to recreate
    camera and projection suitable for current BoundingBox.
  Window.Invalidate;

  References to NewX/Y/ZFunction will be copied, so do NOT Free
  NewX/Y/ZFunction yourself after calling this procedure. }
procedure SetPreciseCurve(
  NewXFunction, NewYFunction, NewZFunction: TCasScriptExpression;
  const NewTBegin, NewTEnd: Float);
begin
  FreeAndNil(PreciseCurve);
  PreciseCurve := TCasScriptCurve.Create(nil);
  PreciseCurve.TBegin := NewTBegin;
  PreciseCurve.TEnd := NewTEnd;
  PreciseCurve.XFunction := NewXFunction;
  PreciseCurve.YFunction := NewYFunction;
  PreciseCurve.ZFunction := NewZFunction;
  PreciseCurve.TVariable := TVariable;
  SceneManager.Items.Add(PreciseCurve);

  SetApproxCurve;

  SceneManager.Camera.Free;

  Window.Invalidate;
end;

{ Frees ApproxCurve,
  Creates ApproxCurve (based on ApproxCurveXxx variables and PreciseCurve)
  Window.Invalidate; }
procedure SetApproxCurve;
begin
  FreeAndNil(ApproxCurve);
  ApproxCurve := ApproxCurveClass.CreateFromEquation(
    PreciseCurve, ApproxCurveControlPointsCount);
  SceneManager.Items.Add(ApproxCurve);

  Window.Invalidate;
end;

{ TStatusText ---------------------------------------------------------------- }

type
  TStatusText = class(TCastleLabel)
    procedure Render; override;
  end;

procedure TStatusText.Render;
begin
  if not GetExists then Exit;

  { regenerate Text contents at every Render call }
  Text.Clear;

  { TODO: ToString not implemented yet.
  Text.Append('Precise curve:');
  Text.Append(Format('  x(t) = %s', [PreciseCurve.XFunction.ToString]));
  Text.Append(Format('  y(t) = %s', [PreciseCurve.YFunction.ToString]));
  Text.Append(Format('  z(t) = %s', [PreciseCurve.ZFunction.ToString])); }
  Text.Append('Approximating curve:');
  Text.Append(Format('  Class = %s', [ApproxCurveClass.ClassName]));
  Text.Append(Format('  Control points count = %d', [ApproxCurveControlPointsCount]));
  Text.Append(Format('Rendering segments = %d', [CurvesRenderSegments]));

  inherited;
end;

var
  StatusText: TStatusText;

procedure Open(Container: TUIContainer);
begin
  RenderContext.PointSize := 10;
end;

{ menu things ------------------------------------------------------------ }

var
  { modified and used only in TMenuItemPredefinedPreciseCurve.Create }
  PredefinedPreciseCurve: Cardinal = 0;

type
  TMenuItemPredefinedPreciseCurve = class(TMenuItem)
  public
    XFunctionString, YFunctionString, ZFunctionString,
      TBeginString, TEndString: string;

    { All strings must be parseable with ParseMathExpr.
      This class defines DoClick that will call SetNewPreciseCurve. }
    constructor Create(const AXFunctionString, AYFunctionString, AZFunctionString,
      ATBeginString, ATEndString: string);
    function DoClick: boolean; override;
  end;

  constructor TMenuItemPredefinedPreciseCurve.Create(
    const AXFunctionString, AYFunctionString, AZFunctionString,
      ATBeginString, ATEndString: string);
  begin
    XFunctionString :=  AXFunctionString;
    YFunctionString :=  AYFunctionString;
    ZFunctionString :=  AZFunctionString;
    TBeginString    :=  ATBeginString;
    TEndString      :=  ATEndString;
    inherited Create(Format('%d. Create [%s, %s, %s], t = [%s .. %s]',
      [PredefinedPreciseCurve,
       XFunctionString, YFunctionString, ZFunctionString,
       TBeginString, TEndString]), -1);
    Inc(PredefinedPreciseCurve);
  end;

  function TMenuItemPredefinedPreciseCurve.DoClick: boolean;
  begin
    inherited;
    SetPreciseCurve(
      ParseFloatExpression(XFunctionString, [TVariable]),
      ParseFloatExpression(YFunctionString, [TVariable]),
      ParseFloatExpression(ZFunctionString, [TVariable]),
      ParseConstantFloatExpression(TBeginString),
      ParseConstantFloatExpression(TEndString) );
    Result := true;
  end;

type
  TMenuItemApproxClass = class(TMenuItem)
  private
    FNewApproxClass: TControlPointsCurveClass;
  public
    constructor Create(ANewApproxClass: TControlPointsCurveClass);
    function DoClick: boolean; override;
  end;

  constructor TMenuItemApproxClass.Create(ANewApproxClass: TControlPointsCurveClass);
  begin
    inherited Create('Set class to "' +ANewApproxClass.ClassName +'"', -1);
    FNewApproxClass := ANewApproxClass;
  end;

  function TMenuItemApproxClass.DoClick: boolean;
  begin
    inherited;
    ApproxCurveClass := FNewApproxClass;
    SetApproxCurve;
    Result := true;
  end;

procedure MenuClick(Container: TUIContainer; MenuItem: TMenuItem);

  { Inputs a string from user. User can accept the string or cancel
    operation. If user will cancel operation -- we will return false.
    If user will accept operation but parsing his input as MathExpr
    (using ParseMathExpr) fails -- we will give him appropriate MessageOK
    with appropriate error message, and return false too.
    Only if he will accept operation and provide valid MathExpr we will
    create new Expr and return true. }
  function MessageInputQueryFunction(const Prompt: string;
    var Expr: TCasScriptExpression): boolean;
  var
    ExprString: string;
  begin
    ExprString := '';
    Result := MessageInputQuery(Window, Prompt, ExprString);
    if Result then
    begin

      try
        Expr := ParseFloatExpression(ExprString, [TVariable]);
      except on E: ECasScriptSyntaxError do
        begin
          MessageOK(Window, ExceptMessage(E, nil));
          Result := false;
          Exit;
        end;
      end;

    end;
  end;

var
  NewXFunction, NewYFunction, NewZFunction: TCasScriptExpression;
  NewTBegin, NewTEnd: Float;
begin
  case MenuItem.IntData of
    2: Window.Close;

    201: StatusText.Exists := not StatusText.Exists;
    202: CurvesRenderSegments := Max(1,
           MessageInputCardinal(Window, 'Render curves as ... segments ?',
             CurvesRenderSegments));
    203: CurvesRenderSegments := CurvesRenderSegments * 2;
    204: CurvesRenderSegments := Max(2, CurvesRenderSegments div 2);

    300: PreciseCurveVisible := not PreciseCurveVisible;
    301: begin
           if MessageInputQueryFunction('Input x(t) = ', NewXFunction) and
              MessageInputQueryFunction('Input y(t) = ', NewYFunction) and
              MessageInputQueryFunction('Input z(t) = ', NewZFunction) then
           begin
             NewTBegin := MessageInputCardinal(Window, 'Input starting t value :', '');
             NewTEnd := MessageInputCardinal(Window, 'Input ending t value :', '');
             SetPreciseCurve(NewXFunction, NewYFunction, NewZFunction,
               NewTBegin, NewTEnd);
           end;
         end;

    400: ApproxCurveVisible := not ApproxCurveVisible;
    401: ApproxCurveControlPointsVisible := not ApproxCurveControlPointsVisible;
    402: begin
           ApproxCurveControlPointsCount := Max(2, MessageInputCardinal(
             Window, 'How many control points ?', ''));
           SetApproxCurve;
         end;
    403: begin
           ApproxCurveControlPointsCount := ApproxCurveControlPointsCount * 2;
           SetApproxCurve;
         end;
    404: begin
           ApproxCurveControlPointsCount := Max(2, ApproxCurveControlPointsCount div 2);
           SetApproxCurve;
         end;
    else raise EInternalError.Create('not impl menu item');
  end;
  Window.Invalidate;
end;

function CreateMainMenu: TMenu;
var
  M: TMenu;
begin
  Result := TMenu.Create('Main menu');
    M := TMenu.Create('File');
    M.Append(TMenuItemToggleFullScreen.Create(Window.FullScreen));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Exit',                     2, CharEscape));
  Result.Append(M);
    M := TMenu.Create('View');
    M.Append(TMenuItemChecked.Create('Show / Hide status',         201, K_F1,
      StatusText.Exists, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set curves rendering segments ...', 202));
    M.Append(TMenuItem.Create('Curves rendering segments x 2',     203, 's'));
    M.Append(TMenuItem.Create('Curves rendering segments / 2',     204, 'S'));
  Result.Append(M);
    M := TMenu.Create('Precise curve');
    M.Append(TMenuItemChecked.Create('Show / Hide', 300,
      PreciseCurveVisible, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Create ...',         301));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemPredefinedPreciseCurve.Create('cos(t)', 'sin(t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('2*cos(t)', 'sin(t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('sin(1+t^2)', 'cos(1-t^2)', '0', '0', '2.51'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('sin(t)', 'cos(3*t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('sin(2*t)', 'cos(3*t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('cos(t)', 'sin(2*t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('t*cos(t)', 'sin(t)', '0', '0', '9*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('t*cos(t)', 't*sin(t)', '0', '0', '10*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('t*cos(t)', 't*sin(2*t)', '0', '0', '12.5*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('t*cos(t)', 't', '0', '0', '15*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('2.5*cos(t)*(cos(t)+1)', '2*sin(2*t)', '0', '0', '2*Pi'));
    M.Append(TMenuItemPredefinedPreciseCurve.Create('2* (cos(t)^3)', '2* (sin(t)^3)', '0', '0', '2*Pi'));
  Result.Append(M);
    M := TMenu.Create('Approximating curve');
    M.Append(TMenuItemChecked.Create('Show / Hide',                400,
      ApproxCurveVisible, true));
    M.Append(TMenuItemChecked.Create('Show / Hide control points', 401,
      ApproxCurveControlPointsVisible, true));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItem.Create('Set control points count ...',      402));
    M.Append(TMenuItem.Create('Control points count x 2',          403, 'c'));
    M.Append(TMenuItem.Create('Control points count / 2',          404, 'C'));
    M.Append(TMenuSeparator.Create);
    M.Append(TMenuItemApproxClass.Create(TLagrangeInterpolatedCurve));
    M.Append(TMenuItemApproxClass.Create(TNaturalCubicSplineCurve));
    M.Append(TMenuItemApproxClass.Create(TNaturalCubicSplineCurveAlwaysClosed));
    M.Append(TMenuItemApproxClass.Create(TNaturalCubicSplineCurveNeverClosed));
  Result.Append(M);
end;

{ main ------------------------------------------------------------ }

begin
  Window := TCastleWindowCustom.Create(Application);

  try
    ApproxCurveClass := TLagrangeInterpolatedCurve;

    TVariable := TCasScriptFloat.Create(false);
    TVariable.Name := 't';
    TVariable.OwnedByParentExpression := false;

    StatusText := TStatusText.Create(Window);
    StatusText.Padding := 5;
    StatusText.Left := 5;
    StatusText.Bottom := 5;
    StatusText.Frame := true;
    Window.Controls.InsertFront(StatusText);

    { init menu }
    Window.OnMenuClick := @MenuClick;
    Window.MainMenu := CreateMainMenu;

    SceneManager := TMySceneManager.Create(Application);
    Window.Controls.InsertFront(SceneManager);

    { init PreciseCurve }
    SetPreciseCurve(
      TCasScriptSin.Create([TVariable]),
      TCasScriptCos.Create([TVariable]),
      TVariable,
      0, 4*Pi);

    { open Window, go ! }
    Window.OnOpen := @Open;
    Window.OpenAndRun;
  finally
    FreeAndNil(PreciseCurve);
    FreeAndNil(ApproxCurve);
    FreeAndNil(TVariable);
  end;
end.
