{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize, CastleCurves,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene, CastleTimeUtils;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    CurveScene: TCastleScene;
    SphereTransform: TCastleSphere;

    Curves: TCurveList;
    SpherePositionOnCurve: TFloatTime;
    { Set SphereTransform.Translation based on SpherePositionOnCurve,
      such that sphere follows along the curve. }
    procedure UpdateSpherePosition;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleLog, X3DNodes;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  procedure TestEvaluateCurve;
  var
    FirstCurve: TCurve;
  begin
    if Curves.Count = 0 then
      raise Exception.Create('No curves defined in file');

    FirstCurve := Curves[0];

    // That's it, you have the 1st curve from XML file.
    // Write some initial curve points.
    WritelnLog(FirstCurve.Point(0.0).ToString);
    WritelnLog(FirstCurve.Point(0.1).ToString);
    WritelnLog(FirstCurve.Point(0.2).ToString);
  end;

  function CreateNodesToDisplayCurve: TX3DRootNode;
  var
    CurveShape: TShapeNode;
    I: Integer;
  begin
    Result := TX3DRootNode.Create;
    for I := 0 to Curves.Count - 1 do
    begin
      CurveShape := TShapeNode.Create;
      CurveShape.Geometry := Curves[I].GeometryNode(100);
      Result.AddChildren(CurveShape);
    end;
  end;

begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  CurveScene := DesignedComponent('CurveScene') as TCastleScene;
  SphereTransform := DesignedComponent('SphereTransform') as TCastleSphere;

  Curves := TCurveList.Create(true { free children });
  Curves.LoadFromFile('castle-data:/my_curves.xml');

  TestEvaluateCurve;

  CurveScene.Load(CreateNodesToDisplayCurve, true);

  SpherePositionOnCurve := 0;
  UpdateSpherePosition;
end;

procedure TStateMain.Stop;
begin
  FreeAndNil(Curves);
  inherited;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;

  SpherePositionOnCurve := SpherePositionOnCurve + SecondsPassed * 0.1;
  UpdateSpherePosition;
end;

procedure TStateMain.UpdateSpherePosition;
begin
  SphereTransform.Translation := Curves[0].Point(Frac(SpherePositionOnCurve));
end;

end.
