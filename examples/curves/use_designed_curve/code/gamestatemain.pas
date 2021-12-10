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
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    CurveScene: TCastleScene;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleCurves, CastleLog;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;

  procedure TestReadCurveFile;
  var
    FirstCurve: TCurve;
    Curves: TCurveList;
  begin
    FirstCurve := TCurve.LoadFromFile('castle-data:/my_curves.xml');
    try
      // That's it, you loaded the 1st curve from XML file.
      // Write some initial curve points.
      WritelnLog(FirstCurve.Point(0.0).ToString);
      WritelnLog(FirstCurve.Point(0.1).ToString);
    finally FreeAndNil(FirstCurve) end;

    { in more complicated scenarios, my_curves.xml may keep many curves
      inside. Load them like this: }

    Curves := TCurveList.Create(true { free objects });
    try
      Curves.LoadFromFile('castle-data:/my_curves.xml');
      if Curves.Count = 0 then
        raise Exception.Create('No curves defined in file');
      FirstCurve := Curves[0];
      // That's it, you have the 1st curve from XML file.
      // Write some initial curve points.
      WritelnLog(FirstCurve.Point(0.0).ToString);
      WritelnLog(FirstCurve.Point(0.1).ToString);
      WritelnLog(FirstCurve.Point(0.2).ToString);
    finally FreeAndNil(Curves) end;
  end;

begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  CurveScene := DesignedComponent('CurveScene') as TCastleScene;

  TestReadCurveFile;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
