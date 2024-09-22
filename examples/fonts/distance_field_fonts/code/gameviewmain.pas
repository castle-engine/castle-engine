{
  Copyright 2023-2023 Eugene Loza.

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
  CastleVectors, CastleComponentSerialize, CastleGlImages,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFonts, CastleGlShaders;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    LabelAlphaProtaNormal,
      LabelAlphaProtaNormalYellow,
      LabelAlphaProtaDistance,
      LabelAlphaProtaDistanceYellow: TCastleLabel;
    LabelDejaVuSansNormal,
      LabelDejaVuSansNormalYellow,
      LabelDejaVuSansDistance,
      LabelDejaVuSansDistanceYellow: TCastleLabel;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    procedure Resize; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils, CastleImages;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.Resize;

  procedure AssertEquals(const Expected, Actual: Single);
  begin
    // just for debug
    // WritelnLog(Format('Expected %f, got %f', [Expected, Actual]));
    if Abs(Expected - Actual) > 0.01 then
      raise Exception.CreateFmt('Expected %f, but got %f', [Expected, Actual]);
  end;

begin
  inherited;

  // test that all labels have the same FontSize, which is expressed in scaled coordinates
  AssertEquals(LabelAlphaProtaNormal.FontSize, LabelAlphaProtaNormalYellow.FontSize);
  AssertEquals(LabelAlphaProtaNormal.FontSize, LabelAlphaProtaDistance.FontSize);
  AssertEquals(LabelAlphaProtaNormal.FontSize, LabelAlphaProtaDistanceYellow.FontSize);

  // test that all labels have the same rect size, in scaled coordinates
  AssertEquals(LabelAlphaProtaNormal.EffectiveHeight, LabelAlphaProtaNormalYellow.EffectiveHeight);
  AssertEquals(LabelAlphaProtaNormal.EffectiveHeight, LabelAlphaProtaDistance.EffectiveHeight);
  AssertEquals(LabelAlphaProtaNormal.EffectiveHeight, LabelAlphaProtaDistanceYellow.EffectiveHeight);

  // test that all labels have the same rect size, in final coordinates
  AssertEquals(LabelAlphaProtaNormal.RenderRect.Height, LabelAlphaProtaNormalYellow.RenderRect.Height);
  AssertEquals(LabelAlphaProtaNormal.RenderRect.Height, LabelAlphaProtaDistance.RenderRect.Height);
  AssertEquals(LabelAlphaProtaNormal.RenderRect.Height, LabelAlphaProtaDistanceYellow.RenderRect.Height);

  // same as above but now for DejaVuSans labels
  AssertEquals(LabelDejaVuSansNormal.FontSize, LabelDejaVuSansNormalYellow.FontSize);
  AssertEquals(LabelDejaVuSansNormal.FontSize, LabelDejaVuSansDistance.FontSize);
  AssertEquals(LabelDejaVuSansNormal.FontSize, LabelDejaVuSansDistanceYellow.FontSize);

  AssertEquals(LabelDejaVuSansNormal.EffectiveHeight, LabelDejaVuSansNormalYellow.EffectiveHeight);
  AssertEquals(LabelDejaVuSansNormal.EffectiveHeight, LabelDejaVuSansDistance.EffectiveHeight);
  AssertEquals(LabelDejaVuSansNormal.EffectiveHeight, LabelDejaVuSansDistanceYellow.EffectiveHeight);

  AssertEquals(LabelDejaVuSansNormal.RenderRect.Height, LabelDejaVuSansNormalYellow.RenderRect.Height);
  AssertEquals(LabelDejaVuSansNormal.RenderRect.Height, LabelDejaVuSansDistance.RenderRect.Height);
  AssertEquals(LabelDejaVuSansNormal.RenderRect.Height, LabelDejaVuSansDistanceYellow.RenderRect.Height);
end;

end.
