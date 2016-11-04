{
  Copyright 2016-2016 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ The unit with common code for both Android and standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindowCustom;

implementation

uses SysUtils, CastleControls, CastleUtils, CastleFilesUtils,
  CastleColors, CastleUIControls;

var
  SimpleBackground: TCastleSimpleBackground;
  Rect: TCastleRectangleControl;
  InsideRect: TCastleRectangleControl;
  Image: TCastleImageControl;
  LabelStats: TCastleLabel;
  ButtonRun, ButtonFight: TCastleButton;

procedure ApplicationInitialize;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { If we descend from TCastleWindowCustom,
    we don't have anything to serve as the default background.
    So add a black background. }
  SimpleBackground := TCastleSimpleBackground.Create(Application);
  SimpleBackground.Color := Black;
  Window.Controls.InsertFront(SimpleBackground);

  Rect := TCastleRectangleControl.Create(Application);
  Rect.Width := 400;
  Rect.Height := 500;
  Rect.Color := HexToColor('5f3939'); // equivalent: Vector4Single(95/255, 57/255, 57/255, 1.0);
  Rect.Anchor(hpMiddle);
  Rect.Anchor(vpMiddle);
  Window.Controls.InsertFront(Rect);

  InsideRect := TCastleRectangleControl.Create(Application);
  InsideRect.Width := Rect.CalculatedWidth - 10;
  InsideRect.Height := Rect.CalculatedHeight - 10;
  InsideRect.Color := Silver;
  InsideRect.Anchor(hpMiddle);
  InsideRect.Anchor(vpMiddle);
  Rect.InsertFront(InsideRect);

  Image := TCastleImageControl.Create(Application);
  Image.URL := ApplicationData('Female-Zombie-300px.png');
  Image.Anchor(hpMiddle);
  Image.Anchor(vpTop, -10);
  InsideRect.InsertFront(Image);

  LabelStats := TCastleLabel.Create(Application);
  LabelStats.Color := Black;
  LabelStats.Html := true;
  { anything, just to show off the HTML :) }
  LabelStats.Caption := 'Statistics:' + NL +
    'Life: <font color="#ff0000">12%</font>' + NL +
    'Stamina: <font color="#ffff00">34%</font>' + NL +
    'Mana: <font color="#0000ff">56%</font>';
  LabelStats.Anchor(hpMiddle);
  LabelStats.Anchor(vpBottom, 100);
  InsideRect.InsertFront(LabelStats);

  ButtonRun := TCastleButton.Create(Application);
  ButtonRun.Caption := 'Run';
  ButtonRun.Anchor(hpLeft, 10);
  ButtonRun.Anchor(vpBottom, 10);
  ButtonRun.PaddingHorizontal := 40;
  InsideRect.InsertFront(ButtonRun);

  ButtonFight := TCastleButton.Create(Application);
  ButtonFight.Caption := 'Fight';
  ButtonFight.Anchor(hpRight, -10);
  ButtonFight.Anchor(vpBottom, 10);
  ButtonFight.PaddingHorizontal := 40;
  InsideRect.InsertFront(ButtonFight);
end;

function MyGetApplicationName: string;
begin
  Result := 'zombie_fighter';
end;

initialization
  { This should be done as early as possible to mark our log lines correctly. }
  OnGetApplicationName := @MyGetApplicationName;

  Window := TCastleWindowCustom.Create(Application);

  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
