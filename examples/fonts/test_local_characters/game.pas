{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Demo that displays strings with local characters
  (Chinese, Greek, Russian... from https://helloworldcollection.github.io/#Human ).
  Strings come from various sources (XML, hardcoded in Pascal, X3D...).
  This shows that internationalization support in Castle Game Engine works
  nicely.

  This unit implements the game logic, independent from mobile / standalone. }
unit Game;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils, CastleScene, CastleControls, CastleLog, CastleSceneCore,
  CastleFilesUtils, CastleUtils, CastleXMLUtils, CastleConfig,
  CastleTextureFontData, CastleFonts, CastleUnicode, CastleStringUtils,
  X3DNodes;

{ TFontContainer ------------------------------------------------------------- }

type
  TFontContainer = class
    MyFontData: TTextureFontData;
    MyFont: TTextureFont;
    constructor Create;
    procedure GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);
  end;

var
  FontContainer: TFontContainer;

constructor TFontContainer.Create;
var
  AllCharacters: TUnicodeCharList;
begin
  inherited;

  { Load from TTF font with support for international characters.
    See https://castle-engine.sourceforge.io/manual_text.php
    TODO: test also font embedded in a Pascal unit. }

  AllCharacters := TUnicodeCharList.Create;
  try
    AllCharacters.Add(SimpleAsciiCharacters);
    // TODO: I think that DejaVuSans.ttf doesn't contain Chinese chars?
    // Use other font.
    AllCharacters.Add('你好世界');
    AllCharacters.Add('Γειασουκόσμε');
    AllCharacters.Add('Здравствуймир');
    MyFontData := TTextureFontData.Create(ApplicationData('DejaVuSans.ttf'), 20, true, AllCharacters);
  finally FreeAndNil(AllCharacters) end;

  MyFont := TTextureFont.Create(Application);
  MyFont.Load(MyFontData, true);
end;

procedure TFontContainer.GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);
begin
  Font := MyFontData;
end;

{ initialization ------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Scene: TCastleScene;
  TestLabel: TCastleLabel;
  Config: TCastleConfig;
  Y: Integer;
begin
  FontContainer := TFontContainer.Create;
  { use custom font by default in all TCastleLabel }
  UIFont := FontContainer.MyFont;
  { use custom font by default when rendering X3D text }
  TFontStyleNode.OnFont := @FontContainer.GetFont;

  Y := 100;

  TestLabel := TCastleLabel.Create(Application);
  TestLabel.Caption := 'String hardcoded in Pascal sources:' + NL +
    'Chinese: 你好世界' + NL +
    'Greek: Γεια σου κόσμε!' + NL +
    'Russian: Здравствуй, мир!';
  TestLabel.Bottom := Y;
  TestLabel.Left := 100;
  Window.Controls.InsertFront(TestLabel);
  Y += 200;

  Config := TCastleConfig.Create(application);
  try
    Config.Load(ApplicationData('example_config.xml'));

    TestLabel := TCastleLabel.Create(Application);
    TestLabel.Caption := Config.GetStringNonEmpty('value');
    TestLabel.Bottom := Y;
    TestLabel.Left := 100;
    Window.Controls.InsertFront(TestLabel);
    Y += 200;
  finally FreeAndNil(Config) end;

  Scene := TCastleScene.Create(Application);
  Scene.Load(ApplicationData('scene.x3dv'));
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;
end;

function MyGetApplicationName: string;
begin
  Result := 'test_local_characters';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;
finalization
  FreeAndNil(FontContainer);
end.
