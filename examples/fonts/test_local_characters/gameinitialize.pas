{
  Copyright 2017-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

uses CastleWindow;

var
  Window: TCastleWindow;

implementation

uses SysUtils, Classes, DOM,
  CastleScene, CastleControls, CastleLog, CastleSceneCore, CastleClassUtils,
  CastleFilesUtils, CastleUtils, CastleXMLUtils, CastleConfig, CastleURIUtils,
  CastleTextureFontData, CastleFonts, CastleUnicode, CastleStringUtils,
  X3DNodes, CastleUIControls, CastleColors, CastleVectors, CastleDownload,
  CastleApplicationProperties,
  Font_DejaVuSans, Font_DroidSansFallback;

{ TFontContainer ------------------------------------------------------------- }

type
  TFontContainer = class
  strict private
    procedure LoadFinish;
    procedure Load(const URL: string);
    procedure Load(const FontData: TTextureFontData);
  public
    MyFontData: TTextureFontData;
    MyFont: TTextureFont;
    constructor Create;
    destructor Destroy; override;
    procedure GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);
    procedure ButtonExternalFontClick(Sender: TObject);
    procedure ButtonExternalFontChineseClick(Sender: TObject);
    procedure ButtonEmbeddedFontClick(Sender: TObject);
    procedure ButtonEmbeddedFontChineseClick(Sender: TObject);
  end;

var
  FontContainer: TFontContainer;

constructor TFontContainer.Create;
begin
  inherited;
  MyFont := TTextureFont.Create(TComponent(nil));
end;

destructor TFontContainer.Destroy;
begin
  FreeAndNil(MyFont);
  inherited;
end;

procedure TFontContainer.GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);
begin
  Font := MyFontData;
end;

procedure TFontContainer.Load(const URL: string);
var
  AllCharacters: TUnicodeCharList;
begin
  try
    { Load from TTF font with support for international characters.
      See https://castle-engine.io/manual_text.php

      Note that in case of EFreeTypeLibraryNotFound in TTextureFontData.Create,
      no state is modified (MyFontData and MyFont stay unchanged). }

    AllCharacters := TUnicodeCharList.Create;
    try
      AllCharacters.Add(SimpleAsciiCharacters);
      AllCharacters.Add('你好世界');
      AllCharacters.Add('Γειασουκόσμε');
      AllCharacters.Add('Здравствуймир');
      AllCharacters.Add('ŚĆĘĄŹŁŻÓŃśćęąźłżóń');
      MyFontData := TTextureFontData.Create(URL, 20, true, AllCharacters);
    finally FreeAndNil(AllCharacters) end;

    MyFont.Load(MyFontData, true);
    LoadFinish;
  except
    on E: EFreeTypeLibraryNotFound do
      Window.MessageOK(E.Message, mtWarning);
  end;
end;

procedure TFontContainer.Load(const FontData: TTextureFontData);
begin
  MyFontData := FontData;
  MyFont.Load(FontData, false);
  LoadFinish;
end;

procedure TFontContainer.LoadFinish;
var
  I: Integer;
begin
  { use custom font by default in all TCastleLabel }
  UIFont := FontContainer.MyFont;
  { this is necessary to recalculate button sizes after UIFont measurements changed }
  for I := 0 to Window.Controls.Count - 1 do
    if Window.Controls[I] is TCastleUserInterfaceFont then
      TCastleUserInterfaceFont(Window.Controls[I]).FontChanged;

  { use custom font by default when rendering X3D text }
  TFontStyleNode.OnFont := @FontContainer.GetFont;
  { this is necessary to recalculate 3D shape of the text after font changes }
  if Window.SceneManager.MainScene <> nil then
    Window.SceneManager.MainScene.FontChanged;
end;

procedure TFontContainer.ButtonExternalFontClick(Sender: TObject);
begin
  FontContainer.Load('castle-data:/DejaVuSans.ttf');
end;

procedure TFontContainer.ButtonExternalFontChineseClick(Sender: TObject);
begin
  FontContainer.Load('castle-data:/DroidSansFallback.ttf');
end;

procedure TFontContainer.ButtonEmbeddedFontClick(Sender: TObject);
begin
  FontContainer.Load(TextureFont_DejaVuSans_20);
end;

procedure TFontContainer.ButtonEmbeddedFontChineseClick(Sender: TObject);
begin
  FontContainer.Load(TextureFont_DroidSansFallback_20);
end;

{ initialization ------------------------------------------------------------- }

{ One-time initialization of resources. }
procedure ApplicationInitialize;
var
  Background: TCastleRectangleControl;
  Scene: TCastleScene;
  TestLabel: TCastleLabel;
  Config: TCastleConfig;
  Y: Single;
  ButtonExternalFont, ButtonExternalFontChinese: TCastleButton;
  ButtonEmbeddedFont, ButtonEmbeddedFontChinese: TCastleButton;
  Doc: TXMLDocument;
  TextReader: TTextReader;
  StringList: TStringList;
begin
  FontContainer := TFontContainer.Create;
  FontContainer.ButtonEmbeddedFontClick(nil);

  Background := TCastleRectangleControl.Create(Application);
  Background.Color := Black;
  Background.FullSize := true;
  Window.Controls.InsertBack(Background);

  Scene := TCastleScene.Create(Application);
  Scene.Load('castle-data:/scene.x3dv');
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;
  Window.SceneManager.Items.Add(Scene);
  Window.SceneManager.MainScene := Scene;

  Window.SceneManager.FullSize := false;
  Window.SceneManager.Anchor(hpRight, -10);
  Window.SceneManager.Anchor(vpTop, -10);
  Window.SceneManager.Width := 800;
  Window.SceneManager.Height := 300;
  Window.SceneManager.BackgroundColor := Gray;
  Window.SceneManager.RequiredCamera.SetView(
    Vector3(2, -2, 10),
    Vector3(0.5, 0, -1),
    Vector3(0, 1, 0)
  ); // rotate camera, just to show it's 3D :)

  Y := 10;

  ButtonExternalFont := TCastleButton.Create(Application);
  ButtonExternalFont.Caption := 'Switch to external font (without Chinese chars)';
  ButtonExternalFont.OnClick := @FontContainer.ButtonExternalFontClick;
  ButtonExternalFont.Left := 10;
  ButtonExternalFont.Bottom := Y;
  Window.Controls.InsertFront(ButtonExternalFont);
  Y := Y + (ButtonExternalFont.EffectiveHeight + 10);

  ButtonExternalFontChinese := TCastleButton.Create(Application);
  ButtonExternalFontChinese.Caption := 'Switch to external font (only Chinese chars)';
  ButtonExternalFontChinese.OnClick := @FontContainer.ButtonExternalFontChineseClick;
  ButtonExternalFontChinese.Left := 10;
  ButtonExternalFontChinese.Bottom := Y;
  Window.Controls.InsertFront(ButtonExternalFontChinese);
  Y := Y + (ButtonExternalFontChinese.EffectiveHeight + 10);

  ButtonEmbeddedFont := TCastleButton.Create(Application);
  ButtonEmbeddedFont.Caption := 'Switch to embedded font (without Chinese chars)';
  ButtonEmbeddedFont.OnClick := @FontContainer.ButtonEmbeddedFontClick;
  ButtonEmbeddedFont.Left := 10;
  ButtonEmbeddedFont.Bottom := Y;
  Window.Controls.InsertFront(ButtonEmbeddedFont);
  Y := Y + (ButtonEmbeddedFont.EffectiveHeight + 10);

  ButtonEmbeddedFontChinese := TCastleButton.Create(Application);
  ButtonEmbeddedFontChinese.Caption := 'Switch to embedded font (only Chinese chars)';
  ButtonEmbeddedFontChinese.OnClick := @FontContainer.ButtonEmbeddedFontChineseClick;
  ButtonEmbeddedFontChinese.Left := 10;
  ButtonEmbeddedFontChinese.Bottom := Y;
  Window.Controls.InsertFront(ButtonEmbeddedFontChinese);
  Y := Y + (ButtonEmbeddedFontChinese.EffectiveHeight + 10);

  TestLabel := TCastleLabel.Create(Application);
  TestLabel.Caption := 'String hardcoded in Pascal sources:' + NL +
    'Chinese: 你好世界' + NL +
    'Greek: Γεια σου κόσμε!' + NL +
    'Russian: Здравствуй, мир!' + NL +
    'Polish: Witaj świecie! Oraz: źrebię ćma koń wężyk dąb!';
  TestLabel.Bottom := Y;
  TestLabel.Left := 100;
  TestLabel.Color := White;
  Window.Controls.InsertFront(TestLabel);
  Y := Y + (TestLabel.EffectiveHeight + 10);

  Config := TCastleConfig.Create(application);
  try
    Config.Load('castle-data:/example_config.xml');

    TestLabel := TCastleLabel.Create(Application);
    TestLabel.Caption := 'String from TCastleConfig:' + NL + Config.GetStringNonEmpty('value');
    TestLabel.Bottom := Y;
    TestLabel.Left := 100;
    TestLabel.Color := Green;
    Window.Controls.InsertFront(TestLabel);
    Y := Y + (TestLabel.EffectiveHeight + 10);
  finally FreeAndNil(Config) end;

  Doc := URLReadXML('castle-data:/example_strings.xml');
  try
    TestLabel := TCastleLabel.Create(Application);
    TestLabel.Caption := 'Strings from XML:' + NL +
      Doc.DocumentElement.ChildElement('element').TextData + NL +
      Doc.DocumentElement.ChildElement('element_with_multiline_content').TextData + NL +
      Doc.DocumentElement.ChildElement('element_with_attributes').AttributeString('value') + NL +
      Doc.DocumentElement.ChildElement('element_with_attributes').AttributeString('value_with_multiline_content');
    TestLabel.Bottom := Y;
    TestLabel.Left := 100;
    TestLabel.Color := Yellow;
    Window.Controls.InsertFront(TestLabel);
    Y := Y + (TestLabel.EffectiveHeight + 10);
  finally FreeAndNil(Doc) end;

  TextReader := TTextReader.Create('castle-data:/example_text.txt');
  try
    TestLabel := TCastleLabel.Create(Application);
    TestLabel.Caption := 'Strings from txt (TTextReader):' + NL +
      TextReader.Readln + NL +
      TextReader.Readln + NL +
      TextReader.Readln + NL +
      TextReader.Readln;
    TestLabel.Bottom := Y;
    TestLabel.Left := 100;
    TestLabel.Color := LightBlue;
    Window.Controls.InsertFront(TestLabel);
    Y := Y + (TestLabel.EffectiveHeight + 10);
  finally FreeAndNil(TextReader) end;

  StringList := TStringList.Create;
  try
    { On desktops, you could also load like this:
        StringList.LoadFromFile(URIToFilenameSafe('castle-data:/example_text.txt'), ...);
      But it will fail on Android, where the 'castle-data:/example_text.txt'
      is an URL inside "assets:/", it doesn't map to the filename.
      So instead we use StringList.LoadFromURL.
    }
    StringList.LoadFromURL('castle-data:/example_text.txt', TEncoding.UTF8);
    TestLabel := TCastleLabel.Create(Application);
    TestLabel.Caption := 'Strings from txt (TStringList):' + NL + StringList.Text;
    TestLabel.Bottom := Y;
    TestLabel.Left := 100;
    TestLabel.Color := Olive;
    Window.Controls.InsertFront(TestLabel);
    Y := Y + (TestLabel.EffectiveHeight + 10);
  finally FreeAndNil(StringList) end;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'test_local_characters';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window }
  Window := TCastleWindow.Create(Application);
  Window.Container.UIScaling := usEncloseReferenceSize;
  Window.Container.UIReferenceWidth := 1600;
  Window.Container.UIReferenceHeight := 900;
  Application.MainWindow := Window;
finalization
  FreeAndNil(FontContainer);
end.
