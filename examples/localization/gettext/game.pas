{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit Game;

interface

uses CastleWindow;

implementation

uses SysUtils, Classes, GetText,
  CastleControls, CastleUtils, CastleColors, CastleUIControls, CastleVectors,
  CastleApplicationProperties, X3DNodes, CastleFonts, CastleTimeUtils,
  CastleTextureFontData, CastleScene, CastleSceneManager, CastleMessages,
  CastleLog, CastleFilesUtils, CastleURIUtils, CastleSystemLanguage,
  Font_DejaVuSans;

var
  Window: TCastleWindowCustom;

{ Translatable strings ------------------------------------------------------- }

{ Note that we use English as "translation id".
  But this is optional, you could as well use some internal-only ids. }

resourcestring
  Text3D = 'I am 3D Text';
  CaptionButtonSwitchEnglish = 'English';
  CaptionButtonSwitchGerman = 'German';
  CaptionButtonSwitchPolish = 'Polish';
  CaptionButtonSwitchRussian = 'Russian';
  CaptionButtonSwitchUkrainian = 'Ukrainian';
  CaptionButtonMessage = 'Show Message';
  EditTextInitial = 'Initial Edit Contents';
  CaptionLabel = 'My Label';
  MessageText = 'I am a message!';

{ TApplicationLogic ---------------------------------------------------------- }

type
  { TApplicationLogic contains almost all logic of this application,
    handling button clicks, setting up user-interface and so on. }
  TApplicationLogic = class(TComponent)
  strict private
    Language: string;

    { For animating rotating text. }
    LifeTime: TFloatTime;
    TextTransform: TTransformNode;

    { Things that contain text (and need to be modified by SwitchLanguage). }
    TextNode: TTextNode;
    Edit: TCastleEdit;
    ButtonSwitchEnglish, ButtonSwitchGerman, ButtonSwitchPolish,
      ButtonSwitchRussian, ButtonSwitchUkrainian: TCastleButton;
    ButtonMessage: TCastleButton;
    Lab: TCastleLabel;

    { Switch the user-interface that depends on localization.
      This allows to switch localization in the middle of the game.
      In a larger application, it may be easier to just tell user
      "Please restart the application in order for language change to take effect.",
      instead of maintaining such method. }
    procedure SwitchLanguage(const NewLanguage: string);

    procedure GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);

    procedure ClickButtonMessage(Sender: TObject);
    procedure ClickButtonEnglish(Sender: TObject);
    procedure ClickButtonGerman(Sender: TObject);
    procedure ClickButtonPolish(Sender: TObject);
    procedure ClickButtonRussian(Sender: TObject);
    procedure ClickButtonUkrainian(Sender: TObject);
  public
    { Animate text rotation. }
    procedure Update(const SecondsPassed: TFloatTime);

    { Constructor initializes all user-inteface. }
    constructor Create(AOwner: TComponent); override;
  end;

constructor TApplicationLogic.Create(AOwner: TComponent);

  function BuildScene: TX3DRootNode;
  var
    FontStyle: TFontStyleNode;
    TextShape: TShapeNode;
  begin
    FontStyle := TFontStyleNode.Create;
    FontStyle.Justify := fjMiddle;
    FontStyle.JustifyMinor := fjMiddle;
    FontStyle.Size := 100;

    TextNode := TTextNode.CreateWithTransform(TextShape, TextTransform);
    TextNode.FontStyle := FontStyle;
    TextNode.Solid := false;
    TextNode.SetString([Text3D]);

    TextShape.Material := TMaterialNode.Create;
    TextShape.Material.EmissiveColor := YellowRGB;
    TextShape.Material.ForcePureEmissive;

    Result := TX3DRootNode.Create;
    Result.AddChildren(TextTransform);
  end;

var
  Scene: TCastleScene;
  SceneManager: TCastleSceneManager;
  BottomGroup: TCastleVerticalGroup;
  ButtonsToSwitchLanguage: TCastleHorizontalGroup;
begin
  inherited;

  { Make sure the font contains all international characters.
    TFontStyleNode.OnFont is used by TTextNode.
    Everything else uses UIFont (by default). }
  TFontStyleNode.OnFont := @GetFont;
  UIFont := TTextureFont.Create(Self);
  (UIFont as TTextureFont).Load(TextureFont_DejaVuSans_50, false);
  UIFont.Size := 20;

  { build 3D view }

  Scene := TCastleScene.Create(Application);
  Scene.Load(BuildScene, true);
  Scene.ProcessEvents := true;

  SceneManager := TCastleSceneManager.Create(Application);
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;
  SceneManager.FullSize := true;
  SceneManager.BackgroundColor := Vector4(0.1, 0.1, 0.1, 1);
  Window.Controls.InsertBack(SceneManager);

  { build 2D user-inteface }

  BottomGroup := TCastleVerticalGroup.Create(Application);
  BottomGroup.TopToBottom := false;
  BottomGroup.Anchor(vpBottom, 10);
  BottomGroup.Anchor(hpLeft, 10);
  BottomGroup.Frame := true;
  Window.Controls.InsertFront(BottomGroup);

  ButtonsToSwitchLanguage := TCastleHorizontalGroup.Create(Application);
  ButtonsToSwitchLanguage.Padding := 0;
  BottomGroup.InsertFront(ButtonsToSwitchLanguage);

  ButtonSwitchEnglish := TCastleButton.Create(Application);
  ButtonSwitchEnglish.Caption := CaptionButtonSwitchEnglish;
  ButtonSwitchEnglish.OnClick := @ClickButtonEnglish;
  ButtonsToSwitchLanguage.InsertFront(ButtonSwitchEnglish);

  ButtonSwitchGerman := TCastleButton.Create(Application);
  ButtonSwitchGerman.Caption := CaptionButtonSwitchGerman;
  ButtonSwitchGerman.OnClick := @ClickButtonGerman;
  ButtonsToSwitchLanguage.InsertFront(ButtonSwitchGerman);

  ButtonSwitchPolish := TCastleButton.Create(Application);
  ButtonSwitchPolish.Caption := CaptionButtonSwitchPolish;
  ButtonSwitchPolish.OnClick := @ClickButtonPolish;
  ButtonsToSwitchLanguage.InsertFront(ButtonSwitchPolish);

  ButtonSwitchRussian := TCastleButton.Create(Application);
  ButtonSwitchRussian.Caption := CaptionButtonSwitchRussian;
  ButtonSwitchRussian.OnClick := @ClickButtonRussian;
  ButtonsToSwitchLanguage.InsertFront(ButtonSwitchRussian);

  ButtonSwitchUkrainian := TCastleButton.Create(Application);
  ButtonSwitchUkrainian.Caption := CaptionButtonSwitchUkrainian;
  ButtonSwitchUkrainian.OnClick := @ClickButtonUkrainian;
  ButtonsToSwitchLanguage.InsertFront(ButtonSwitchUkrainian);

  ButtonMessage := TCastleButton.Create(Application);
  ButtonMessage.Caption := CaptionButtonMessage;
  ButtonMessage.OnClick := @ClickButtonMessage;
  BottomGroup.InsertFront(ButtonMessage);

  Edit := TCastleEdit.Create(Application);
  Edit.Text := EditTextInitial;
  Edit.Width := 400;
  BottomGroup.InsertFront(Edit);

  Lab := TCastleLabel.Create(Application);
  Lab.Caption := CaptionLabel;
  BottomGroup.InsertFront(Lab);

  Language := SystemLanguage;
  { if we don't have a translation file for this language, fallback to English }
  if not URIFileExists(ApplicationData('locale/game.' + Language + '.mo')) then
    Language := SystemDefaultLanguage;

  SwitchLanguage(Language);
end;

procedure TApplicationLogic.Update(const SecondsPassed: TFloatTime);
begin
  LifeTime += Window.Fps.SecondsPassed;
  TextTransform.Rotation := Vector4(0, 1, 0, LifeTime);
end;

procedure TApplicationLogic.GetFont(const FontStyle: TFontStyleNode; var Font: TTextureFontData);
begin
  Font := TextureFont_DejaVuSans_50;
end;

procedure TApplicationLogic.ClickButtonMessage(Sender: TObject);
begin
  MessageOK(Window, MessageText);
end;

procedure TApplicationLogic.SwitchLanguage(const NewLanguage: string);
var
  EditTextModified: boolean;
begin
  Language := NewLanguage;

  EditTextModified := Edit.Text <> EditTextInitial;

  TranslateResourceStrings(
    URIToFilenameSafe(ApplicationData('locale/game.' + Language + '.mo')));

  TextNode.SetString([Text3D]);
  ButtonSwitchEnglish.Caption := CaptionButtonSwitchEnglish;
  ButtonSwitchGerman.Caption := CaptionButtonSwitchGerman;
  ButtonSwitchPolish.Caption := CaptionButtonSwitchPolish;
  ButtonSwitchRussian.Caption := CaptionButtonSwitchRussian;
  ButtonSwitchUkrainian.Caption := CaptionButtonSwitchUkrainian;
  ButtonMessage.Caption := CaptionButtonMessage;
  Lab.Caption := CaptionLabel;

  if not EditTextModified then
    Edit.Text := EditTextInitial;
end;

procedure TApplicationLogic.ClickButtonEnglish(Sender: TObject);
begin
  SwitchLanguage('en');
end;

procedure TApplicationLogic.ClickButtonGerman(Sender: TObject);
begin
  SwitchLanguage('de');
end;

procedure TApplicationLogic.ClickButtonPolish(Sender: TObject);
begin
  SwitchLanguage('pl');
end;

procedure TApplicationLogic.ClickButtonRussian(Sender: TObject);
begin
  SwitchLanguage('ru');
end;

procedure TApplicationLogic.ClickButtonUkrainian(Sender: TObject);
begin
  SwitchLanguage('ua');
end;

{ ----------------------------------------------------------------------------
  CastleWindow events, that mostly just call ApplicationLogic methods }

var
  ApplicationLogic: TApplicationLogic;

procedure WindowUpdate(Container: TUIContainer);
begin
  ApplicationLogic.Update(Window.Fps.SecondsPassed);
end;

procedure ApplicationInitialize;
begin
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  Window.OnUpdate := @WindowUpdate;

  ApplicationLogic := TApplicationLogic.Create(Application);
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'localization_test';

  InitializeLog;

  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
  Application.OnInitialize := @ApplicationInitialize;
end.
