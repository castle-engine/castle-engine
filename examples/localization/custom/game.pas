{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialisation and logic. }
unit Game;

interface

uses
  Classes, SysUtils,
  CastleWindow, CastleApplicationProperties,
  CastleControls, CastleUIControls,
  CastleFilesUtils, CastleLocalization;

type
  TLanguageButton = class (TCastleButtonLocalizable)
  protected
    FLanguageFile: String;
    procedure SetLanguage(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    property LanguageFile: String read FLanguageFile write FLanguageFile;
  end;

  TImageLocalized = class (TCastleImageControl)
  protected
    FURLPrefix, FURLPostfix: String;
  public
    procedure LoadImage;
    property URLPrefix: String read FURLPrefix write FURLPrefix;
    property URLPostfix: String read FURLPostfix write FURLPostfix;
  end;

var
  Window: TCastleWindow;
  ImageTop: TImageLocalized;

implementation

{ One-time initialisation. }
procedure ApplicationInitialize;
var
  MyLabel: TCastleLabelLocalizable;
  MyButton: TLanguageButton;
begin
  ImageTop := TImageLocalized.Create(Application);
  ImageTop.Anchor(hpMiddle);
  ImageTop.Anchor(vpTop, -10);
  ImageTop.URLPostfix := '.png';
  Localization.OnUpdateLocalization.Add(@ImageTop.LoadImage);
  Window.Controls.InsertFront(ImageTop);

  MyLabel := TCastleLabelLocalizable.Create(Application);
  MyLabel.Anchor(hpMiddle);
  MyLabel.Anchor(vpTop, -160);
  Localization.AddOrSet(MyLabel, 'LabelTop');
  Window.Controls.InsertFront(MyLabel);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -210);
  MyButton.LanguageFile := 'en.xml';
  Localization.AddOrSet(MyButton, 'ButtonEnglishXML');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -260);
  MyButton.LanguageFile := 'en.json';
  Localization.AddOrSet(MyButton, 'ButtonEnglishJSON');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -310);
  MyButton.LanguageFile := 'en.jsonobj';
  Localization.AddOrSet(MyButton, 'ButtonEnglishJSONObj');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -360);
  MyButton.LanguageFile := 'en.csv';
  Localization.AddOrSet(MyButton, 'ButtonEnglishCSV');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -410);
  MyButton.LanguageFile := 'en.mo';
  Localization.AddOrSet(MyButton, 'ButtonEnglishMO');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -460);
  MyButton.LanguageFile := 'de.xml';
  Localization.AddOrSet(MyButton, 'ButtonGermanXML');
  Window.Controls.InsertFront(MyButton);

  Localization.LanguageURL := ApplicationData('en.xml');
end;

constructor TLanguageButton.Create(AOwner: TComponent);
begin
  inherited;

  OnClick := @SetLanguage;
end;

procedure TLanguageButton.SetLanguage(Sender: TObject);
begin
  Localization.LanguageURL := ApplicationData(FLanguageFile);
end;

procedure TImageLocalized.LoadImage;
begin
  URL := ApplicationData(URLPrefix + Localization['LangCode'] + URLPostfix);
end;

initialization
  ApplicationProperties.ApplicationName := 'localization';

  { initialise Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialise Window callbacks }
  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

end.
