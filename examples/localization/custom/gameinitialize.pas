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
unit GameInitialize;

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
  Window: TCastleWindowBase;
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
  ImageTop.URLPrefix := 'castle-data:/';
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
  MyButton.LanguageFile := 'castle-data:/en.xml';
  Localization.AddOrSet(MyButton, 'ButtonEnglishXML');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -260);
  MyButton.LanguageFile := 'castle-data:/en.json';
  Localization.AddOrSet(MyButton, 'ButtonEnglishJSON');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -310);
  MyButton.LanguageFile := 'castle-data:/en.jsonobj';
  Localization.AddOrSet(MyButton, 'ButtonEnglishJSONObj');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -360);
  MyButton.LanguageFile := 'castle-data:/en.csv';
  Localization.AddOrSet(MyButton, 'ButtonEnglishCSV');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -410);
  MyButton.LanguageFile := 'castle-data:/en.mo';
  Localization.AddOrSet(MyButton, 'ButtonEnglishMO');
  Window.Controls.InsertFront(MyButton);

  MyButton := TLanguageButton.Create(Application);
  MyButton.Anchor(hpMiddle);
  MyButton.Anchor(vpTop, -460);
  MyButton.LanguageFile := 'castle-data:/de.xml';
  Localization.AddOrSet(MyButton, 'ButtonGermanXML');
  Window.Controls.InsertFront(MyButton);

  Localization.LanguageURL := 'castle-data:/en.xml';
end;

constructor TLanguageButton.Create(AOwner: TComponent);
begin
  inherited;

  OnClick := @SetLanguage;
end;

procedure TLanguageButton.SetLanguage(Sender: TObject);
begin
  Localization.LanguageURL := FLanguageFile;
end;

procedure TImageLocalized.LoadImage;
begin
  URL := URLPrefix + Localization['LangCode'] + URLPostfix;
end;

initialization
  ApplicationProperties.ApplicationName := 'localization';

  { initialise Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialise Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;

end.
