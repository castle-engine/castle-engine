{
  Copyright 2017-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main view, where most of the application logic takes place.

  Note: This file must contain UTF-8 BOM, otherwise Delphi will not interpret
  correctly the chars in "String hardcoded in Pascal sources:" below. }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleUIControls, CastleControls, CastleKeysMouse,
  CastleScene;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonLoadFromFiles: TCastleButton;
    LabelInternational, LabelChinese: TCastleLabel;
    TextInternational, TextChinese: TCastleText;
  private
    procedure ClickLoadFromFiles(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses DOM, SysUtils,
  CastleUtils, CastleConfig, CastleXmlUtils, CastleDownload;

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
end;

procedure TViewMain.Start;
begin
  inherited;
  ButtonLoadFromFiles.OnClick := {$ifdef FPC}@{$endif} ClickLoadFromFiles;
end;

procedure TViewMain.ClickLoadFromFiles(Sender: TObject);

  function TestText: String;
  var
    Config: TCastleConfig;
    Doc: TXmlDocument;
    TextReader: TTextReader;
    StringList: TStringList;
  begin
    Result :=
      'String hardcoded in Pascal sources:' + NL +
      'Chinese: 你好世界' + NL +
      'Greek: Γεια σου κόσμε!' + NL +
      'Russian: Здравствуй, мир!' + NL +
      'Polish: Witaj świecie! Oraz: źrebię ćma koń wężyk dąb!' + NL +
      NL;

    Config := TCastleConfig.Create(nil);
    try
      Config.Load('castle-data:/example_config.xml');
      Result := Result +
        'String from TCastleConfig:' + NL +
        Config.GetStringNonEmpty('value') + NL +
        NL;
    finally FreeAndNil(Config) end;

    Doc := URLReadXML('castle-data:/example_strings.xml');
    try
      Result := Result +
        'Strings from XML:' + NL +
        Doc.DocumentElement.ChildElement('element').TextData + NL +
        Doc.DocumentElement.ChildElement('element_with_multiline_content').TextData + NL +
        Doc.DocumentElement.ChildElement('element_with_attributes').AttributeString('value') + NL +
        Doc.DocumentElement.ChildElement('element_with_attributes').AttributeString('value_with_multiline_content') + NL +
        NL;
    finally FreeAndNil(Doc) end;

    TextReader := TTextReader.Create('castle-data:/example_text.txt');
    try
      Result := Result +
        'Strings from txt (TTextReader):' + NL +
        TextReader.Readln + NL +
        TextReader.Readln + NL +
        TextReader.Readln + NL +
        TextReader.Readln + NL +
        NL;
    finally FreeAndNil(TextReader) end;

    StringList := TStringList.Create;
    try
      StringList.LoadFromURL('castle-data:/example_text.txt', TEncoding.UTF8);
      Result := Result +
        'Strings from txt (TStringList):' + NL +
        StringList.Text + NL +
        NL;
    finally FreeAndNil(StringList) end;
  end;

var
  S: String;
begin
  S := TestText;
  LabelInternational.Caption := S;
  LabelChinese.Caption := S;
  TextInternational.Caption := S;
  TextChinese.Caption := S;
end;

end.
