// -*- compile-command: "castle-engine compile && castle-engine run" -*-
{
  Copyright 2018-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generate Pascal code to instantiate persistent vectors/colors classes.
  See README.md for detailed description. }

uses SysUtils, Generics.Collections, StrUtils,
  CastleUtils, CastleStringUtils, CastleFilesUtils, CastleDownload;

type
  TAutoGenerateProperty = class
    PropertyName, PropertyType: String;
    NoStore, Rotation: Boolean;
    StoreCheckFunction: String;
    function PersistentPropertyClass: String;
    procedure ProcessToken(const Token: String);
  end;

  TAutoGeneratePropertyList = class(specialize TObjectList<TAutoGenerateProperty>)
  end;

  TAutoGenerateClass = class
    Properties: TAutoGeneratePropertyList;
    Path, OutputClassName: String;
    constructor Create;
    destructor Destroy; override;
    procedure GenerateCode;
  end;

  TAutoGenerateClasses = class(specialize TObjectDictionary<String, TAutoGenerateClass>)
    procedure Add(const Path, OutputClassName: String;
      const Prop: TAutoGenerateProperty);
    procedure GenerateAll;
  end;

{ TAutoGenerateProperty ------------------------------------------------------ }

function TAutoGenerateProperty.PersistentPropertyClass: String;
begin
  Result := StringReplace(PropertyType, 'TVector', 'TCastleVector', [rfReplaceAll]);
  if Rotation then
    Result += 'RotationPersistent'
  else
    Result += 'Persistent';
end;

procedure TAutoGenerateProperty.ProcessToken(const Token: String);
begin
  if Token = 'no-store' then
    NoStore := true
  else
  if IsPrefix('store=', Token, false) then
    StoreCheckFunction := PrefixRemove('store=', Token, false)
  else
  if Token = 'rotation' then
    Rotation := true
  else
    raise Exception.CreateFmt('Invalid token "%s" at property "%s"', [Token, PropertyName]);
end;

{ TAutoGenerateClass --------------------------------------------------------- }

constructor TAutoGenerateClass.Create;
begin
  inherited;
  Properties := TAutoGeneratePropertyList.Create(true);
end;

destructor TAutoGenerateClass.Destroy;
begin
  FreeAndNil(Properties);
  inherited;
end;

procedure TAutoGenerateClass.GenerateCode;
var
  InputTemplate, OutputPath, OutputFileName, OutputCode: String;
  Macros: TStringStringMap;
  P: TAutoGenerateProperty;
begin
  OutputPath := Path + 'auto_generated_persistent_vectors' + PathDelim;
  ForceDirectories(OutputPath);
  OutputFileName := OutputPath +
    LowerCase(OutputClassName) + '_persistent_vectors.inc';

  InputTemplate := FileToString('persistent_vectors_template.inc');
  OutputCode := '';

  for P in Properties do
  begin
    Macros := TStringStringMap.Create;
    try
      Macros.Add('${CLASS_NAME}', OutputClassName);
      Macros.Add('${PROPERTY_NAME}', P.PropertyName);
      Macros.Add('${PROPERTY_TYPE}', P.PropertyType);
      if P.StoreCheckFunction <> '' then
        Macros.Add('${PROPERTY_STORED}', 'stored ' + P.StoreCheckFunction)
      else
        Macros.Add('${PROPERTY_STORED}', IfThen(P.NoStore, 'stored false', ''));
      Macros.Add('${PERSISTENT_PROPERTY_CLASS}', P.PersistentPropertyClass);
      OutputCode := OutputCode + SReplacePatterns(InputTemplate, Macros, false) + NL;
    finally FreeAndNil(Macros) end;
  end;

  StringToFile(OutputFileName, OutputCode);
  Writeln('Generated ', OutputFileName, ' with ', Properties.Count, ' persistent vectors/colors properties');
end;

{ TAutoGenerateClasses ------------------------------------------------------- }

procedure TAutoGenerateClasses.Add(
  const Path, OutputClassName: String;
  const Prop: TAutoGenerateProperty);
var
  C: TAutoGenerateClass;
begin
  if not TryGetValue(Path + OutputClassName, C) then
  begin
    C := TAutoGenerateClass.Create;
    C.Path := Path;
    C.OutputClassName := OutputClassName;
    inherited Add(Path + OutputClassName, C);
  end;

  C.Properties.Add(Prop);
end;

procedure TAutoGenerateClasses.GenerateAll;
var
  C: TAutoGenerateClass;
begin
  for C in Values do
    C.GenerateCode;
end;

{ main program --------------------------------------------------------------- }

var
  AutoGenerateClasses: TAutoGenerateClasses;
  InputFile: TTextReader;
  Line, Path, OutputClassName: String;
  LineSplit: TCastleStringList;
  Prop: TAutoGenerateProperty;
  I: Integer;
begin
  AutoGenerateClasses := TAutoGenerateClasses.Create([doOwnsValues]);
  try
    InputFile := TTextReader.Create('persistent_vectors_input.txt');
    try
      while not InputFile.Eof do
      begin
        Line := InputFile.Readln;

        if Trim(Line) = '' then Continue; // skip empty lines

        LineSplit := CreateTokens(Line);
        try
          if LineSplit.Count < 4 then
            raise Exception.CreateFmt('Expected at least 4 tokens at line "%s"', [Line]);

          Path := InclPathDelim(LineSplit[0]);
          OutputClassName := LineSplit[1];

          Prop := TAutoGenerateProperty.Create;
          Prop.PropertyName := LineSplit[2];
          Prop.PropertyType := LineSplit[3];
          for I := 4 to LineSplit.Count - 1 do
            Prop.ProcessToken(LineSplit[I]);

          AutoGenerateClasses.Add(Path, OutputClassName, Prop);
        finally FreeAndNil(LineSplit) end;
      end;
    finally FreeAndNil(InputFile) end;

    AutoGenerateClasses.GenerateAll;
  finally FreeAndNil(AutoGenerateClasses) end;
end.
