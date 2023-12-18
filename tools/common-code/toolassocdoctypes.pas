{
  Copyright 2014-2022 Michalis Kamburelis and Jan Adamec.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Associated documents types (file extensions) requested by the project. }
unit ToolAssocDocTypes;

interface

uses SysUtils, Generics.Collections, DOM, Classes,
  CastleUtils, CastleStringUtils;

type
  TAssocDocType = class
  private
    FCaption, FName, FIcon: String;
    Extensions, MimeTypes: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function ToPListBundleDocumentTypesSection(const ProjectQualifiedName, DefaultIcon: string): string;
    function ToPListExportedTypeDeclarationsSection(const ProjectQualifiedName: string): string;
    function ToIntentFilterPathPattern: string;
  end;

  TAssociatedDocTypeList = class({$ifdef FPC}specialize{$endif} TObjectList<TAssocDocType>)
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function ToPListSection(const ProjectQualifiedName, DefaultIcon: string): string;
    function ToIntentFilter: string;
  end;

implementation

uses XMLRead, XMLWrite,
  CastleXmlUtils, CastleUriUtils, CastleFilesUtils;

{ TAssocDocType ------------------------------------------------------ }

constructor TAssocDocType.Create;
begin
  inherited;
  Extensions := TStringList.Create;
  MimeTypes := TStringList.Create;
end;

destructor TAssocDocType.Destroy;
begin
  FreeAndNil(Extensions);
  FreeAndNil(MimeTypes);
  inherited;
end;

procedure TAssocDocType.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  S: String;
begin
  FName := Element.AttributeString('name');
  if FName = '' then
    raise Exception.Create('<document_type> requires non-empty "name" attribute');
  if CharsPos(AllChars - ['a'..'z', 'A'..'Z', '0'..'9'], FName) <> 0 then
    raise Exception.CreateFmt('<document_type> "name" attribute must contain only alphanumeric characters, for maximum portability: "%s"', [
      FName
    ]);

  FCaption := Element.AttributeString('caption');
  if FCaption = '' then
    raise Exception.Create('<document_type> requires non-empty "caption" attribute');

  FIcon := Element.AttributeStringDef('icon', '');

  Extensions.Clear;
  ChildElements := Element.ChildrenIterator('extension');
  try
    while ChildElements.GetNext do
    begin
      S := Trim(ChildElements.Current.TextData);
      if SCharIs(S, 1, '.') then
        raise Exception.CreateFmt('Extensions in <document_type> must not start with a dot: "%s"', [S]);
      Extensions.Add(S);
    end;
  finally FreeAndNil(ChildElements) end;
  if Extensions.Count = 0 then
    raise Exception.Create('<document_type> requires at least one <extension>');

  MimeTypes.Clear;
  ChildElements := Element.ChildrenIterator('mime');
  try
    while ChildElements.GetNext do
    begin
      S := Trim(ChildElements.Current.TextData);
      MimeTypes.Add(S);
    end;
  finally FreeAndNil(ChildElements) end;
  if MimeTypes.Count = 0 then
    raise Exception.Create('<document_type> requires at least one <mime>');
end;

function TAssocDocType.ToPListBundleDocumentTypesSection(const ProjectQualifiedName, DefaultIcon: string): string;
var
  FinalIcon: string;
begin
  if FIcon <> '' then
    FinalIcon := FIcon
  else
    FinalIcon := DefaultIcon;

  Result :=
    #9#9'<dict>' + NL +
    #9#9#9'<key>CFBundleTypeIconFiles</key>' + NL +
    #9#9#9'<array>' + NL +
    #9#9#9#9'<string>' + FinalIcon + '</string>' + NL +
    #9#9#9'</array>' + NL +
    #9#9#9'<key>CFBundleTypeName</key>' + NL +
    #9#9#9'<string>' + FCaption + '</string>' + NL +
    #9#9#9'<key>CFBundleTypeRole</key>' + NL +
    #9#9#9'<string>Viewer</string>' + NL +
    #9#9#9'<key>LSHandlerRank</key>' + NL +
    #9#9#9'<string>Owner</string>' + NL +
    #9#9#9'<key>LSItemContentTypes</key>' + NL +
    #9#9#9'<array>' + NL +
    #9#9#9#9'<string>' + ProjectQualifiedName + '.' + FName + '</string>' + NL +
    #9#9#9'</array>' + NL +
    #9#9'</dict>' + NL;
end;

function TAssocDocType.ToPListExportedTypeDeclarationsSection(const ProjectQualifiedName: string): string;
var
  S: String;
begin
  Result :=
    #9#9'<dict>' + NL +
    #9#9#9'<key>UTTypeConformsTo</key>' + NL +
    #9#9#9'<array>' + NL +
    #9#9#9#9'<string>public.data</string>' + NL +
    #9#9#9'</array>' + NL +
    #9#9#9'<key>UTTypeDescription</key>' + NL +
    #9#9#9'<string>' + FCaption + '</string>' + NL +
    #9#9#9'<key>UTTypeIdentifier</key>' + NL +
    #9#9#9'<string>' + ProjectQualifiedName + '.' + FName + '</string>' + NL +
    #9#9#9'<key>UTTypeTagSpecification</key>' + NL +
    #9#9#9'<dict>' + NL +
    #9#9#9#9'<key>public.filename-extension</key>' + NL +
    #9#9#9#9'<array>' + NL;
  for S in Extensions do
    Result := Result +
    #9#9#9#9#9'<string>' + S + '</string>' + NL;
  Result := Result +
    #9#9#9#9'</array>' + NL +
    #9#9#9#9'<key>public.mime-type</key>' + NL +
    #9#9#9#9'<array>' + NL;
  for S in MimeTypes do
    Result := Result +
    #9#9#9#9#9'<string>' + S + '</string>' + NL;
  Result := Result +
    #9#9#9#9'</array>' + NL +
    #9#9#9'</dict>' + NL +
    #9#9'</dict>' + NL;
end;

function TAssocDocType.ToIntentFilterPathPattern: string;
var
  Ext: String;
begin
  Result := '';
  for Ext in Extensions do
  begin
    Result := Result +
      #9#9#9#9'<data android:pathPattern=".*\\.' + Ext + '" />' + NL +
      #9#9#9#9'<data android:pathPattern=".*\\..*\\.' + Ext + '" />' + NL +    // to match dot in filename
      #9#9#9#9'<data android:pathPattern=".*\\..*\\..*\\.' + Ext + '" />' + NL;
  end;
end;

{ TAssociatedDocTypeList ------------------------------------------------------ }

procedure TAssociatedDocTypeList.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  DocType: TAssocDocType;
begin
  ChildElements := Element.ChildrenIterator('document_type');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      DocType := TAssocDocType.Create;
      Add(DocType);
      DocType.ReadCastleEngineManifest(ChildElement);
    end;
  finally FreeAndNil(ChildElements) end;
end;

function TAssociatedDocTypeList.ToPListSection(const ProjectQualifiedName, DefaultIcon: string): string;
var
  BundleDocumentTypes, ExportedTypeDeclarations: string;
  DocType: TAssocDocType;
begin
  BundleDocumentTypes := '';
  ExportedTypeDeclarations := '';
  for DocType in Self do
  begin
    BundleDocumentTypes := BundleDocumentTypes +
      DocType.ToPListBundleDocumentTypesSection(ProjectQualifiedName, DefaultIcon);
    ExportedTypeDeclarations := ExportedTypeDeclarations +
      DocType.ToPListExportedTypeDeclarationsSection(ProjectQualifiedName);
  end;

  Result :=
    #9'<key>CFBundleDocumentTypes</key>' + NL +
    #9'<array>' + NL +
    #9#9'<dict>' + NL +
    #9#9#9'<key>CFBundleTypeRole</key>' + NL +
    #9#9#9'<string>Viewer</string>' + NL +
    #9#9#9'<key>CFBundleTypeExtensions</key>' + NL +
    #9#9#9'<array>' + NL +
    #9#9#9#9'<string>*</string>' + NL +
    #9#9#9'</array>' + NL +
    #9#9#9'<key>CFBundleTypeOSTypes</key>' + NL +
    #9#9#9'<array>' + NL +
    #9#9#9#9'<string>fold</string>' + NL +
    #9#9#9#9'<string>disk</string>' + NL +
    #9#9#9#9'<string>****</string>' + NL +
    #9#9#9'</array>' + NL +
    #9#9'</dict>' + NL +
    BundleDocumentTypes +
    #9'</array>' + NL;

  if ExportedTypeDeclarations <> '' then
    Result := Result +
    #9'<key>UTExportedTypeDeclarations</key>' + NL +
    #9'<array>' + NL +
    ExportedTypeDeclarations +
    #9'</array>';
end;

// https://stackoverflow.com/questions/3760276/android-intent-filter-associate-app-with-file-extension
function TAssociatedDocTypeList.ToIntentFilter: string;
var
  PathPatterns: string;
  DocType: TAssocDocType;
begin
  if Count = 0 then
    Exit('');

  PathPatterns := '';
  for DocType in Self do
  begin
    PathPatterns := PathPatterns + DocType.ToIntentFilterPathPattern;
  end;

  Result :=
    #9#9#9'<intent-filter>' + NL +
    #9#9#9#9'<action android:name="android.intent.action.VIEW" />' + NL +
    #9#9#9#9'<category android:name="android.intent.category.DEFAULT" />' + NL +
    #9#9#9#9'<category android:name="android.intent.category.BROWSABLE" />' + NL +
    #9#9#9#9'<data android:scheme="content" />' + NL +
    #9#9#9#9'<data android:scheme="file" />' + NL +
    #9#9#9#9'<data android:mimeType="*/*" />' + NL +
    PathPatterns +
    #9#9#9'</intent-filter>' + NL +
    #9#9#9'<intent-filter>' + NL +
    #9#9#9#9'<action android:name="android.intent.action.VIEW" />' + NL +
    #9#9#9#9'<category android:name="android.intent.category.DEFAULT" />' + NL +
    #9#9#9#9'<category android:name="android.intent.category.BROWSABLE" />' + NL +
    #9#9#9#9'<data android:scheme="http" />' + NL +
    #9#9#9#9'<data android:scheme="https" />' + NL +
    #9#9#9#9'<data android:scheme="ftp" />' + NL +
    #9#9#9#9'<data android:host="*" />' + NL +
    PathPatterns +
    #9#9#9'</intent-filter>';
end;

end.
