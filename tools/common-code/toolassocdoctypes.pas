{
  Copyright 2014-2018 Michalis Kamburelis and Jan Adamec.

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

uses SysUtils, Generics.Collections, DOM,
  CastleUtils, CastleStringUtils;

type
  TAssocDocTypeFileExt = class
  private
    FExt, FMime, FIosSystemUti: string;
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    property Ext: string read FExt;
    property Mime: string read FMime;
    property IosSystemUti: string read FIosSystemUti;
    function Uti(const ProjectQualifiedName: string): string;
  end;

  TAssocDocType = class(specialize TObjectList<TAssocDocTypeFileExt>)
  private
    FName, FIcon: string;
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function ToPListBundleDocumentTypesSection(const ProjectQualifiedName, DefaultIcon: string): string;
    function ToPListExportedTypeDeclarationsSection(const ProjectQualifiedName: string): string;
    function ToIntentFilterPathPattern: string;
  end;

  TAssociatedDocTypeList = class(specialize TObjectList<TAssocDocType>)
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function ToPListSection(const ProjectQualifiedName, DefaultIcon: string): string;
    function ToIntentFilter: string;
  end;

implementation

uses Classes, XMLRead, XMLWrite,
  CastleXMLUtils, CastleURIUtils, CastleFilesUtils;

{ TAssocDocTypeFileExt ------------------------------------------------- }

procedure TAssocDocTypeFileExt.ReadCastleEngineManifest(const Element: TDOMElement);
begin
  FExt := Element.AttributeString('extension');
  FMime := Element.AttributeStringDef('mime', '');
  FIosSystemUti := Element.AttributeStringDef('ios_type_identifier', '');
end;

function TAssocDocTypeFileExt.Uti(const ProjectQualifiedName: string): string;
begin
  if Length(FIosSystemUti) > 0 then
     Result := FIosSystemUti else
     Result := ProjectQualifiedName + '.' + FExt;
end;

{ TAssocDocType ------------------------------------------------------ }

procedure TAssocDocType.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  FileExt: TAssocDocTypeFileExt;
begin
  FName := Element.AttributeString('name');
  FIcon := Element.AttributeStringDef('icon', '');

  ChildElements := Element.ChildrenIterator('file_extension');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      FileExt := TAssocDocTypeFileExt.Create;
      Add(FileExt);
      FileExt.ReadCastleEngineManifest(ChildElement);
    end;
  finally FreeAndNil(ChildElements) end;
end;

function TAssocDocType.ToPListBundleDocumentTypesSection(const ProjectQualifiedName, DefaultIcon: string): string;
var
  I: Integer;
  FileIcon: string;
begin
  if Count = 0 then
    Exit('');
  if Length(FIcon) > 0 then
    FileIcon := FIcon else
    FileIcon := DefaultIcon;

  Result := #9#9'<dict>' + NL +
            #9#9#9'<key>CFBundleTypeIconFiles</key>' + NL +
            #9#9#9'<array>' + NL +
            #9#9#9#9'<string>' + FileIcon + '</string>' + NL +
            #9#9#9'</array>' + NL +
            #9#9#9'<key>CFBundleTypeName</key>' + NL +
            #9#9#9'<string>' + FName + '</string>' + NL +
            #9#9#9'<key>CFBundleTypeRole</key>' + NL +
            #9#9#9'<string>Viewer</string>' + NL +
            #9#9#9'<key>LSHandlerRank</key>' + NL +
            #9#9#9'<string>Owner</string>' + NL +
            #9#9#9'<key>LSItemContentTypes</key>' + NL +
            #9#9#9'<array>' + NL;
  for I := 0 to Count - 1 do
  begin
    Result := Result + #9#9#9#9'<string>' + Items[I].Uti(ProjectQualifiedName) + '</string>' + NL;
  end;
  Result := Result +
            #9#9#9'</array>' + NL +
            #9#9'</dict>' + NL;
end;

function TAssocDocType.ToPListExportedTypeDeclarationsSection(const ProjectQualifiedName: string): string;
var
  I: Integer;
begin
  Result := '';
  if Count = 0 then
    Exit;
  for I := 0 to Count - 1 do
  begin
    if (Length(Items[I].Ext) > 0) and (Length(Items[I].IosSystemUti) = 0) then
      Result := Result +
                #9#9'<dict>' + NL +
                #9#9#9'<key>UTTypeConformsTo</key>' + NL +
                #9#9#9'<array>' + NL +
                #9#9#9#9'<string>public.data</string>' + NL +
                #9#9#9'</array>' + NL +
                #9#9#9'<key>UTTypeDescription</key>' + NL +
                #9#9#9'<string>' + FName + '</string>' + NL +
                #9#9#9'<key>UTTypeIdentifier</key>' + NL +
                #9#9#9'<string>' + Items[I].Uti(ProjectQualifiedName) + '</string>' + NL +
                #9#9#9'<key>UTTypeTagSpecification</key>' + NL +
                #9#9#9'<dict>' + NL +
                #9#9#9#9'<key>public.filename-extension</key>' + NL +
                #9#9#9#9'<string>' + Items[I].Ext + '</string>' + NL +
                #9#9#9#9'<key>public.mime-type</key>' + NL +
                #9#9#9#9'<string>' + Items[I].Mime + '</string>' + NL +
                #9#9#9'</dict>' + NL +
                #9#9'</dict>' + NL;
  end;
end;

function TAssocDocType.ToIntentFilterPathPattern: string;
var
  I: Integer;
begin
  Result := '';
  if Count = 0 then
    Exit;
  for I := 0 to Count - 1 do
  begin
    if Length(Items[I].Ext) > 0 then
      Result := Result +
                #9#9#9#9'<data android:pathPattern=".*\\.' + Items[I].Ext + '" />' + NL +
                #9#9#9#9'<data android:pathPattern=".*\\..*\\.' + Items[I].Ext + '" />' + NL +    // to match dot in filename
                #9#9#9#9'<data android:pathPattern=".*\\..*\\..*\\.' + Items[I].Ext + '" />' + NL;
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
  I: Integer;
begin
  if Count = 0 then
    Exit('');
  BundleDocumentTypes := '';
  ExportedTypeDeclarations := '';
  for I := 0 to Count - 1 do
  begin
    BundleDocumentTypes := BundleDocumentTypes +
                           Items[I].ToPListBundleDocumentTypesSection(ProjectQualifiedName, DefaultIcon);
    ExportedTypeDeclarations := ExportedTypeDeclarations +
                                Items[I].ToPListExportedTypeDeclarationsSection(ProjectQualifiedName);
  end;

  Result := #9'<key>CFBundleDocumentTypes</key>' + NL +
            #9'<array>' + NL +
            BundleDocumentTypes +
            #9'</array>' + NL +
            #9'<key>UTExportedTypeDeclarations</key>' + NL +
            #9'<array>' + NL +
            ExportedTypeDeclarations +
            #9'</array>';
end;

// https://stackoverflow.com/questions/3760276/android-intent-filter-associate-app-with-file-extension
function TAssociatedDocTypeList.ToIntentFilter: string;
var
  PathPatterns: string;
  I: Integer;
begin
  if Count = 0 then
    Exit('');
  PathPatterns := '';
  for I := 0 to Count - 1 do
  begin
    PathPatterns := PathPatterns + Items[I].ToIntentFilterPathPattern;
  end;

  Result := #9#9#9'<intent-filter>' + NL +
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
