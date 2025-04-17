{
  Copyright 2014-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Services (on Android and iOS) definitions (parameters) in CastleEngineManifest. }
unit ToolServices;

interface

uses SysUtils, Generics.Collections, DOM,
  CastleUtils, CastleStringUtils;

type
  TService = class
  private
    FParameters: TStringStringMap;
    FName: string;
  public
    constructor Create(const Name: string = '');
    destructor Destroy; override;
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    property Name: string read FName;
    property Parameters: TStringStringMap read FParameters;
  end;

  TServiceList = class({$ifdef FPC}specialize{$endif} TObjectList<TService>)
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function HasService(const Name: string): boolean;
    procedure AddService(const Name: string);
    { Find service by name.
      Returns @nil if not found.
      You can check HasService earlier, if HasService('xxx') = @true then Service('xxx')
      will never be @nil. }
    function Service(const Name: string): TService;
  end;

{ Find in Element all children called <parameter>,
  read them and add to Parameters list,
  expecting this format:

  <parameter key="my_key" value="my_value" />
  or
  <parameter key="my_key">my_value</parameter>
  or
  <parameter key="my_key"><![CDATA[my_value]]></parameter>

  Note that keys are converted to lowercase.
  Parameter keys, just like macro names, are not case-sensitive.

  All the keys in RequiredKeys are guaranteed to have a value set.
  If they are not specified in Element (or Element is @nil),
  they will have an empty value.
  This is necessary if you want to use this macro in template *even*
  when user doesn't specify it.
}
procedure ReadParameters(const Element: TDOMElement; const Parameters: TStringStringMap;
  const RequiredKeys: array of String);

implementation

uses Classes, XMLRead, XMLWrite,
  CastleXmlUtils, CastleUriUtils, CastleFilesUtils;

{ internal utils ------------------------------------------------------------- }

function GetCData(const Element: TDOMElement): String;
var
  I: TXMLCDataIterator;
begin
  Result := '';
  I := TXMLCDataIterator.Create(Element);
  try
    while I.GetNext do
      Result := Result + I.Current;
  finally FreeAndNil(I) end;
end;

procedure ReadParameters(const Element: TDOMElement; const Parameters: TStringStringMap;
  const RequiredKeys: array of String);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  Key, Value, KeyLower: string;
begin
  if Element <> nil then
  begin
    ChildElements := Element.ChildrenIterator('parameter');
    try
      while ChildElements.GetNext do
      begin
        ChildElement := ChildElements.Current;

        Key := LowerCase(ChildElement.AttributeString('key'));
        if Key = '' then
          raise Exception.Create('Key for <parameter> is empty in CastleEngineManifest.xml');

        if ChildElement.HasAttribute('value') then
          Value := ChildElement.AttributeString('value')
        else
        begin
          Value := ChildElement.TextData;
          if Value = '' then
            Value := GetCData(ChildElement);
          { value cannot be empty in this case }
          if Value = '' then
            raise Exception.CreateFmt('No value for key "%s" specified in CastleEngineManifest.xml', [Key]);
        end;

        Parameters.Add(Key, Value);
      end;
    finally FreeAndNil(ChildElements) end;
  end;

  for Key in RequiredKeys do
  begin
    KeyLower := LowerCase(Key);
    if not Parameters.ContainsKey(KeyLower) then
      Parameters.Add(KeyLower, '');
  end;
end;

{ TService ------------------------------------------------------------------- }

constructor TService.Create(const Name: string);
begin
  inherited Create;
  FParameters := TStringStringMap.Create;
  FName := Name;
end;

destructor TService.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

procedure TService.ReadCastleEngineManifest(const Element: TDOMElement);
begin
  FName := Element.AttributeString('name');
  ReadParameters(Element, Parameters, []);
end;

{ TServiceList ------------------------------------------------------ }

procedure TServiceList.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  NewService: TService;
begin
  ChildElements := Element.ChildrenIterator('component'); // parse deprecated name 'component'
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      NewService := TService.Create;
      Add(NewService);
      NewService.ReadCastleEngineManifest(ChildElement);
    end;
  finally FreeAndNil(ChildElements) end;

  ChildElements := Element.ChildrenIterator('service');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      NewService := TService.Create;
      Add(NewService);
      NewService.ReadCastleEngineManifest(ChildElement);
    end;
  finally FreeAndNil(ChildElements) end;
end;

function TServiceList.HasService(const Name: string): boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(true);
  Result := false;
end;

function TServiceList.Service(const Name: string): TService;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);
  Result := nil;
end;

procedure TServiceList.AddService(const Name: string);
var
  NewService: TService;
begin
  if not HasService(Name) then
  begin
    NewService := TService.Create(Name);
    Add(NewService);
  end;
end;

end.
