{
  Copyright 2014-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Android or iOS services and their configurations. }
unit ToolServices;

interface

uses SysUtils, Generics.Collections, DOM,
  CastleUtils, CastleStringUtils,
  ToolUtils;

type
  TService = class
  private
    FParameters: TStringStringMap;
    FName: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    property Name: string read FName;
    property Parameters: TStringStringMap read FParameters;
  end;

  TServiceList = class(specialize TObjectList<TService>)
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function HasService(const Name: string): boolean;
  end;

implementation

uses Classes, XMLRead, XMLWrite,
  CastleXMLUtils, CastleURIUtils, CastleFilesUtils;

{ TService ---------------------------------------------------------- }

constructor TService.Create;
begin
  inherited;
  FParameters := TStringStringMap.Create;
end;

destructor TService.Destroy;
begin
  FreeAndNil(FParameters);
  inherited;
end;

procedure TService.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  Key, Value: string;
begin
  FName := Element.AttributeString('name');

  ChildElements := Element.ChildrenIterator('parameter');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      Key := ChildElement.AttributeString('key');
      if ChildElement.HasAttribute('value') then
        Value := ChildElement.AttributeString('value')
      else
      begin
        Value := ChildElement.TextData;
        { value cannot be empty in this case }
        if Value = '' then
          raise Exception.CreateFmt('No value for key "%s" specified in CastleEngineManifest.xml', [Key]);
      end;
      FParameters.Add(Key, Value);
    end;
  finally FreeAndNil(ChildElements) end;
end;

{ TServiceList ------------------------------------------------------ }

procedure TServiceList.ReadCastleEngineManifest(const Element: TDOMElement);
var
  ChildElements: TXMLElementIterator;
  ChildElement: TDOMElement;
  Service: TService;
begin
  ChildElements := Element.ChildrenIterator('component');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      Service := TService.Create;
      Add(Service);
      Service.ReadCastleEngineManifest(ChildElement);
    end;
  finally FreeAndNil(ChildElements) end;

  ChildElements := Element.ChildrenIterator('service');
  try
    while ChildElements.GetNext do
    begin
      ChildElement := ChildElements.Current;
      Service := TService.Create;
      Add(Service);
      Service.ReadCastleEngineManifest(ChildElement);
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

end.
