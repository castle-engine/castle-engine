{
  Copyright 2014-2018 Michalis Kamburelis.

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
    constructor Create(const Name: string = '');
    destructor Destroy; override;
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    property Name: string read FName;
    property Parameters: TStringStringMap read FParameters;
  end;

  TServiceList = class(specialize TObjectList<TService>)
  public
    procedure ReadCastleEngineManifest(const Element: TDOMElement);
    function HasService(const Name: string): boolean;
    procedure AddService(const Name: string);
  end;

implementation

uses Classes, XMLRead, XMLWrite,
  CastleXMLUtils, CastleURIUtils, CastleFilesUtils;

{ TService ---------------------------------------------------------- }

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

procedure TServiceList.AddService(const Name: string);
var
  Service: TService;
begin
  if not HasService(Name) then
  begin
    Service := TService.Create(Name);
    Add(Service);
  end;
end;

end.
