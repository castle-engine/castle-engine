{
  Copyright 2007-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Material and texture properties from external files (TMaterialProperty,
  global MaterialProperties collection). }
unit CastleMaterialProperties deprecated 'use CastleTextureImage unit for TextureLoadingScale, everything else from this unit was internal';

interface

{$ifdef FPC}
function GetTextureLoadingScale: Cardinal;
procedure SetTextureLoadingScale(const Value: Cardinal);
property TextureLoadingScale: Cardinal read GetTextureLoadingScale write SetTextureLoadingScale;
{$endif}

type
  TDummyMaterialProperties = class
  strict private
    FUrl: String;
    procedure SetUrl(const Value: String);
  public
    property Url: String read FUrl write SetUrl;
  end;

function MaterialProperties: TDummyMaterialProperties;

implementation

uses SysUtils, CastleLog, CastleTextureImages;

function GetTextureLoadingScale: Cardinal;
begin
  Result := CastleTextureImages.TextureLoadingScale;
end;

procedure SetTextureLoadingScale(const Value: Cardinal);
begin
  CastleTextureImages.TextureLoadingScale := Value;
end;

procedure TDummyMaterialProperties.SetUrl(const Value: String);
begin
  if FUrl <> Value then
  begin
    FUrl := Value;
    WritelnWarning('Do not use "MaterialProperties.Url := ..." anymore. Instead place your data in "castle-data:/material_properties.xml" and it will be read automatically.');
  end;
end;

var
  FMaterialProperties: TDummyMaterialProperties;

function MaterialProperties: TDummyMaterialProperties;
begin
  if FMaterialProperties = nil then
    FMaterialProperties := TDummyMaterialProperties.Create;
  Result := FMaterialProperties;
end;

initialization
finalization
  FreeAndNil(FMaterialProperties);
end.
