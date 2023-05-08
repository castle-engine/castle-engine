{%MainUnit x3dfields.pas}
{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Reading of VRML/X3D from stream.
    Common knowledge for both classic and XML reader.
    X3DNodes unit extends this into TX3DReaderNames. }
  TX3DReader = class
  strict private
    FVersion: TX3DVersion;
    FBaseUrl: string;
  private
    AngleConversionFactor: Float;
  public
    LengthConversionFactor: Float;

    constructor Create(const ABaseUrl: string;
      const AVersion: TX3DVersion);
    constructor CreateCopy(Source: TX3DReader);

    { Base path for resolving URLs from nodes in this namespace.
      See TX3DNode.BaseUrl. }
    property BaseUrl: string read FBaseUrl;

    { VRML/X3D version number. For resolving node class names and other stuff. }
    property Version: TX3DVersion read FVersion;

    { Apply unit conversion.
      If this is angle conversion factor, it is stored and used internally.
      If this is length conversion factor, we update our
      LengthConversionFactor property, but it's callers responsibility
      to make use of it. (You want to use here TX3DRootNode.Scale.) }
    procedure UnitConversion(const Category, Name: string;
      const ConversionFactor: Float);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DReader ----------------------------------------------------------------- }

constructor TX3DReader.Create(
  const ABaseUrl: string; const AVersion: TX3DVersion);
begin
  inherited Create;
  FBaseUrl := ABaseUrl;
  FVersion := AVersion;
  AngleConversionFactor := 1;
  LengthConversionFactor := 1;
end;

constructor TX3DReader.CreateCopy(Source: TX3DReader);
begin
  inherited Create;
  FBaseUrl := Source.BaseUrl;
  FVersion := Source.Version;
  AngleConversionFactor := Source.AngleConversionFactor;
  LengthConversionFactor := Source.LengthConversionFactor;
end;

procedure TX3DReader.UnitConversion(const Category, Name: string;
  const ConversionFactor: Float);
begin
  if (Version.Major < 3) or
     ( (Version.Major = 3) and
       (Version.Minor < 3) ) then
    WritelnWarning('X3D', 'UNIT declaration found, but X3D version is < 3.3');

  { store UNIT inside Reader }
  if Category = 'angle' then
    AngleConversionFactor := ConversionFactor else
  if Category = 'force' then
    { TODO } else
  if Category = 'length' then
    LengthConversionFactor := ConversionFactor else
  if Category = 'mass' then
    { TODO } else
    WritelnWarning('X3D', FormatDot('UNIT category "%s" unknown. Only the categories listed in X3D specification as base units are allowed',
      [Category]));
end;

{$endif read_implementation}
