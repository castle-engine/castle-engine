{%MainUnit castleinternalloadsaveifc.pas}
{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Types and classes related to the IFC, but not from IFC standard. }

{$ifdef read_interface}

type
  TIfcProject = class;

  {$M+}
  { Abstract class that can serialized to / deserialized from an IFC file.
    This is not part of the official IFC standard, it added for CGE purposes. }
  TIfcPersistent = class abstract
  public
    constructor Create; virtual;
  end;
  {$M-}

  TIfcPersistentList = {$ifdef FPC}specialize{$endif} TObjectList<TIfcPersistent>;

  TIfcPersistentClass = class of TIfcPersistent;

  { List of TIfcPersistent descendant classes. }
  TIfcPersistentClassList = class({$ifdef FPC}specialize{$endif} TList<TIfcPersistentClass>)
  public
    { Find class corresponding to the given IFC type.
      Returns @nil if not found. }
    function FindIfcType(const IfcType: String): TIfcPersistentClass;
  end;

  { IFC file representation.

    This class is not defined in the IFC standard, but it is consistent
    with how the IFC file is actually represented, in both STEP and JSON encodings.
    The file starts with some metadata (in STEP header, on JSON top-level object)
    and then follows the "data" which is TIfcProject instance. }
  TIfcFile = class(TIfcPersistent)
  private
    FEncodingType: String;
    FVersion: String;
    FSchemaIdentifier: String;
    FOriginatingSystem: String;
    FPreprocessorVersion: String;
    FTimeStamp: String;
    FData: TIfcPersistentList;
  public
    constructor Create; override;
    destructor Destroy; override;
    { Return one and only TIfcProject instance from Data.
      @raises(EInvalidIfc If there is no TIfcProject instance in Data
        or there's more than one TIfcProject instance in Data.) }
    function Project: TIfcProject;
  published
    property EncodingType: String read FEncodingType write FEncodingType;
    property Version: String read FVersion write FVersion;
    property SchemaIdentifier: String read FSchemaIdentifier write FSchemaIdentifier;
    property OriginatingSystem: String read FOriginatingSystem write FOriginatingSystem;
    property PreprocessorVersion: String read FPreprocessorVersion write FPreprocessorVersion;
    // TODO: If ever necessary, convert this to TDateTime
    property TimeStamp: String read FTimeStamp write FTimeStamp;
    property Data: TIfcPersistentList read FData write FData;
  end;

  { Invalid IFC file exception.
    Not all IFC extensions descend from this, e.g. we may also generate EJson
    exception or stream-reading exception. }
  EInvalidIfc = class(Exception);

{$endif read_interface}

{$ifdef read_implementation}

{ TIfcPersistent ------------------------------------------------------------- }

constructor TIfcPersistent.Create;
begin
  inherited;
  { Implemented only to not be abstract, to allow all descendants,
    direct or not, to just call "inherited". }
end;

{ TIfcPersistentList --------------------------------------------------------- }

function TIfcPersistentClassList.FindIfcType(const IfcType: String): TIfcPersistentClass;
var
  I: Integer;
  FindClassName: String;
begin
  FindClassName := 'T' + IfcType;
  for I := 0 to Count - 1 do
    if Items[I].ClassName = FindClassName then
    begin
      Result := Items[I];
      Exit;
    end;
  Result := nil;
end;

{ TIfcFile ------------------------------------------------------------------- }

constructor TIfcFile.Create;
begin
  inherited;
  FData := TIfcPersistentList.Create(true);
end;

destructor TIfcFile.Destroy;
begin
  FreeAndNil(FData);
  inherited;
end;

function TIfcFile.Project: TIfcProject;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to FData.Count - 1 do
  begin
    if FData[I] is TIfcProject then
    begin
      if Result <> nil then
        raise EInvalidIfc.Create('IFC file contains more than one TIfcProject');
      Result := TIfcProject(FData[I]);
    end;
  end;
  if Result = nil then
    raise EInvalidIfc.Create('IFC file does not contain any TIfcProject');
end;

{$endif read_implementation}
