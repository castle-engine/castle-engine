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

  { Stores information about available VRML/X3D field classes.
    The only use for now is to make a mapping from VRML/X3D field name to
    actual class (needed by VRML/X3D interface declarations). }
  TX3DFieldsManager = class
  strict private
    Registered: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterClass(AClass: TX3DFieldClass);
    procedure RegisterClasses(const Classes: array of TX3DFieldClass);

    { Return field class for given name. Returns @nil if not found. }
    function X3DTypeToClass(const X3DType: string): TX3DFieldClass;
  end;

function X3DFieldsManager: TX3DFieldsManager;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DFieldsManager --------------------------------------------------------- }

constructor TX3DFieldsManager.Create;
begin
  inherited;
  Registered := TStringList.Create;
  { All VRML/X3D names are case-sensitive. }
  Registered.CaseSensitive := true;

  RegisterClasses([
    TSFBitMask,
    TSFEnum,
    TSFBool,     TMFBool,
    TSFFloat,    TMFFloat,
    TSFImage,
    TSFLong,     TMFLong,
    TSFInt32,    TMFInt32,

    TSFMatrix3f, TMFMatrix3f,
    TSFMatrix,
    TSFMatrix3d, TMFMatrix3d,
    TSFMatrix4f, TMFMatrix4f,
    TSFMatrix4d, TMFMatrix4d,

    TSFRotation, TMFRotation,
    TSFString,   TMFString,
    TSFDouble,   TMFDouble,
    TSFTime,     TMFTime,
    TSFVec2f,    TMFVec2f,
    TSFVec3f,    TMFVec3f,
    TSFColor,    TMFColor,
    TSFVec4f,    TMFVec4f,
    TSFVec2d,    TMFVec2d,
    TSFVec3d,    TMFVec3d,
    TSFVec4d,    TMFVec4d,
    TSFColorRGBA,TMFColorRGBA
  ]);
end;

destructor TX3DFieldsManager.Destroy;
begin
  FreeAndNil(Registered);
  inherited;
end;

procedure TX3DFieldsManager.RegisterClass(AClass: TX3DFieldClass);
begin
  Registered.AddObject(AClass.X3DType, TObject(AClass));
end;

procedure TX3DFieldsManager.RegisterClasses(
  const Classes: array of TX3DFieldClass);
var
  I: Integer;
begin
  for I := 0 to High(Classes) do
    RegisterClass(Classes[I]);
end;

function TX3DFieldsManager.X3DTypeToClass(
  const X3DType: string): TX3DFieldClass;
var
  I: Integer;
begin
  I := Registered.IndexOf(X3DType);
  if I <> -1 then
    Result := TX3DFieldClass(Registered.Objects[I]) else
    Result := nil;
end;

var
  FX3DFieldsManager: TX3DFieldsManager;

function X3DFieldsManager: TX3DFieldsManager;
{ This function automatically creates FX3DFieldsManager instance.
  I don't do this in initialization of this unit, since (because
  of circular uses clauses) X3DFieldsManager may be referenced
  before our initialization (e.g. by initialization of X3DNodes). }
begin
  if FX3DFieldsManager = nil then
    FX3DFieldsManager := TX3DFieldsManager.Create;
  Result := FX3DFieldsManager;
end;

{$endif read_implementation}
