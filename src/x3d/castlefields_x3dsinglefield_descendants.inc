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

{ Single-value X3D fields. }
{$ifdef read_interface}

  { VRML 1.0 (deprecated) field representing a bitmask,
    where a number of "flags" can be "on" or "off".
    @deprecated This is deprecated, as VRML 1.0 is an ancient format now. }
  TSFBitMask = class(TX3DSingleField)
  strict private
    fAllString, fNoneString: string;
    fFlagNames: TStringList;

    { Value of this field, as a bit mask.
      VRML 1.0 specification guarantees that SFBitMask has 32 or less flags.
      Actually, defined field values have no more than 3 fields, and
      VRML > 1.0 dropped SFBitMask entirely. So 32 is always enough. }
    fFlags: set of 0..31;
    function GetFlags(i: integer): boolean;
    procedure SetFlags(i: integer; value: boolean);
    function GetFlagNames(i: integer): string;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Value of this field. You can use Index from the range 0 .. FlagsCount - 1. }
    property Flags[i: integer]:boolean read GetFlags write SetFlags;
    function FlagsCount: integer;

    { Names of the flags that can be "on" or "off".
      Note that this cannot be initialized by CreateUndefined,
      so you always must initialize this field using the @link(Create)
      overload that takes AFlagNames parameter. }
    property FlagNames[i: integer]:string read GetFlagNames;

    { Special strings that will be understood by parser as ALL or NONE
      bit values. AllString selects all flags, NoneString selects none.
      AllString may be '' is there's no such string, NoneString
      should never be '' (otherwise, user could not be able to specify
      some SFBitMask values --- NoneString is the only way to specify 0).

      There is usually little sense in using them like "ALL | something"
      (because it means just "ALL") or "NONE | something" (because it means
      just "something"). But it's allowed syntactically.

      @groupBegin }
    property AllString: string read fAllString;
    property NoneString: string read fNoneString;
    { @groupEnd }

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Are all flag values set to @true currently. }
    function AreAllFlags(value: boolean): boolean;

    { Constructor.

      Remember that arrays AFlagNames and AFlags
      (AFlags is initial value of Flags) must have equal length.
      Eventually, AFlags may be longer (excessive items will be ignored). }
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AFlagNames: array of string;
      const ANoneString, AAllString: string; const AFlags: array of boolean);

    destructor Destroy; override;

    function Equals(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;

    class function X3DType: string; override;
  end;

  { X3D field containing a single boolean value. }
  TSFBool = class(TX3DSingleField)
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: boolean;
    DefaultValue: boolean;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: boolean);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: boolean; const PartialSend: TPartialSend = nil); overload;
  end;

  { VRML 1.0 (deprecated) field representing an enumerated value,
    which means that you choose one (named) value from a set of possible values.
    @deprecated This is deprecated, as VRML 1.0 is an ancient format now. }
  TSFEnum = class(TX3DSingleField)
  strict private
    FEnumNames: TStringList;
    function GetEnumNames(i: integer): string;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Value between 0 .. EnumCount - 1. By default 0. }
    Value: integer;

    DefaultValue: integer;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const AEnumNames: array of string; const AValue: integer);
    destructor Destroy; override;

    { Names of the enumerated values.
      Note that this cannot be initialized by CreateUndefined,
      so you always must initialize this field using the @link(Create)
      overload that takes AEnumNames parameter. }
    property EnumNames[i: integer]:string read GetEnumNames;
    function EnumNamesCount: integer;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;

    procedure Send(const AValue: LongInt); overload;
  end;

  { X3D field containing a floating-point value with Single precision. }
  TSFFloat = class(TX3DSingleField)
  strict private
    FMustBeNonnegative: boolean;
    FValue: Single;
    FAngle: boolean;
    procedure SetValue(const AValue: Single);
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Single;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: Single;
      const AMustBeNonnegative: boolean = false);

    property Value: Single read FValue write SetValue;

    { If @true then when trying to set Value to something < 0,
      we'll negate it (in other words, we'll keep Value >= 0 always).
      This is nice e.g. for Sphere.FdRadius field --- some incorrect VRML specify
      negative sphere radius. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;

    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: Single); overload;
  end;

  { X3D field containing a floating-point value with Double precision. }
  TSFDouble = class(TX3DSingleField)
  strict private
    FValue: Double;
    FAngle: boolean;
    procedure SetValue(const AValue: Double);
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Double;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: Double);

    property Value: Double read FValue write SetValue;

    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: Double); overload;
  end;

  { X3D field containing a time, expressed as seconds that passed since some moment. }
  TSFTime = class(TSFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: Double); overload;
  end;

  { X3D field containing an image that is "embedded" in the X3D file. }
  TSFImage = class(TX3DSingleField)
  strict private
    FValue: TCastleImage;
    procedure SetValue(const AValue: TCastleImage);
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    { Current image, expressed as the @link(TCastleImage) instance.

      The image instance is by default owned by this object,
      which means that we will free it in destructor or when setting
      another value.

      Value may be IsEmpty, and then we know that there is no image
      recorded in this field. Value may never be @nil. }
    property Value: TCastleImage read FValue write SetValue;

    { @param(AValue is the initial value for Value.

        Note - our constructor COPIES passed reference AValue, not it's contents
        (I mean, we do Value := AValue, NOT Value := ImageCopy(AValue),
        so don't Free image given to us (at least, don't do this without clearing
        our Value field)).
        You can pass AValue = nil, then Value will be initialized to null image
        TRGBImage.Create.) }
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: TCastleImage);
    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); override;

    destructor Destroy; override;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function Equals(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  { X3D field containing an single integer 32-bit value. }
  TSFLong = class(TX3DSingleField)
  strict private
    FMustBeNonnegative: boolean;
    FValue: Longint;
    procedure SetValue(const AValue: Longint);
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    DefaultValue: Longint;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: Longint;
      const AMustBeNonnegative: boolean = false);

    property Value: Longint read FValue write SetValue;

    { See TSFFloat.MustBeNonnegative for explanation of this. }
    property MustBeNonnegative: boolean read FMustBeNonnegative default false;
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: LongInt); virtual; overload;
  end;

  { X3D field containing an single integer 32-bit value.
    The name SFInt32 is used instead of SFLong in new X3D versions. }
  TSFInt32 = class(TSFLong)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure Send(const AValue: LongInt); override;
  end;

  { Generic class for an X3D field containing a single matrix value. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TSFGenericMatrix<
    TItem,
    TItemColumn,
    TEvent> = class(TX3DSingleField)
  strict private
    FValue: TItem;
    DefaultValue: TItem;
    DefaultValueExists: boolean;
    class function MatrixSize: Integer;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: TItem);

    property Value: TItem read FValue write FValue;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure Send(const AValue: TItem); overload;

    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  { X3D field containing a 3x3 matrix with Single precision. }
  TSFMatrix3f = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericMatrix<
    TMatrix3,
    TVector3,
    TSFMatrix3fEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 3x3 matrix with Double precision. }
  TSFMatrix3d = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericMatrix<
    TMatrix3Double,
    TVector3Double,
    TSFMatrix3dEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 4x4 matrix with Single precision. }
  TSFMatrix4f = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericMatrix<
    TMatrix4,
    TVector4,
    TSFMatrix4fEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 4x4 matrix with Double precision. }
  TSFMatrix4d = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericMatrix<
    TMatrix4Double,
    TVector4Double,
    TSFMatrix4dEvent>)
  public
    class function X3DType: string; override;
  end;

  { VRML 1.0 SFMatrix field.
    @deprecated This is deprecated, as VRML 1.0 is an ancient format now. }
  TSFMatrix = class(TSFMatrix4f)
  public
    class function X3DType: string; override;
  end;

  { X3D field representing a 3D rotation around an arbitrary axis. }
  TSFRotation = class(TX3DSingleField)
  strict private
    DefaultAxis: TVector3;
    DefaultRotationRad: Single;
    DefaultValueExists: boolean;
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function GetValue: TVector4;
    procedure SetValue(const AValue: TVector4);
    function GetValueDeg: TVector4;
    procedure SetValueDeg(const AValue: TVector4);
  public
    Axis: TVector3;
    RotationRad: Single;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const AnAxis: TVector3; const ARotationRad: Single); overload;
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const AValue: TVector4); overload;

    { Current rotation value, with last component expressing rotation in radians.

      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property Value: TVector4 read GetValue write SetValue;

    { Current rotation value, with last component expressing rotation in degrees.

      So this is just like @link(Value), but last component is in degrees.
      This internally gets / sets values from @link(Axis), @link(RotationRad),
      it only presents them to you differently. }
    property ValueDeg: TVector4 read GetValueDeg write SetValueDeg;

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;
    { Rotate point Pt around Self. }
    function RotatedPoint(const pt: TVector3): TVector3;

    function Equals(SecondValue: TX3DField): boolean; override;
    function EqualsDefaultValue: boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: TVector4); overload;
  end;

  { X3D field containing a single string. }
  TSFString = class(TX3DSingleField)
  strict private
    FValue: string;
    FDefaultValue: string;
    FDefaultValueExists: boolean;
  strict protected
    procedure SetValue(const NewValue: string); virtual;
    procedure SetDefaultValue(const NewDefaultValue: string); virtual;
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    property DefaultValue: string read FDefaultValue write SetDefaultValue;
    property DefaultValueExists: boolean
      read FDefaultValueExists write FDefaultValueExists;
    property Value: string read FValue write SetValue;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: string);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;

    procedure Send(const AValue: string); overload;
  end;

  { X3D field that contains a value from a specified set.
    This represents a commonly used VRML/X3D construct where SFString field
    is used to hold values from some limited set, thus emulating
    an "enumerated" field.

    Access the EnumValue to get / set the field value as an integer,
    which is an index to ValueNames array. }
  TSFStringEnum = class(TSFString)
  strict private
    FEnumNames: TStringList;
    FEnumValue: Integer;
    FDefaultEnumValue: Integer;
    procedure SetEnumValue(const NewEnumValue: Integer);
    procedure SetDefaultEnumValue(const NewDefaultEnumValue: Integer);
  strict protected
    function StringToEnumValue(const NewValue: string): Integer;
    procedure SetValue(const NewValue: string); override;
    procedure SetDefaultValue(const NewDefaultValue: string); override;
    class function ExposedEventsFieldClass: TX3DFieldClass; override;
  public
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const AEnumNames: array of string; const AValue: Integer);
    destructor Destroy; override;
    property EnumValue: Integer
      read FEnumValue write SetEnumValue;
    property DefaultEnumValue: Integer
      read FDefaultEnumValue write SetDefaultEnumValue;
    procedure SendEnumValue(const NewValue: Integer);
  end;

  { X3D field containing a vector. }
  {$ifdef CASTLE_OBJFPC}generic{$endif}
  TSFGenericVector<
    TItem,
    TEvent> = class(TX3DSingleField)
  strict protected
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    Value: TItem;

    DefaultValue: TItem;
    DefaultValueExists: boolean;

    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string; const AValue: TItem);

    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    function EqualsDefaultValue: boolean; override;
    function Equals(SecondValue: TX3DField): boolean; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure Send(const AValue: TItem); overload;

    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  { X3D field containing a 2D vector with Single precision. }
  TSFVec2f = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector2,
    TSFVec2fEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 3D vector with Single precision. }
  TSFVec3f = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector3,
    TSFVec3fEvent>)
  public
    class function X3DType: string; override;
    { Alternative version of @name, change only a given component of the vector. }
    procedure Send(const Index: Integer; const ComponentValue: Single); overload;
  end;

  { X3D field containing an RGB color. }
  TSFColor = class(TSFVec3f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  { X3D field containing a 4D vector with Single precision. }
  TSFVec4f = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector4,
    TSFVec4fEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing an RGBA color (with an alpha). }
  TSFColorRGBA = class(TSFVec4f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  { X3D field containing a 2D vector with Double precision. }
  TSFVec2d = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector2Double,
    TSFVec2dEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 3D vector with Double precision. }
  TSFVec3d = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector3Double,
    TSFVec3dEvent>)
  public
    class function X3DType: string; override;
  end;

  { X3D field containing a 4D vector with Double precision. }
  TSFVec4d = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TSFGenericVector<
    TVector4Double,
    TSFVec4dEvent>)
  public
    class function X3DType: string; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TSFBool -------------------------------------------------------------------- }

constructor TSFBool.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: boolean);
begin
  inherited Create(AParentNode, AExposed, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFBool.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure VRML2BooleanIntegerWarning;
  begin
    if Lexer.Version.Major >= 2 then
      WritelnWarning('X3D', 'In X3D and VRML >= 2.0 you cannot express boolean values as 0 (instead of FALSE) or 1 (instead of TRUE)');
  end;

const
  SBoolExpected = 'boolean constant (TRUE, FALSE)';
begin
  Lexer.CheckTokenIs([vtKeyword, vtInteger], SBoolExpected);
  if Lexer.Token = vtKeyword then
  begin
    if Lexer.TokenKeyword = vkTrue then Value := true else
      if Lexer.TokenKeyword = vkFalse then Value := false else
        raise EX3DParserError.Create(Lexer,
          'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end else
  begin
    if Lexer.TokenInteger = 1 then
    begin
      Value := true;
      VRML2BooleanIntegerWarning;
    end else
    if Lexer.TokenInteger = 0 then
    begin
      Value := false;
      VRML2BooleanIntegerWarning;
    end else
      raise EX3DParserError.Create(Lexer,
        'Expected '+SBoolExpected+', got '+Lexer.DescribeToken);
  end;
  Lexer.NextToken;
end;

const
  BoolKeywords: array [TX3DEncoding, boolean] of string =
  ( ('FALSE', 'TRUE'), ('false', 'true') );

procedure TSFBool.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(BoolKeywords[Writer.Encoding, Value]);
end;

function TSFBool.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFBool.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFBool) and
    (TSFBool(SecondValue).Value = Value);
end;

function TSFBool.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFBool) and
    (TSFBool(SecondValue).Value = Value);
end;

procedure TSFBool.Assign(Source: TPersistent);
begin
  if Source is TSFBool then
  begin
    DefaultValue       := TSFBool(Source).DefaultValue;
    DefaultValueExists := TSFBool(Source).DefaultValueExists;
    Value              := TSFBool(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFBool.AssignValue(Source: TX3DField);
begin
  if Source is TSFBool then
  begin
    inherited;
    Value := TSFBool(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFBool.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFBool.X3DType: string;
begin
  Result := 'SFBool';
end;

procedure TSFBool.Send(const AValue: Boolean; const PartialSend: TPartialSend);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFBool.Create(ParentNode, false, X3DName, AValue);
  try
    FieldValue.InternalPartialSend := PartialSend;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFBool.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFBoolEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFFloat ------------------------------------------------------------------- }

procedure TSFFloat.SetValue(const AValue: Single);
begin
  if MustBeNonnegative then
    FValue := Abs(AValue) else
    FValue := AValue;
end;

constructor TSFFloat.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: Single;
  const AMustBeNonnegative: boolean);
begin
  inherited Create(AParentNode, AExposed, AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue; { Set property, zeby SetValue moglo ew. zmienic Value }
  AssignDefaultValueFromValue;
end;

procedure TSFFloat.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Value := ParseFloat(Lexer);
  if Angle then
    FValue := FValue * Reader.AngleConversionFactor;
end;

procedure TSFFloat.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(Format('%g', [Value]));
end;

function TSFFloat.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value)
end;

function TSFFloat.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFFloat) and
    (TSFFloat(SecondValue).MustBeNonnegative = MustBeNonnegative) and
    SameValue(TSFFloat(SecondValue).Value, Value);
end;

function TSFFloat.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFFloat) and
    (TSFFloat(SecondValue).Value = Value);
end;

procedure TSFFloat.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := Lerp(A, (Value1 as TSFFloat).Value, (Value2 as TSFFloat).Value);
end;

function TSFFloat.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFFloat.Assign(Source: TPersistent);
begin
  if Source is TSFFloat then
  begin
    DefaultValue       := TSFFloat(Source).DefaultValue;
    DefaultValueExists := TSFFloat(Source).DefaultValueExists;
    FValue             := TSFFloat(Source).Value;
    FMustBeNonnegative := TSFFloat(Source).MustBeNonnegative;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFFloat.AssignValue(Source: TX3DField);
begin
  if Source is TSFFloat then
  begin
    inherited;
    Value := TSFFloat(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFFloat.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFFloat.X3DType: string;
begin
  Result := 'SFFloat';
end;

procedure TSFFloat.Send(const AValue: Single);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFFloat.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFFloat.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFFloatEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFDouble -------------------------------------------------------------------- }

constructor TSFDouble.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: Double);
begin
  inherited Create(AParentNode, AExposed, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFDouble.SetValue(const AValue: Double);
begin
  FValue := AValue;
end;

procedure TSFDouble.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Value := ParseFloat(Lexer);
  if Angle then
    FValue := FValue * Reader.AngleConversionFactor;
end;

procedure TSFDouble.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(Format('%g', [Value]));
end;

function TSFDouble.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFDouble.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFDouble) and
    SameValue(TSFDouble(SecondValue).Value, Value);
end;

function TSFDouble.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFDouble) and
    (TSFDouble(SecondValue).Value = Value);
end;

procedure TSFDouble.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := Lerp(A, (Value1 as TSFDouble).Value, (Value2 as TSFDouble).Value);
end;

function TSFDouble.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFDouble.Assign(Source: TPersistent);
begin
  if Source is TSFDouble then
  begin
    DefaultValue       := TSFDouble(Source).DefaultValue;
    DefaultValueExists := TSFDouble(Source).DefaultValueExists;
    FValue             := TSFDouble(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFDouble.AssignValue(Source: TX3DField);
begin
  if Source is TSFDouble then
  begin
    inherited;
    Value := TSFDouble(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFDouble.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFDouble.X3DType: string;
begin
  Result := 'SFDouble';
end;

procedure TSFDouble.Send(const AValue: Double);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFDouble.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFDouble.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFDoubleEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFTime -------------------------------------------------------------------- }

class function TSFTime.X3DType: string;
begin
  Result := 'SFTime';
end;

class function TSFTime.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFTimeEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFTime.Send(const AValue: Double);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFTime.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFImage ------------------------------------------------------------------- }

constructor TSFImage.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: TCastleImage);
begin
  inherited Create(AParentNode, AExposed, AName);

  if AValue <> nil then
    { use SetValue setter, to free previous value if needed }
    Value := AValue;
end;

constructor TSFImage.CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  inherited;

  { Value must be initialized to non-nil. }
  FValue := TRGBImage.Create;
end;

destructor TSFImage.Destroy;
begin
  FreeAndNil(FValue);
  inherited;
end;

procedure TSFImage.SetValue(const AValue: TCastleImage);
begin
  if FValue <> AValue then
  begin
    FreeAndNil(FValue);
    FValue := AValue;
  end;
end;

procedure TSFImage.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  w, h, comp: LongWord;
  i: Cardinal;
  RGBPixels: PVector3ByteArray;
  RGBAlphaPixels: PVector4ByteArray;
  GrayscalePixels: PByteArray;
  GrayscaleAlphaPixels: PVector2ByteArray;
begin
  { Note that we should never let Value to be nil too long,
    because even if this method exits with exception, Value should
    always remain non-nil.
    That's why I'm doing below Value.Empty instead of FreeAndNil(Value).
    This way if e.g. TRGBImage.Create will raise "out of memory" exception,
    Value will still remain non-nil.
  }

  Value.Empty;

  w := ParseLongWord(Lexer);
  h := ParseLongWord(Lexer);
  comp := ParseLongWord(Lexer);

  { If w or h =0 then w*h = 0 so we don't have to read anything more.
    We leave Value.IsEmpty in this case. }
  if (w <> 0) and (h <> 0) then
  begin
    case comp of
      1:begin
          Value := TGrayscaleImage.Create(w, h);
          GrayscalePixels := TGrayscaleImage(Value).PixelsArray;
          for i := 0 to W * H - 1 do
            DecodeX3DColor(ParseLongWord(Lexer), GrayscalePixels^[I]);
        end;
      2:begin
          Value := TGrayscaleAlphaImage.Create(w, h);
          GrayscaleAlphaPixels := TGrayscaleAlphaImage(Value).PixelsArray;
          for i := 0 to W * H - 1 do
            DecodeX3DColor(ParseLongWord(Lexer), GrayscaleAlphaPixels^[i]);
        end;
      3:begin
          Value := TRGBImage.Create(w, h);
          RGBPixels := TRGBImage(Value).PixelsArray;
          for i := 0 to W * H - 1 do
            DecodeX3DColor(ParseLongWord(Lexer), RGBPixels^[i]);
        end;
      4:begin
          Value := TRGBAlphaImage.Create(w, h);
          RGBAlphaPixels := TRGBAlphaImage(Value).PixelsArray;
          for i := 0 to W * H - 1 do
            DecodeX3DColor(ParseLongWord(Lexer), RGBAlphaPixels^[i]);
        end;
      else raise EX3DParserError.Create(Lexer, Format('Invalid components count'+
             ' for SFImage : is %d, should be 1, 2, 3 or 4.',[comp]));
    end;
  end;
end;

procedure TSFImage.SaveToStreamValue(Writer: TX3DWriter);
var
  ga: TVector2Byte;
  rgb: TVector3Byte;
  rgba: TVector4Byte;
  i: Cardinal;
  pixel: LongWord;
begin
  if Value.IsEmpty then
    Writer.Write('0 0 1') else
  begin
    Writer.Writeln(Format('%d %d %d', [
      Value.Width,
      Value.Height,
      Value.ColorComponentsCount]));
    Writer.IncIndent;
    Writer.WriteIndent('');
    {$I NoRQCheckBegin.inc}
    if Value is TGrayscaleImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        pixel := TGrayscaleImage(Value).PixelsArray^[i];
        Writer.Write(Format('0x%.2x ', [pixel]));
      end;
    end else
    if Value is TGrayscaleAlphaImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        ga := TGrayscaleAlphaImage(Value).PixelsArray^[i];
        pixel := (ga[0] shl 8) or ga[1];
        Writer.Write(Format('0x%.4x ', [pixel]));
      end;
    end else
    if Value is TRGBImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        rgb := TRGBImage(Value).PixelsArray^[i];
        pixel := (rgb[0] shl 16) or (rgb[1] shl 8) or rgb[2];
        Writer.Write(Format('0x%.6x ', [pixel]));
      end;
    end else
    if Value is TRGBAlphaImage then
    begin
      for i := 0 to Value.Width * Value.Height - 1 do
      begin
        rgba := TRGBAlphaImage(Value).PixelsArray^[i];
        pixel := (rgba[0] shl 24) or (rgba[1] shl 16) or (rgba[2] shl 8) or rgba[3];
        Writer.Write(Format('0x%.8x ', [pixel]));
      end;
    end else
      raise Exception.Create('TSFImage.SaveToStreamValue - not implemented TCastleImage descendant');
    {$I NoRQCheckEnd.inc}
    Writer.DecIndent;
  end;
end;

function TSFImage.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFImage) and
    { TODO: compare values
    (TSFImage(SecondValue).Value = Value) }true;
end;

procedure TSFImage.Assign(Source: TPersistent);
begin
  if Source is TSFImage then
  begin
    Value := TSFImage(Source).Value.MakeCopy;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFImage.AssignValue(Source: TX3DField);
begin
  if Source is TSFImage then
  begin
    inherited;
    Value := TSFImage(Source).Value.MakeCopy;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFImage.X3DType: string;
begin
  Result := 'SFImage';
end;

class function TSFImage.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFImageEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFLong -------------------------------------------------------------------- }

procedure TSFLong.SetValue(const AValue: Longint);
begin
  if MustBeNonnegative then
    FValue := Abs(AValue) else
    FValue := AValue;
end;

constructor TSFLong.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: Longint;
  const AMustBeNonnegative: boolean);
begin
  inherited Create(AParentNode, AExposed, AName);

  FMustBeNonnegative := AMustBeNonnegative;
  Value := AValue; { Set using property, to let SetValue change Value }
  AssignDefaultValueFromValue;
end;

procedure TSFLong.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Lexer.CheckTokenIs(vtInteger);

  { Check is TokenInteger outside of 32-bit range. }
  if (Lexer.TokenInteger >= Low(LongInt)) and
     (Lexer.TokenInteger <= High(LongInt)) then
  begin
    Value := Lexer.TokenInteger;
  end else
  begin
    WritelnWarning('X3D', Format('Integer in the file is out of 32-bit range: %d',
      [Lexer.TokenInteger]));
    Value := -1;
  end;

  Lexer.NextToken;
end;

procedure TSFLong.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(IntToStr(Value));
end;

function TSFLong.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value)
end;

function TSFLong.Equals(SecondValue: TX3DField): boolean;
begin
  { Note that this means that SFInt32 and SFLong will actually be considered
    equal. That's Ok, we want this. }
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFLong) and
    (TSFLong(SecondValue).MustBeNonnegative = MustBeNonnegative) and
    (TSFLong(SecondValue).Value = Value);
end;

function TSFLong.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFLong) and
    (TSFLong(SecondValue).Value = Value);
end;

procedure TSFLong.Assign(Source: TPersistent);
begin
  if Source is TSFLong then
  begin
    DefaultValue       := TSFLong(Source).DefaultValue;
    DefaultValueExists := TSFLong(Source).DefaultValueExists;
    FValue             := TSFLong(Source).Value;
    FMustBeNonnegative := TSFLong(Source).MustBeNonnegative;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFLong.AssignValue(Source: TX3DField);
begin
  if Source is TSFLong then
  begin
    inherited;
    Value := TSFLong(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFLong.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFLong.X3DType: string;
begin
  Result := 'SFLong';
end;

procedure TSFLong.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFLong.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFLong.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFLongEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFInt32 ------------------------------------------------------------------- }

class function TSFInt32.X3DType: string;
begin
  Result := 'SFInt32';
end;

procedure TSFInt32.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFInt32.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFInt32.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFInt32Event.Create(AParentNode, AName, AInEvent);
end;

{ TSFGenericMatrix ---------------------------------------------------------------------------- }

constructor TSFGenericMatrix.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: TItem);
begin
  inherited Create(AParentNode, AExposed, AName);
  FValue := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFGenericMatrix.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  Column, Row: integer;
begin
  for Column := 0 to MatrixSize - 1 do
  begin
    for Row := 0 to MatrixSize - 1 do
    begin
      Lexer.CheckTokenIs(TokenNumbers, 'float number');
      FValue.Data[Column, Row] := Lexer.TokenFloat;
      Lexer.NextToken;
    end;

    // Calling here global ParseVector or ParseFloat causes
    // Error: Global Generic template references static symtable
    // with FPC 3.0.2. TODO: test other FPC versions, potentially submit FPC bug.
    // ParseVector(, Lexer);
  end;
end;

procedure TSFGenericMatrix.SaveToStreamValue(Writer: TX3DWriter);
var
  V: TItemColumn;
  Column: integer;
begin
  V.Data := FValue.Data[0];
  Writer.Writeln(V.ToRawString);

  Writer.IncIndent;
  for Column := 1 to MatrixSize - 1 do
  begin
    V.Data := FValue.Data[Column];
    Writer.WritelnIndent(V.ToRawString);
  end;
  Writer.DecIndent;
end;

function TSFGenericMatrix.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFGenericMatrix) and
    TItem.Equals(TSFGenericMatrix(SecondValue).FValue, FValue);
end;

function TSFGenericMatrix.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFGenericMatrix) and
    TItem.PerfectlyEquals(TSFGenericMatrix(SecondValue).Value, FValue);
end;

procedure TSFGenericMatrix.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := TItem.Lerp(A, (Value1 as TSFGenericMatrix).Value, (Value2 as TSFGenericMatrix).Value);
end;

function TSFGenericMatrix.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFGenericMatrix.Assign(Source: TPersistent);
begin
  if Source is TSFGenericMatrix then
  begin
    FValue := TSFGenericMatrix(Source).FValue;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFGenericMatrix.AssignValue(Source: TX3DField);
begin
  if Source is TSFGenericMatrix then
  begin
    inherited;
    FValue := TSFGenericMatrix(Source).FValue;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

function TSFGenericMatrix.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and
    TItem.PerfectlyEquals(DefaultValue, Value);
end;

procedure TSFGenericMatrix.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFGenericMatrix.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TEvent.Create(AParentNode, AName, AInEvent);
end;

class function TSFGenericMatrix.MatrixSize: Integer;
begin
  Result := High(TItemColumn.TIndex) + 1;
end;

procedure TSFGenericMatrix.Send(const AValue: TItem);
var
  FieldValue: TSFGenericMatrix;
begin
  FieldValue := TSFGenericMatrix(
    TX3DFieldClass(ClassType).CreateUndefined(ParentNode, false, X3DName));
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFMatrix3f ------------------------------------------------------------------ }

class function TSFMatrix3f.X3DType: string;
begin
  Result := 'SFMatrix3f';
end;

{ TSFMatrix3d ------------------------------------------------------------------ }

class function TSFMatrix3d.X3DType: string;
begin
  Result := 'SFMatrix3d';
end;

{ TSFMatrix4f ------------------------------------------------------------------ }

class function TSFMatrix4f.X3DType: string;
begin
  Result := 'SFMatrix4f';
end;

{ TSFMatrix4d ------------------------------------------------------------------ }

class function TSFMatrix4d.X3DType: string;
begin
  Result := 'SFMatrix4d';
end;

{ TSFMatrix ------------------------------------------------------------------ }

class function TSFMatrix.X3DType: string;
begin
  Result := 'SFMatrix';
end;

{ TSFRotation ---------------------------------------------------------------- }

constructor TSFRotation.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string;
  const AnAxis: TVector3; const ARotationRad: Single);
begin
  inherited Create(AParentNode, AExposed, AName);

  Axis := AnAxis;
  RotationRad := ARotationRad;

  AssignDefaultValueFromValue;
end;

constructor TSFRotation.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string;
  const AValue: TVector4);
var
  AnAxis: TVector3 absolute AValue;
begin
  inherited Create(AParentNode, AExposed, AName);

  Axis := AnAxis;
  RotationRad := AValue[3];

  AssignDefaultValueFromValue;
end;

procedure TSFRotation.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  ParseVector(Axis.Data, Lexer);
  RotationRad := ParseFloat(Lexer) * Reader.AngleConversionFactor;
end;

function TSFRotation.GetValue: TVector4;
begin
  Move(Axis.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Result[3] := RotationRad;
end;

procedure TSFRotation.SetValue(const AValue: TVector4);
begin
  Axis[0] := AValue[0];
  Axis[1] := AValue[1];
  Axis[2] := AValue[2];
  RotationRad := AValue[3];
end;

function TSFRotation.GetValueDeg: TVector4;
begin
  Move(Axis.Data[0], Result.Data[0], SizeOf(Single) * 3);
  Result[3] := RadToDeg(RotationRad);
end;

procedure TSFRotation.SetValueDeg(const AValue: TVector4);
begin
  Axis[0] := AValue[0];
  Axis[1] := AValue[1];
  Axis[2] := AValue[2];
  RotationRad := DegToRad(AValue[3]);
end;

procedure TSFRotation.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(Axis.ToRawString +' ' +Format('%g', [RotationRad]));
end;

function TSFRotation.RotatedPoint(const pt: TVector3): TVector3;
begin
  if not Axis.IsZero then
    Result := RotatePointAroundAxisRad(RotationRad, pt, Axis) else
  begin
    { Safeguard against rotation around zero vector, which produces unpredictable
      results (actually, Result would be filled with Nan values).
      VRML spec says that SFRotation should always specify a normalized vector. }
    Result := Pt;
    WritelnWarning('X3D', Format('SFRotation field (%s) specifies rotation around zero vector', [NiceName]));
  end;
end;

function TSFRotation.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFRotation) and
    TVector3.Equals(TSFRotation(SecondValue).Axis, Axis) and
    SameValue(TSFRotation(SecondValue).RotationRad, RotationRad);
end;

function TSFRotation.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFRotation) and
    TVector3.PerfectlyEquals(TSFRotation(SecondValue).Axis, Axis) and
    (TSFRotation(SecondValue).RotationRad = RotationRad);
end;

function TSFRotation.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and
    TVector3.PerfectlyEquals(DefaultAxis, Axis) and
    (DefaultRotationRad = RotationRad);
end;

procedure TSFRotation.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  { interpolate using slerp (testcase when linear interpolation on axis/vector fails:
    god triangle in escape_universe) }
  Value := SLerp(A, (Value1 as TSFRotation).Value, (Value2 as TSFRotation).Value);
end;

function TSFRotation.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFRotation.Assign(Source: TPersistent);
begin
  if Source is TSFRotation then
  begin
    Axis        := TSFRotation(Source).Axis;
    RotationRad := TSFRotation(Source).RotationRad;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFRotation.AssignValue(Source: TX3DField);
begin
  if Source is TSFRotation then
  begin
    inherited;
    Axis := TSFRotation(Source).Axis;
    RotationRad := TSFRotation(Source).RotationRad;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFRotation.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultAxis := Axis;
  DefaultRotationRad := RotationRad;
  DefaultValueExists := true;
end;

class function TSFRotation.X3DType: string;
begin
  Result := 'SFRotation';
end;

procedure TSFRotation.Send(const AValue: TVector4);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFRotation.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

class function TSFRotation.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFRotationEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFString ------------------------------------------------------------------ }

constructor TSFString.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: string);
begin
  inherited Create(AParentNode, AExposed, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFString.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
begin
  Lexer.CheckTokenIs(vtString);
  Value := Lexer.TokenString;
  Lexer.NextToken;
end;

procedure TSFString.SaveToStreamValue(Writer: TX3DWriter);
begin
  case Writer.Encoding of
    xeClassic: Writer.Write(StringToX3DClassic(Value));
    xeXML    : Writer.Write(StringToX3DXml(Value));
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TSFString.SaveToStreamValue Encoding?');
    {$endif}
  end;
end;

function TSFString.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFString.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFString) and
    (TSFString(SecondValue).Value = Value);
end;

function TSFString.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFString) and
    (TSFString(SecondValue).Value = Value);
end;

procedure TSFString.Assign(Source: TPersistent);
begin
  if Source is TSFString then
  begin
    DefaultValue       := TSFString(Source).DefaultValue;
    DefaultValueExists := TSFString(Source).DefaultValueExists;
    Value              := TSFString(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFString.AssignValue(Source: TX3DField);
begin
  if Source is TSFString then
  begin
    inherited;
    Value := TSFString(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFString.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFString.X3DType: string;
begin
  Result := 'SFString';
end;

procedure TSFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
begin
  { SFString has quite special interpretation, it's just attrib
    name. It would not be usefull trying to use TX3DLexer here,
    it's easier just to handle this as a special case.

    Uhm... some X3D XML files commit the reverse mistake
    as for MFString: they *include* additional quotes around the string.
    Spec says that for SFString, such quotes are not needed.
    Example: openlibraries trunk/media files.

    I detect this, warn and strip quotes. }
  if (Length(AttributeValue) >= 2) and
     (AttributeValue[1] = '"') and
     (AttributeValue[Length(AttributeValue)] = '"') then
  begin
    WritelnWarning('X3D', 'X3D XML: found extra quotes around SFString value. Assuming this is a mistake, and stripping quotes from ''' + AttributeValue + '''. Fix your model: SFString field values should not be enclosed in extra quotes!');
    Value := Copy(AttributeValue, 2, Length(AttributeValue) - 2);
  end else
    Value := AttributeValue;
end;

procedure TSFString.Send(const AValue: string);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFString.Create(ParentNode, false, X3DName, AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

function TSFString.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttributeCustomQuotes;
end;

class function TSFString.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFStringEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFString.SetValue(const NewValue: string);
begin
  FValue := NewValue;
end;

procedure TSFString.SetDefaultValue(const NewDefaultValue: string);
begin
  FDefaultValue := NewDefaultValue;
end;

{ TSFStringEnum -------------------------------------------------------------- }

constructor TSFStringEnum.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AEnumNames: array of string; const AValue: integer);
begin
  FEnumNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AEnumNames, FEnumNames);

  inherited Create(AParentNode, AExposed, AName, FEnumNames[AValue]);
  { inherited Create will assign Value, and in SetValue should cause setting
    our FEnumValue }
  Assert(AValue = FEnumValue);
end;

destructor TSFStringEnum.Destroy;
begin
  FreeAndNil(FEnumNames);
  inherited;
end;

class function TSFStringEnum.ExposedEventsFieldClass: TX3DFieldClass;
begin
  Result := TSFString;
end;

function TSFStringEnum.StringToEnumValue(const NewValue: string): Integer;
var
  UpperValue: string;
begin
  UpperValue := UpperCase(NewValue);
  if UpperValue <> NewValue then
    WritelnWarning('X3D', Format('Field "%s" value should be uppercase, but is not: "%s"',
      [X3DName, NewValue]));

  Result := FEnumNames.IndexOf(UpperValue);
  if Result = -1 then
  begin
    Result := DefaultEnumValue;
    WritelnWarning('X3D', Format('Unknown "%s" field value: "%s"',
      [X3DName, NewValue]));
  end;
end;

procedure TSFStringEnum.SetValue(const NewValue: string);
begin
  inherited SetValue(NewValue);
  { calculate new FEnumValue, IOW convert string NewValue to integer }
  FEnumValue := StringToEnumValue(NewValue);
end;

procedure TSFStringEnum.SetEnumValue(const NewEnumValue: Integer);
begin
  inherited SetValue(FEnumNames[NewEnumValue]);
  FEnumValue := NewEnumValue;
end;

procedure TSFStringEnum.SendEnumValue(const NewValue: Integer);
begin
  inherited Send(FEnumNames[NewValue]);
end;

procedure TSFStringEnum.SetDefaultValue(const NewDefaultValue: string);
begin
  inherited SetDefaultValue(NewDefaultValue);
  FDefaultEnumValue := StringToEnumValue(NewDefaultValue);
end;

procedure TSFStringEnum.SetDefaultEnumValue(const NewDefaultEnumValue: Integer);
begin
  inherited SetDefaultValue(FEnumNames[NewDefaultEnumValue]);
  FDefaultEnumValue := NewDefaultEnumValue;
end;

{ TSFGenericVector ----------------------------------------------------------- }

constructor TSFGenericVector.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AValue: TItem);
begin
  inherited Create(AParentNode, AExposed, AName);

  Value := AValue;
  AssignDefaultValueFromValue;
end;

procedure TSFGenericVector.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  I: Integer;
begin
  for I := 0 to High(Value.Data) do
  begin
    Lexer.CheckTokenIs(TokenNumbers, 'float number');
    Value.Data[I] := Lexer.TokenFloat;
    Lexer.NextToken;
  end;

  // Calling ParseVector or ParseFloat here causes FPC 3.0.2 error
  // Error: Global Generic template references static symtable
  // TODO: check on other FPC versions and report.
  // ParseVector(Value.Data, Lexer);
end;

procedure TSFGenericVector.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(Value.ToRawString);
end;

function TSFGenericVector.EqualsDefaultValue: boolean;
begin
  Result := DefaultValueExists and TItem.PerfectlyEquals(DefaultValue, Value);
end;

function TSFGenericVector.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFGenericVector) and
    TItem.Equals(TSFGenericVector(SecondValue).Value, Value);
end;

function TSFGenericVector.FastEqualsValue(SecondValue: TX3DField): boolean;
begin
  Result := (SecondValue is TSFGenericVector) and
    TItem.PerfectlyEquals(TSFGenericVector(SecondValue).Value, Value);
end;

procedure TSFGenericVector.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Value := TItem.Lerp(A, (Value1 as TSFGenericVector).Value, (Value2 as TSFGenericVector).Value);
end;

function TSFGenericVector.CanAssignLerp: boolean;
begin
  Result := true;
end;

procedure TSFGenericVector.Assign(Source: TPersistent);
begin
  if Source is TSFGenericVector then
  begin
    DefaultValue       := TSFGenericVector(Source).DefaultValue;
    DefaultValueExists := TSFGenericVector(Source).DefaultValueExists;
    Value              := TSFGenericVector(Source).Value;
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFGenericVector.AssignValue(Source: TX3DField);
begin
  if Source is TSFGenericVector then
  begin
    inherited;
    Value := TSFGenericVector(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFGenericVector.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFGenericVector.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TEvent.Create(AParentNode, AName, AInEvent);
end;

procedure TSFGenericVector.Send(const AValue: TItem);
var
  FieldValue: TSFGenericVector;
begin
  FieldValue := TSFGenericVector(
    TX3DFieldClass(ClassType).CreateUndefined(ParentNode, false, X3DName));
  try
    FieldValue.Value := AValue;
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{ TSFVec2f ------------------------------------------------------------------- }

class function TSFVec2f.X3DType: string;
begin
  Result := 'SFVec2f';
end;

{ TSFVec3f ------------------------------------------------------------------- }

class function TSFVec3f.X3DType: string;
begin
  Result := 'SFVec3f';
end;

procedure TSFVec3f.Send(const Index: Integer; const ComponentValue: Single);
var
  V: TVector3;
begin
  V := Value;
  V[Index] := ComponentValue;
  Send(V);
end;

{ TSFColor ------------------------------------------------------------------- }

class function TSFColor.X3DType: string;
begin
  Result := 'SFColor';
end;

class function TSFColor.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFColorEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFVec4f ------------------------------------------------------------------- }

class function TSFVec4f.X3DType: string;
begin
  Result := 'SFVec4f';
end;

{ TSFColorRGBA --------------------------------------------------------------- }

class function TSFColorRGBA.X3DType: string;
begin
  Result := 'SFColorRGBA';
end;

class function TSFColorRGBA.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TSFColorRGBAEvent.Create(AParentNode, AName, AInEvent);
end;

{ TSFVec2d ------------------------------------------------------------------- }

class function TSFVec2d.X3DType: string;
begin
  Result := 'SFVec2d';
end;

{ TSFVec3d ------------------------------------------------------------------- }

class function TSFVec3d.X3DType: string;
begin
  Result := 'SFVec3d';
end;

{ TSFVec4d ------------------------------------------------------------------- }

class function TSFVec4d.X3DType: string;
begin
  Result := 'SFVec4d';
end;

{ TSFBitMask ------------------------------------------------------------ }

constructor TSFBitMask.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AFlagNames: array of string;
  const ANoneString, AAllString: string; const AFlags: array of boolean);
var
  i: integer;
begin
  inherited Create(AParentNode, AExposed, AName);

  fFlagNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AFlagNames, fFlagNames);
  for i := 0 to FlagsCount-1 do Flags[i] := AFlags[i];
  fNoneString := ANoneString;
  fAllString := AAllString;

  Assert(NoneString <> '', 'NoneString must be defined for SFBitMask');
end;

destructor TSFBitMask.Destroy;
begin
  fFlagNames.Free;
  inherited;
end;

function TSFBitMask.GetFlags(i: integer): boolean;
begin
  result := i in fFlags
end;

procedure TSFBitMask.SetFlags(i: integer; value: boolean);
begin
  if value then Include(fFlags, i) else Exclude(fFlags, i)
end;

function TSFBitMask.FlagsCount: integer;
begin
  result := fFlagNames.Count
end;

function TSFBitMask.GetFlagNames(i: integer): string;
begin
  result := fFlagNames[i]
end;

procedure TSFBitMask.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);

  procedure InterpretTokenAsFlagName;
  var
    i: integer;
  begin
    Lexer.CheckTokenIs(vtName, 'bit mask constant');
    i := fFlagNames.IndexOf(Lexer.TokenName);
    if i >= 0 then
      Flags[i] := true else
    if Lexer.TokenName = fAllString then
      fFlags := [0..FlagsCount-1] else
    if Lexer.TokenName = fNoneString then
      { Don't set anything. Note that this doesn't clear other flags,
        so e.g. "( FLAG_1 | NONE )" equals just "FLAG_1". } else
      raise EX3DParserError.Create(Lexer,
        'Expected bit mask constant, got '+Lexer.DescribeToken);
  end;

begin
  fFlags:=[];

  if Lexer.Token = vtOpenBracket then
  begin
    repeat
      Lexer.NextToken;
      InterpretTokenAsFlagName;
      Lexer.NextToken;
    until Lexer.Token <> vtBar;
    Lexer.CheckTokenIs(vtCloseBracket);
    Lexer.NextToken;
  end else
  begin
    InterpretTokenAsFlagName;
    Lexer.NextToken;
  end;
end;

function TSFBitMask.AreAllFlags(value: boolean): boolean;
var
  i: integer;
begin
  for i := 0 to FlagsCount-1 do
    if Flags[i] <> value then exit(false);
  exit(true);
end;

procedure TSFBitMask.SaveToStreamValue(Writer: TX3DWriter);
var
  i: integer;
  PrecedeWithBar: boolean;
begin
  { This is an VRML 1.0 (and Inventor) type. The existing specs only say
    how to encode it for classic encoding. For XML, we just use the same format. }
  if AreAllFlags(false) then
    Writer.Write(NoneString) else
  begin
    { We don't really need AllString to express that all bit are set
      (we could as well just name them all), but it looks nicer. }
    if (AllString <> '') and AreAllFlags(true) then
      Writer.Write(AllString) else
    begin
      PrecedeWithBar := false;
      Writer.Write('(');
      for i := 0 to FlagsCount-1 do
        if Flags[i] then
        begin
          if PrecedeWithBar then Writer.Write('|') else PrecedeWithBar := true;
          Writer.Write(FlagNames[i]);
        end;
      Writer.Write(')');
    end;
  end;
end;

function TSFBitMask.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFBitMask) and
    (TSFBitMask(SecondValue).FFlagNames.Equals(FFlagNames)) and
    (TSFBitMask(SecondValue).FFlags = FFlags) and
    (TSFBitMask(SecondValue).AllString = AllString) and
    (TSFBitMask(SecondValue).NoneString = NoneString);
end;

procedure TSFBitMask.Assign(Source: TPersistent);
begin
  if Source is TSFBitMask then
  begin
    FAllString  := TSFBitMask(Source).AllString;
    FNoneString := TSFBitMask(Source).NoneString;
    FFlags      := TSFBitMask(Source).FFlags;
    FFlagNames.Assign(TSFBitMask(Source).FFlagNames);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFBitMask.AssignValue(Source: TX3DField);
begin
  if Source is TSFBitMask then
  begin
    inherited;
    FFlags := TSFBitMask(Source).FFlags;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

class function TSFBitMask.X3DType: string;
begin
  Result := 'SFBitMask';
end;

{ TSFEnum ----------------------------------------------------------------- }

constructor TSFEnum.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string; const AEnumNames: array of string; const AValue: integer);
begin
  inherited Create(AParentNode, AExposed, AName);

  FEnumNames := TStringListCaseSens.Create;
  AddStrArrayToStrings(AEnumNames, FEnumNames);
  Value := AValue;
  AssignDefaultValueFromValue;
end;

destructor TSFEnum.Destroy;
begin
  FreeAndNil(FEnumNames);
  inherited;
end;

function TSFEnum.GetEnumNames(i: integer): string;
begin
  result := FEnumNames[i]
end;

function TSFEnum.EnumNamesCount: integer;
begin
  result := FEnumNames.Count
end;

procedure TSFEnum.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  val: integer;
begin
  Lexer.CheckTokenIs(vtName, 'enumerated type constant');
  val := FEnumNames.IndexOf(Lexer.TokenName);
  if val = -1 then
   raise EX3DParserError.Create(Lexer,
     'Expected enumerated type constant, got '+Lexer.DescribeToken);
  Value := val;
  Lexer.NextToken;
end;

procedure TSFEnum.SaveToStreamValue(Writer: TX3DWriter);
begin
  Writer.Write(EnumNames[Value]);
end;

function TSFEnum.EqualsDefaultValue: boolean;
begin
  result := DefaultValueExists and (DefaultValue = Value);
end;

function TSFEnum.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and
    (SecondValue is TSFEnum) and
    (TSFEnum(SecondValue).FEnumNames.Equals(FEnumNames)) and
    (TSFEnum(SecondValue).Value = Value);
end;

procedure TSFEnum.Assign(Source: TPersistent);
begin
  if Source is TSFEnum then
  begin
    DefaultValue       := TSFEnum(Source).DefaultValue;
    DefaultValueExists := TSFEnum(Source).DefaultValueExists;
    Value              := TSFEnum(Source).Value;
    FEnumNames.Assign(TSFEnum(Source).FEnumNames);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TSFEnum.AssignValue(Source: TX3DField);
begin
  if Source is TSFEnum then
  begin
    inherited;
    Value := TSFEnum(Source).Value;
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TSFEnum.AssignDefaultValueFromValue;
begin
  inherited;
  DefaultValue := Value;
  DefaultValueExists := true;
end;

class function TSFEnum.X3DType: string;
begin
  Result := 'SFEnum';
end;

procedure TSFEnum.Send(const AValue: LongInt);
var
  FieldValue: TX3DField;
begin
  FieldValue := TSFEnum.Create(ParentNode, false, X3DName, [], AValue);
  try
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{$endif read_implementation}
