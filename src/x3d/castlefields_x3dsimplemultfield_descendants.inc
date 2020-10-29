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

  { Multiple value (MF) fields ----------------------------------------------- }

  { }
  TMFBool = class(specialize TX3DSimpleMultField<
    boolean,
    TSFBool,
    TBooleanList,
    TMFBoolEvent>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
  public
    class function X3DType: string; override;
  end;

  TMFLong = class(specialize TX3DSimpleMultField<
    LongInt,
    TSFLong,
    TLongIntList,
    TMFLongEvent>)
  strict private
    FSaveToStreamLineUptoNegative: boolean;
    WrongVertexIndexWarnings: Integer;
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    { When saving to VRML/X3D classic encoding,
      make newline after each negative value on the list.
      This makes a nice output for fields like IndexedFaceSet.coordIndex. }
    property SaveToStreamLineUptoNegative: boolean
      read FSaveToStreamLineUptoNegative write FSaveToStreamLineUptoNegative
      default false;

    class function X3DType: string; override;

    { Call WritelnWarning reporting that an invalid vertex index
      is caused from this field. This simply calls WritelnWarning
      formatting appropriate message.

      Additionally this guards
      us against producing too many warnings from the same field.
      When a given threshold will be reached, further
      WritelnWarning_WrongVertexIndex calls for this field instance
      will be simply ignored. This is a good thing, as some invalid models
      have really an incredible amount of invalid indexes, and the very
      amount of lines printed on console makes viewing these invalid files
      (at least, the valid parts of them) impossible.

      Example test cases:
      content/examples/Basic/HumanoidAnimation/AllenStandShootRifleM24.x3d
      and
      content/examples/Basic/HumanoidAnimation/NancyDiving.x3dv
      from http://www.web3d.org/ example models. }
    procedure WritelnWarning_WrongVertexIndex(
      const GeometryX3DType: string;
      const VertexNum: Integer; const CoordCount: Integer);
  end;

  TMFInt32 = class(TMFLong)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFMatrix3f = class(specialize TX3DSimpleMultField<
    TMatrix3,
    TSFMatrix3f,
    TMatrix3List,
    TMFMatrix3fEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFMatrix3d = class(specialize TX3DSimpleMultField<
    TMatrix3Double,
    TSFMatrix3d,
    TMatrix3DoubleList,
    TMFMatrix3dEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFMatrix4f = class(specialize TX3DSimpleMultField<
    TMatrix4,
    TSFMatrix4f,
    TMatrix4List,
    TMFMatrix4fEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFMatrix4d = class(specialize TX3DSimpleMultField<
    TMatrix4Double,
    TSFMatrix4d,
    TMatrix4DoubleList,
    TMFMatrix4dEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFVec2f = class(specialize TX3DSimpleMultField<
    TVector2,
    TSFVec2f,
    TVector2List,
    TMFVec2fEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFVec3f = class(specialize TX3DSimpleMultField<
    TVector3,
    TSFVec3f,
    TVector3List,
    TMFVec3fEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFColor = class(TMFVec3f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec4f = class(specialize TX3DSimpleMultField<
    TVector4,
    TSFVec4f,
    TVector4List,
    TMFVec4fEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFColorRGBA = class(TMFVec4f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec2d = class(specialize TX3DSimpleMultField<
    TVector2Double,
    TSFVec2d,
    TVector2DoubleList,
    TMFVec2dEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFVec3d = class(specialize TX3DSimpleMultField<
    TVector3Double,
    TSFVec3d,
    TVector3DoubleList,
    TMFVec3dEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFVec4d = class(specialize TX3DSimpleMultField<
    TVector4Double,
    TSFVec4d,
    TVector4DoubleList,
    TMFVec4dEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFRotation = class(specialize TX3DSimpleMultField<
    TVector4,
    TSFRotation,
    TVector4List,
    TMFRotationEvent>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFFloat = class(specialize TX3DSimpleMultField<
    Single,
    TSFFloat,
    TSingleList,
    TMFFloatEvent>)
  strict private
    FAngle: boolean;
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
    function CreateItemBeforeParse: TSFFloat; override;
  public
    { Value represents an angle. When reading from X3D 3.3 file, we will
      make sure it's expressed in radians, honoring optional "UNIT angle ..."
      declaration in X3D file. }
    property Angle: boolean read FAngle write FAngle default false;

    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFDouble = class(specialize TX3DSimpleMultField<
    Double,
    TSFDouble,
    TDoubleList,
    TMFDoubleEvent>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
  end;

  TMFTime = class(TMFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFString = class(specialize TX3DSimpleMultField<
    string,
    TSFString,
    TCastleStringList,
    TMFStringEvent>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
  public
    class function X3DType: string; override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMFBool ------------------------------------------------------------------ }

function TMFBool.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := BoolKeywords[Encoding][Items[ItemNum]];
end;

class function TMFBool.X3DType: string;
begin
  Result := 'MFBool';
end;

{ TMFLong -------------------------------------------------------------------- }

function TMFLong.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := SaveToStreamLineUptoNegative and (Items[ItemNum] < 0);
end;

function TMFLong.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := IntToStr(Items[ItemNum])
end;

class function TMFLong.X3DType: string;
begin
  Result := 'MFLong';
end;

procedure TMFLong.WritelnWarning_WrongVertexIndex(
  const GeometryX3DType: string;
  const VertexNum: Integer; const CoordCount: Integer);
const
  MaxWrongVertexIndexWarnings = 10;
begin
  Inc(WrongVertexIndexWarnings);
  if WrongVertexIndexWarnings < MaxWrongVertexIndexWarnings then
    WritelnWarning('X3D', Format('Wrong vertex index in indexed node %s (not enough points in Coordinate node defined: index is %d, we have only %d vertices)',
      [GeometryX3DType, VertexNum, CoordCount])) else
  if WrongVertexIndexWarnings = MaxWrongVertexIndexWarnings then
    WritelnWarning('X3D', Format('Wrong vertex index in indexed node %s reported for the %dth time. Further warnings regarding this field will not be reported (to avoid wasting time on printing countless warnings...)',
      [GeometryX3DType, WrongVertexIndexWarnings]));
end;

{ TMFInt32 ------------------------------------------------------------------- }

class function TMFInt32.X3DType: string;
begin
  Result := 'MFInt32';
end;

class function TMFInt32.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFInt32Event.Create(AParentNode, AName, AInEvent);
end;

{ TMFMatrix3f ------------------------------------------------------------------- }

class function TMFMatrix3f.X3DType: string;
begin
  Result := 'MFMatrix3f';
end;

function TMFMatrix3f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFMatrix3f.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFMatrix3f).Items, (Value2 as TMFMatrix3f).Items);
end;

function TMFMatrix3f.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFMatrix3d ------------------------------------------------------------------- }

class function TMFMatrix3d.X3DType: string;
begin
  Result := 'MFMatrix3d';
end;

function TMFMatrix3d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFMatrix3d.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFMatrix3d).Items, (Value2 as TMFMatrix3d).Items);
end;

function TMFMatrix3d.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFMatrix4f ------------------------------------------------------------------- }

class function TMFMatrix4f.X3DType: string;
begin
  Result := 'MFMatrix4f';
end;

function TMFMatrix4f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFMatrix4f.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFMatrix4f).Items, (Value2 as TMFMatrix4f).Items);
end;

function TMFMatrix4f.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFMatrix4d ------------------------------------------------------------------- }

class function TMFMatrix4d.X3DType: string;
begin
  Result := 'MFMatrix4d';
end;

function TMFMatrix4d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFMatrix4d.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFMatrix4d).Items, (Value2 as TMFMatrix4d).Items);
end;

function TMFMatrix4d.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFVec2f ------------------------------------------------------------------- }

class function TMFVec2f.X3DType: string;
begin
  Result := 'MFVec2f';
end;

function TMFVec2f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec2f.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec2f).Items, (Value2 as TMFVec2f).Items);
end;

function TMFVec2f.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFVec3f ------------------------------------------------------------------- }

class function TMFVec3f.X3DType: string;
begin
  Result := 'MFVec3f';
end;

function TMFVec3f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec3f.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec3f).Items, (Value2 as TMFVec3f).Items);
end;

function TMFVec3f.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFColor ------------------------------------------------------------------- }

class function TMFColor.X3DType: string;
begin
  Result := 'MFColor';
end;

class function TMFColor.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFColorEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFVec4f ------------------------------------------------------------------- }

class function TMFVec4f.X3DType: string;
begin
  Result := 'MFVec4f';
end;

function TMFVec4f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec4f.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec4f).Items, (Value2 as TMFVec4f).Items);
end;

function TMFVec4f.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFColorRGBA --------------------------------------------------------------- }

class function TMFColorRGBA.X3DType: string;
begin
  Result := 'MFColorRGBA';
end;

class function TMFColorRGBA.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFColorRGBAEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFVec2d ------------------------------------------------------------------- }

class function TMFVec2d.X3DType: string;
begin
  Result := 'MFVec2d';
end;

function TMFVec2d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec2d.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec2d).Items, (Value2 as TMFVec2d).Items);
end;

function TMFVec2d.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFVec3d ------------------------------------------------------------------- }

class function TMFVec3d.X3DType: string;
begin
  Result := 'MFVec3d';
end;

function TMFVec3d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec3d.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec3d).Items, (Value2 as TMFVec3d).Items);
end;

function TMFVec3d.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFVec4d ------------------------------------------------------------------- }

class function TMFVec4d.X3DType: string;
begin
  Result := 'MFVec4d';
end;

function TMFVec4d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFVec4d.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFVec4d).Items, (Value2 as TMFVec4d).Items);
end;

function TMFVec4d.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFRotation ---------------------------------------------------------------- }

class function TMFRotation.X3DType: string;
begin
  Result := 'MFRotation';
end;

function TMFRotation.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.List^[ItemNum].ToRawString;
end;

procedure TMFRotation.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFRotation).Items, (Value2 as TMFRotation).Items);
end;

function TMFRotation.CanAssignLerp: boolean;
begin
  Result := true;
end;

{ TMFFloat ------------------------------------------------------------------- }

function TMFFloat.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFFloat.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := Format('%g', [Items[ItemNum]]);
end;

procedure TMFFloat.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFFloat).Items, (Value2 as TMFFloat).Items);
end;

function TMFFloat.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFFloat.X3DType: string;
begin
  Result := 'MFFloat';
end;

function TMFFloat.CreateItemBeforeParse: TSFFloat;
begin
  Result := inherited;
  { Assign our Angle to single item used for parsing.
    This way float values on MFFloat fields will be correctly converted to radians,
    important e.g. for Background.skyAngle,groundAngle. }
  (Result as TSFFloat).Angle := Angle;
end;

{ TMFDouble -------------------------------------------------------------------- }

function TMFDouble.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFDouble.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := Format('%g', [Items[ItemNum]]);
end;

procedure TMFDouble.AssignLerp(const A: Double; Value1, Value2: TX3DField);
begin
  Items.AssignLerp(A, (Value1 as TMFDouble).Items, (Value2 as TMFDouble).Items);
end;

function TMFDouble.CanAssignLerp: boolean;
begin
  Result := true;
end;

class function TMFDouble.X3DType: string;
begin
  Result := 'MFDouble';
end;

{ TMFTime -------------------------------------------------------------------- }

class function TMFTime.X3DType: string;
begin
  Result := 'MFTime';
end;

class function TMFTime.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFTimeEvent.Create(AParentNode, AName, AInEvent);
end;

{ TMFString ------------------------------------------------------------------ }

function TMFString.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  case Encoding of
    xeClassic: Result := StringToX3DClassic(Items[ItemNum]);
    xeXML    : Result := StringToX3DXmlMulti(Items[ItemNum]);
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create('TMFString.RawItemToString Encoding?');
    {$endif}
  end;
end;

class function TMFString.X3DType: string;
begin
  Result := 'MFString';
end;

procedure TMFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);
var
  Lexer: TX3DLexer;
begin
  { For MFString, it's very common that normal parsing fails because
    of missing double quotes, even in models from
    http://www.web3d.org/x3d/content/examples/Basic/
    Although specification clearly says that MFString
    components should always be enclosed within double
    quotes. We just do what Xj3D seems to do, that is
    we handle this as a single string (producing a warning). }

  try
    Lexer := TX3DLexer.CreateForPartialStream(AttributeValue, Reader.Version);
    try
      ParseXMLAttributeLexer(Lexer, Reader);
    finally FreeAndNil(Lexer) end;

    { Surround in try..except both CreateForPartialStream and ParseXMLAttributeLexer,
      as CreateForPartialStream can already cause exception in case of
      demo-models/x3d/test_single_quotes_mfstring.x3d . }
  except
    on E: Exception do
    begin
      WritelnWarning('X3D', 'Error when reading MFString field "' + X3DName + '" value. Possibly missing double quotes (treating as a single string): ' + E.Message);
      Items.Count := 0;
      Items.Add(AttributeValue);
    end;
  end;
end;

function TMFString.SaveToXmlValue: TSaveToXmlMethod;
begin
  Result := sxAttributeCustomQuotes;
end;

procedure TMFString.SaveToStreamValue(Writer: TX3DWriter);
begin
  { MFString in XML encoding is surrounded by single quotes }
  if Writer.Encoding = xeXML then Writer.Write('''');
  inherited;
  if Writer.Encoding = xeXML then Writer.Write('''');
end;

{$endif read_implementation}
