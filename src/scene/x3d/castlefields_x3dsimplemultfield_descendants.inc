{%MainUnit x3dfields.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

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
  TMFBool = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    boolean,
    TSFBool,
    TBooleanList>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TBooleanList; const Item: TSFBool); override;
    function CreateItemBeforeParse: TSFBool; override;
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFLong = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    Int32,
    TSFLong,
    TInt32List>)
  strict private
    FSaveToStreamLineUptoNegative: boolean;
    WrongVertexIndexWarnings: Integer;
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TInt32List; const Item: TSFLong); override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
    function CreateItemBeforeParse: TSFLong; override;
  public
    { When saving to VRML/X3D classic encoding,
      make newline after each negative value on the list.
      This makes a nice output for fields like IndexedFaceSet.coordIndex. }
    property SaveToStreamLineUptoNegative: boolean
      read FSaveToStreamLineUptoNegative write FSaveToStreamLineUptoNegative
      default false;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

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

  TMFMatrix3f = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TMatrix3,
    TSFMatrix3f,
    TMatrix3List>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TMatrix3List; const Item: TSFMatrix3f); override;
    function CreateItemBeforeParse: TSFMatrix3f; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFMatrix3d = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TMatrix3Double,
    TSFMatrix3d,
    TMatrix3DoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TMatrix3DoubleList; const Item: TSFMatrix3d); override;
    function CreateItemBeforeParse: TSFMatrix3d; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFMatrix4f = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TMatrix4,
    TSFMatrix4f,
    TMatrix4List>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TMatrix4List; const Item: TSFMatrix4f); override;
    function CreateItemBeforeParse: TSFMatrix4f; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFMatrix4d = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TMatrix4Double,
    TSFMatrix4d,
    TMatrix4DoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TMatrix4DoubleList; const Item: TSFMatrix4d); override;
    function CreateItemBeforeParse: TSFMatrix4d; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec2f = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector2,
    TSFVec2f,
    TVector2List>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector2List; const Item: TSFVec2f); override;
    function CreateItemBeforeParse: TSFVec2f; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec3f = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector3,
    TSFVec3f,
    TVector3List>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector3List; const Item: TSFVec3f); override;
    function CreateItemBeforeParse: TSFVec3f; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFColor = class(TMFVec3f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec4f = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector4,
    TSFVec4f,
    TVector4List>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector4List; const Item: TSFVec4f); override;
    function CreateItemBeforeParse: TSFVec4f; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFColorRGBA = class(TMFVec4f)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec2d = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector2Double,
    TSFVec2d,
    TVector2DoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector2DoubleList; const Item: TSFVec2d); override;
    function CreateItemBeforeParse: TSFVec2d; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec3d = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector3Double,
    TSFVec3d,
    TVector3DoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector3DoubleList; const Item: TSFVec3d); override;
    function CreateItemBeforeParse: TSFVec3d; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFVec4d = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector4Double,
    TSFVec4d,
    TVector4DoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector4DoubleList; const Item: TSFVec4d); override;
    function CreateItemBeforeParse: TSFVec4d; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFRotation = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    TVector4,
    TSFRotation,
    TVector4List>)
  strict protected
    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TVector4List; const Item: TSFRotation); override;
    function CreateItemBeforeParse: TSFRotation; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFFloat = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    Single,
    TSFFloat,
    TSingleList>)
  strict private
    FAngle: boolean;
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TSingleList; const Item: TSFFloat); override;
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
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFDouble = class({$ifdef FPC}specialize{$endif} TX3DSimpleMultField<
    Double,
    TSFDouble,
    TDoubleList>)
  strict protected
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; override;
    procedure AddToList(const ItemList: TDoubleList; const Item: TSFDouble); override;
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; override;
    function CreateItemBeforeParse: TSFDouble; override;
  public
    procedure AssignLerp(const A: Double; Value1, Value2: TX3DField); override;
    function CanAssignLerp: boolean; override;
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFTime = class(TMFDouble)
  public
    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
  end;

  TMFString = class(TX3DMultField)
  strict private
    WarningDoneInvalidIndex: boolean;

    FItems: TCastleStringList;

    { DefaultValuesCount may have three valid values (for now):
      -1 (means "no default value for this field")
      0 (means "default value of this field is empty")
      1 (means "default value of this field is one-item array with DefaultValue").

      So it's not possible to express default values with more
      than one item. That's OK, because nodes from VRML 1.0 and 2.0 specifications
      never have such field (and VRML 2.0 prototypes (that have user-defined
      default field values) actually don't need it). So, for now, more flexible
      DefaultValuesCount is not needed.

      CreateUndefined sets DefaultValuesCount to -1. }
    DefaultValuesCount: Integer;
    DefaultValue: string;

    procedure SetItems(const Value: TCastleStringList);
    function GetItemsSafe(Index: Integer): string;
    procedure SetItemsSafe(Index: Integer; const Value: string);
  strict protected
    procedure Clear;
    function GetCount: TListSize; override;
    procedure SetCount(const Value: TListSize); override;

    function RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
    procedure AddToList(const ItemList: TCastleStringList; const Item: TSFString);
    procedure SaveToStreamValue(Writer: TX3DWriter); override;
    function CreateItemBeforeParse: TSFString;

    procedure WritelnWarning_InvalidIndex(const Index, ACount: Integer);
  public
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const InitialContent: array of string);

    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); override;
    destructor Destroy; override;
    function FastEqualsValue(SecondValue: TX3DField): boolean; override;

    { Parse MF field. This class handles parsing fully, usually no need to
      override this more in descendants. It uses TSingleField.Parse method. }
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    class function X3DType: string; override;
    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;
    procedure ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader); override;
    function SaveToXmlValue: TSaveToXmlMethod; override;

    property Items: TCastleStringList read FItems write SetItems;
    property ItemsSafe[Index: Integer]: string read GetItemsSafe write SetItemsSafe;

    { Copy another field value, default value and other properties. }
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure UnassignDefaultValue; override;

    { Checks equality between this and SecondValue field.
      In addition to inherited(Equals), this checks the actual contents
      of the items.
      Note that floating-point values are compared with some tolerance
      for errors by this method. }
    function Equals(SecondValue: TX3DField): boolean; override;
    function EqualsDefaultValue: boolean; override;

    { Set the field's value in a correct way (by sending X3D event, or at least notifying
      the parent scene). }
    procedure Send(const AValue: array of string); overload;
    procedure Send(const AValue: TCastleStringList); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TMFBool ------------------------------------------------------------------ }

function TMFBool.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := BoolKeywords[Encoding][Items[ItemNum]];
end;

procedure TMFBool.AddToList(const ItemList: TBooleanList; const Item: TSFBool);
begin
  Items.Add(Item.Value);
end;

class function TMFBool.X3DType: string;
begin
  Result := 'MFBool';
end;

class function TMFBool.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFBoolEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFBool.CreateItemBeforeParse: TSFBool;
begin
  Result := TSFBool.CreateUndefined(ParentNode, false, '');
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

procedure TMFLong.AddToList(const ItemList: TInt32List; const Item: TSFLong);
begin
  Items.Add(Item.Value);
end;

class function TMFLong.X3DType: string;
begin
  Result := 'MFLong';
end;

class function TMFLong.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFLongEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFLong.CreateItemBeforeParse: TSFLong;
begin
  Result := TSFLong.CreateUndefined(ParentNode, false, '');
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
      [GeometryX3DType, VertexNum, CoordCount]))
  else
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

class function TMFMatrix3f.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFMatrix3fEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFMatrix3f.CreateItemBeforeParse: TSFMatrix3f;
begin
  Result := TSFMatrix3f.CreateUndefined(ParentNode, false, '');
end;

function TMFMatrix3f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFMatrix3f.AddToList(const ItemList: TMatrix3List; const Item: TSFMatrix3f);
begin
  Items.Add(Item.Value);
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

class function TMFMatrix3d.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFMatrix3dEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFMatrix3d.CreateItemBeforeParse: TSFMatrix3d;
begin
  Result := TSFMatrix3d.CreateUndefined(ParentNode, false, '');
end;

function TMFMatrix3d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFMatrix3d.AddToList(const ItemList: TMatrix3DoubleList; const Item: TSFMatrix3d);
begin
  Items.Add(Item.Value);
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

class function TMFMatrix4f.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFMatrix4fEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFMatrix4f.CreateItemBeforeParse: TSFMatrix4f;
begin
  Result := TSFMatrix4f.CreateUndefined(ParentNode, false, '');
end;

function TMFMatrix4f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFMatrix4f.AddToList(const ItemList: TMatrix4List; const Item: TSFMatrix4f);
begin
  Items.Add(Item.Value);
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

class function TMFMatrix4d.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFMatrix4dEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFMatrix4d.CreateItemBeforeParse: TSFMatrix4d;
begin
  Result := TSFMatrix4d.CreateUndefined(ParentNode, false, '');
end;

function TMFMatrix4d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFMatrix4d.AddToList(const ItemList: TMatrix4DoubleList; const Item: TSFMatrix4d);
begin
  Items.Add(Item.Value);
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

class function TMFVec2f.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec2fEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec2f.CreateItemBeforeParse: TSFVec2f;
begin
  Result := TSFVec2f.CreateUndefined(ParentNode, false, '');
end;

function TMFVec2f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec2f.AddToList(const ItemList: TVector2List; const Item: TSFVec2f);
begin
  Items.Add(Item.Value);
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

class function TMFVec3f.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec3fEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec3f.CreateItemBeforeParse: TSFVec3f;
begin
  Result := TSFVec3f.CreateUndefined(ParentNode, false, '');
end;

function TMFVec3f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec3f.AddToList(const ItemList: TVector3List; const Item: TSFVec3f);
begin
  Items.Add(Item.Value);
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

class function TMFVec4f.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec4fEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec4f.CreateItemBeforeParse: TSFVec4f;
begin
  Result := TSFVec4f.CreateUndefined(ParentNode, false, '');
end;

function TMFVec4f.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec4f.AddToList(const ItemList: TVector4List; const Item: TSFVec4f);
begin
  Items.Add(Item.Value);
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

class function TMFVec2d.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec2dEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec2d.CreateItemBeforeParse: TSFVec2d;
begin
  Result := TSFVec2d.CreateUndefined(ParentNode, false, '');
end;

function TMFVec2d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec2d.AddToList(const ItemList: TVector2DoubleList; const Item: TSFVec2d);
begin
  Items.Add(Item.Value);
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

class function TMFVec3d.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec3dEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec3d.CreateItemBeforeParse: TSFVec3d;
begin
  Result := TSFVec3d.CreateUndefined(ParentNode, false, '');
end;

function TMFVec3d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec3d.AddToList(const ItemList: TVector3DoubleList; const Item: TSFVec3d);
begin
  Items.Add(Item.Value);
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

class function TMFVec4d.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFVec4dEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFVec4d.CreateItemBeforeParse: TSFVec4d;
begin
  Result := TSFVec4d.CreateUndefined(ParentNode, false, '');
end;

function TMFVec4d.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFVec4d.AddToList(const ItemList: TVector4DoubleList; const Item: TSFVec4d);
begin
  Items.Add(Item.Value);
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

class function TMFRotation.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFRotationEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFRotation.CreateItemBeforeParse: TSFRotation;
begin
  Result := TSFRotation.CreateUndefined(ParentNode, false, '');
end;

function TMFRotation.RawItemToString(ItemNum: Integer; const Encoding: TX3DEncoding): string;
begin
  Result := Items.L[ItemNum].ToRawString;
end;

procedure TMFRotation.AddToList(const ItemList: TVector4List; const Item: TSFRotation);
begin
  Items.Add(Item.Value);
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
  Result := FormatDot('%g', [Items[ItemNum]]);
end;

procedure TMFFloat.AddToList(const ItemList: TSingleList; const Item: TSFFloat);
begin
  Items.Add(Item.Value);
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

class function TMFFloat.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFFloatEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFFloat.CreateItemBeforeParse: TSFFloat;
begin
  Result := TSFFloat.CreateUndefined(ParentNode, false, '');

  { Assign our Angle to single item used for parsing.
    This way float values on MFFloat fields will be correctly converted to radians,
    important e.g. for Background.skyAngle,groundAngle. }
  Result.Angle := Angle;
end;

{ TMFDouble -------------------------------------------------------------------- }

function TMFDouble.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := false;
end;

function TMFDouble.RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string;
begin
  Result := FormatDot('%g', [Items[ItemNum]]);
end;

procedure TMFDouble.AddToList(const ItemList: TDoubleList; const Item: TSFDouble);
begin
  Items.Add(Item.Value);
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

class function TMFDouble.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFDoubleEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFDouble.CreateItemBeforeParse: TSFDouble;
begin
  Result := TSFDouble.CreateUndefined(ParentNode, false, '');
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

function TMFString.FastEqualsValue(SecondValue: TX3DField): boolean;

  function StringsEqual(const S1, S2: TStrings): Boolean;
  var
    I: Integer;
  begin
    Result := S1.Count = S2.Count;
    if Result then
      for I := 0 to S1.Count - 1 do
        if S1[I] <> S2[I] then
          Exit(false);
  end;

begin
  { It is important to implement useful FastEqualsValue for MFString,
    not just return false (as most multi-value fields do) because
    MFString change happens when you do SetUrl.
    It's useful that TImageTexture.SetUrl multiple times with the same URL
    does nothing. See:

    https://github.com/castle-engine/castle-engine/issues/304
    https://gist.github.com/michaliskambi/876992f8fbc04ac869d8994d520383b4
  }
  Result := (SecondValue is TMFString) and
    StringsEqual(Items, TMFString(SecondValue).Items);
end;

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

procedure TMFString.AddToList(const ItemList: TCastleStringList; const Item: TSFString);
begin
  Items.Add(Item.Value);
end;

class function TMFString.X3DType: string;
begin
  Result := 'MFString';
end;

class function TMFString.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMFStringEvent.Create(AParentNode, AName, AInEvent);
end;

function TMFString.CreateItemBeforeParse: TSFString;
begin
  Result := TSFString.CreateUndefined(ParentNode, false, '');
end;

procedure TMFString.ParseXMLAttribute(const AttributeValue: string; Reader: TX3DReader);

  { TODO: Copy-paste of TX3DSimpleMultField.ParseXMLAttributeLexer }
  procedure ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader);
  var
    SingleItem: TSFString;
  begin
    { This is much easier and simpler in XML encoding than it was
      in classic encoding. We don't have to check for [ and ] tokens,
      comma is ignored (it was only for VRML 1.0 anyway), we just read
      single values up to the end of stream. }

    Clear;

    SingleItem := CreateItemBeforeParse;
    try
      { List size may be increased rapidly during parsing.
        Prepare for it by allocating some size in advance. }
      //Capacity := 64;

      while Lexer.Token <> vtEnd do
      begin
        SingleItem.ParseValue(Lexer, Reader);
        AddToList(Items, SingleItem);
      end;
    finally FreeAndNil(SingleItem) end;
  end;

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

  function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
  begin
    Result := true;
  end;

  { TODO: Copy-paste of TX3DSimpleMultField.SaveToStreamValue }
  procedure SaveToStreamValueCore(Writer: TX3DWriter);
  var
    i: integer;
    WriteIndentNextTime: boolean;
    IndentMultiValueFields: boolean;
  begin
    case Writer.Encoding of
      xeClassic:
        { The general "for I := ..." code below can handle correctly any FItems.Count
          value. But for aesthetics, i.e. more clear output for humans,
          I handle the FItems.Count = 0 and 1 cases separately. }
        case Count of
          0: Writer.Write('[]');
          1: Writer.Write(RawItemToString(0, Writer.Encoding));
          else
            begin
              Writer.Writeln('[');
              Writer.IncIndent;

              { For really long fields, writing indentation before each item
                can cost a significant disk space. So do not indent when
                there are many items. }
              IndentMultiValueFields := Count <= 10;

              WriteIndentNextTime := IndentMultiValueFields;
              for i := 0 to Count-1 do
              begin
                if WriteIndentNextTime then Writer.WriteIndent('');
                Writer.Write(RawItemToString(i, Writer.Encoding) +',');
                { After the last item we always write newline,
                  no matter what's SaveToStreamDoNewLineAfterRawItem }
                if (i = Count - 1) or
                   SaveToStreamDoNewLineAfterRawItem(i) then
                  begin Writer.Writeln; WriteIndentNextTime := IndentMultiValueFields end else
                  begin Writer.Write(' '); WriteIndentNextTime := false; end;
              end;

              Writer.DecIndent;
              Writer.WriteIndent(']');
            end;
        end;
      xeXML:
        for I := 0 to Count - 1 do
        begin
          Writer.Write(RawItemToString(I, Writer.Encoding));
          if I <> Count - 1 then
            Writer.Write(' ');
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TX3DSimpleMultField.SaveToStreamValue Encoding?');
      {$endif}
    end;
  end;

begin
  { MFString in XML encoding is surrounded by single quotes }
  if Writer.Encoding = xeXML then Writer.Write('''');
  SaveToStreamValueCore(Writer);
  if Writer.Encoding = xeXML then Writer.Write('''');
end;

procedure TMFString.SetItems(const Value: TCastleStringList);
begin
  FItems.Assign(Value);
end;

procedure TMFString.Send(const AValue: array of string);
var
  FieldValue: TMFString;
begin
  if SendsByEvent then
  begin
    { We want to create an instance of ClassType (actual current class type),
      not just an instance of TMFString (which is only an ancestor
      of other MF field classes). }
    // This works in FPC (Delphi and ObjFpc mode), but doesn't in actual Delphi:
    // FieldValue := TX3DFieldClass(ClassType).Create(ParentNode, false, X3DName, AValue);

    { AExposed = false below, because not needed otherwise. }
    FieldValue := TMFString(
      TX3DFieldClass(ClassType).CreateUndefined(ParentNode, false, X3DName));
    try
      FieldValue.Items.AddRange(AValue);
      Send(FieldValue);
    finally FreeAndNil(FieldValue) end;
  end else
  begin
    { When SendsByEvent = false, we can optimize this method.
      No need to create an intermediate FieldValue instance
      (and assign AValue to FieldValue, just to later assign FieldValue to Items). }
    ValueFromIsClause := false;
    Items.Assign(AValue);
    Changed;
  end;
end;

procedure TMFString.Send(const AValue: TCastleStringList);
var
  FieldValue: TMFString;
begin
  { This is a copy-paste of implementation of Send(const AValue: array of TItem) }
  if SendsByEvent then
  begin
    FieldValue := TMFString(
      TX3DFieldClass(ClassType).CreateUndefined(ParentNode, false, X3DName));
    try
      FieldValue.Items.Assign(AValue);
      Send(FieldValue);
    finally FreeAndNil(FieldValue) end;
  end else
  begin
    ValueFromIsClause := false;
    Items.Assign(AValue);
    Changed;
  end;
end;

constructor TMFString.CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  inherited;
  FItems := TCastleStringList.Create;
  DefaultValuesCount := -1;
end;

constructor TMFString.Create(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string;
  const InitialContent: array of string);
begin
  inherited Create(AParentNode, AExposed, AName);
  Items.AddRange(InitialContent);
  AssignDefaultValueFromValue;
end;

destructor TMFString.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TMFString.Clear;
begin
  FItems.Clear;
end;

function TMFString.GetCount: TListSize;
begin
  Result := FItems.Count;
end;

procedure TMFString.SetCount(const Value: TListSize);
begin
  FItems.Count := Value;
end;

procedure TMFString.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TSFString;
begin
  Clear;

  SingleItem := nil;
  try
    SingleItem := CreateItemBeforeParse;

    if Lexer.Token = vtOpenSqBracket then
    begin
      Lexer.NextToken;

      { List size may be increased rapidly during parsing.
        Prepare for it by allocating some size in advance. }
      FItems.Capacity := 64;

      while Lexer.Token <> vtCloseSqBracket do
      { we always look now at "]" or next single value }
      begin
        SingleItem.ParseValue(Lexer, Reader);
        AddToList(Items, SingleItem);

        if Lexer.Token = vtCloseSqBracket then break;

        if Lexer.Version.Major < 2 then
        begin
          Lexer.CheckTokenIs(vtComma);
          Lexer.NextToken;
        end;
      end;

      { Our handling of commas is specified by VRML 1.0 spec:
        - When the list has no items, "[]" is allowed but "[,]" is not.
        - When there are some items on the list, the last item *may*
          be followed by a comma.
        For VRML >= 2.0 this all doesn't matter, comma is just a whitespace
        and Lexer will never return such token. }

      Lexer.NextToken;
    end else
    begin
      { one single field - not enclosed in [] brackets }
      SingleItem.ParseValue(Lexer, Reader);
      AddToList(Items, SingleItem);
    end;

  finally FreeAndNil(SingleItem) end;
end;

procedure TMFString.Assign(Source: TPersistent);
begin
  if Source is TMFString then
  begin
    DefaultValuesCount := TMFString(Source).DefaultValuesCount;
    DefaultValue       := TMFString(Source).DefaultValue;
    Items.Assign(TMFString(Source).Items);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TMFString.AssignValue(Source: TX3DField);
begin
  if Source is TMFString then
  begin
    inherited;
    Items.Assign(TMFString(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TMFString.AssignDefaultValueFromValue;
begin
  inherited;

  (* initialize default value of the field: DefaultValuesCount, and (only
     in case of DefaultValuesCount = 1 for now) initialize also DefaultValue *)
  case Count of
    0: DefaultValuesCount := 0;
    1: begin
         DefaultValuesCount := 1;
         DefaultValue := Items[0];
       end;
    else DefaultValuesCount := -1;
  end;
end;

procedure TMFString.UnassignDefaultValue;
begin
  DefaultValuesCount := -1;
end;

function TMFString.Equals(SecondValue: TX3DField): boolean;
begin
  Result := (inherited Equals(SecondValue)) and (SecondValue is TMFString) and
    Items.Equals(TMFString(SecondValue).Items);
end;

function TMFString.EqualsDefaultValue: boolean;
var
  Tmp: TCastleStringList;
begin
  Result := Count = DefaultValuesCount;

  if Result then
  begin
    if DefaultValuesCount = 1 then
    begin
      Tmp := TCastleStringList.Create;
      try
        Tmp.Add(DefaultValue);
        Result := Tmp.PerfectlyEquals(Items);
      finally
        FreeAndNil(Tmp)
      end;
    end else
    if DefaultValuesCount <> 0 then
      // we cannot check in case DefaultValuesCount >= 2
      Result := false;
  end;
end;

function TMFString.GetItemsSafe(Index: Integer): string;
begin
  if (Index >= 0) and (Index < Items.Count) then
    //Result := Items.{$ifdef FPC}L[Index]{$else}InternalGetItem(Index){$endif} else
    Result := Items.L[Index] else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
    Result := Default(string);
  end;
end;

procedure TMFString.SetItemsSafe(Index: Integer; const Value: string);
begin
  if (Index >= 0) and (Index < Items.Count) then
    Items.L[Index] := Value else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
  end;
end;

procedure TMFString.WritelnWarning_InvalidIndex(const Index, ACount: Integer);
begin
  if not WarningDoneInvalidIndex then
  begin
    WarningDoneInvalidIndex := true;
    WritelnWarning('X3D', Format('Invalid index %d (should be less than count %d) for field %s (type: %s)', [
      Index,
      ACount,
      NiceName,
      X3DType
    ]));
  end;
end;

{$endif read_implementation}
