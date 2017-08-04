{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { X3D field with a list of values.
    This is an ancestor of most multiple-value X3D fields.

    @italic(Design note): The MF field classes can keep their items
    in compact lists, e.g. Generics.Collections.TList<T>.
    This allows speed (the TList<T> has trivial cache-friendly layout).
    That's why TX3DSimpleMultField is not a descendant of TX3DSingleFieldsList,
    as then we would have a list of references, which could be quite memory-consuming
    (imagine a milion vertexes in MFVec3f, which is absolutely possible)
    and not cache friendly. }
  generic TX3DSimpleMultField<
    TMF_STATIC_ITEM,
    TMF_CLASS_ITEM,
    TMF_DYN_STATIC_ITEM_ARRAY,
    TMF_EVENT> = class(TX3DMultField)
  private
    InvalidIndexWarnings: Cardinal;
    RawItems: TMF_DYN_STATIC_ITEM_ARRAY;

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
    DefaultValue: TMF_STATIC_ITEM;
    function GetItems: TMF_DYN_STATIC_ITEM_ARRAY;
    procedure SetItems(const Value: TMF_DYN_STATIC_ITEM_ARRAY);
    function GetItemsSafe(Index: Integer): TMF_STATIC_ITEM;
    procedure SetItemsSafe(Index: Integer; const Value: TMF_STATIC_ITEM);
  protected
    { Create new instance of a corresponding single-value field. It doesn't need to
      have initialized value (so it can be created by
      CreateUndefined), since we'll call his Parse method immediately.
      Default implementation in this class uses simply
      calls TMF_CLASS_ITEM.CreateUndefined. }
    function CreateItemBeforeParse: TMF_CLASS_ITEM; virtual;

    { Add Item at the end of RawItems. You should copy
      Item contents as appropriate (remember that Item instance itself
      may be freed soon, so copy contents, not only some reference). }
    procedure RawItemsAdd(Item: TMF_CLASS_ITEM);

    procedure WritelnWarning_InvalidIndex(const Index, ACount: Integer);

    function GetCount: SizeInt; override;
    procedure SetCount(const Value: SizeInt); override;

    { SaveToStreamValue overriden for MF fields. This class handles
      SaveToStreamValue fully, no need to override it again in
      descendants. }
    procedure SaveToStreamValue(Writer: TX3DWriter); override;

    { RawItemToString(i) must change RawItems[i] into a string that can be used to
      store this is text stream. In descendants, you have to override this. }
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; virtual; abstract;

    { This says when we should do newline when writing this field into a stream.
      It's has purely aesthetical meaning, nothing more.
      In this class SaveToStreamDoNewLineAfterRawItem always returns @true
      (although SaveToStreamValue may sometimes ignore it, if it knows better). }
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; virtual;

    procedure Clear;
    function GetCapacity: SizeInt;
    procedure SetCapacity(const Value: SizeInt);
    property Capacity: SizeInt read GetCapacity write SetCapacity;
  public
    constructor Create(AParentNode: TX3DFileItem;
      const AName: string;
      const InitialContent: array of TMF_STATIC_ITEM);
    constructor CreateUndefined(AParentNode: TX3DFileItem;
      const AName: string; const AExposed: boolean); override;
    destructor Destroy; override;

    property Items: TMF_DYN_STATIC_ITEM_ARRAY read GetItems write SetItems;

    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;

    { Parse MF field. This class handles parsing fully, usually no need to
      override this more in descendants. It uses TMF_CLASS_ITEM.Parse method. }
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Checks equality between this and SecondValue field.
      In addition to inherited(Equals), this checks the actual contents
      of the items.
      Note that floating-point values are compared with some tolerance
      for errors by this method. }
    function Equals(SecondValue: TX3DField): boolean; override;
    function EqualsDefaultValue: boolean; override;

    procedure ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Access Items[] checking for range errors.
      In case of errors, Get will return false, Set will do nothing,
      and both will produce clear WritelnWarning. }
    property ItemsSafe[Index: Integer]: TMF_STATIC_ITEM read GetItemsSafe write SetItemsSafe;

    class function CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent; override;

    procedure Send(const AValue: array of TMF_STATIC_ITEM); virtual; overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DSimpleMultField ------------------------------------------------------- }

function TX3DSimpleMultField.CreateItemBeforeParse: TMF_CLASS_ITEM;
begin
  result := TMF_CLASS_ITEM.CreateUndefined(ParentNode, '', false);
end;

procedure TX3DSimpleMultField.ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TMF_CLASS_ITEM;
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
      Capacity := 64;

      while Lexer.Token <> vtCloseSqBracket do
      { we always look now at "]" or next single value }
      begin
        SingleItem.ParseValue(Lexer, Reader);
        RawItemsAdd(SingleItem);

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
      RawItemsAdd(SingleItem);
    end;

  finally FreeAndNil(SingleItem) end;
end;

procedure TX3DSimpleMultField.ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TMF_CLASS_ITEM;
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
    Capacity := 64;

    while Lexer.Token <> vtEnd do
    begin
      SingleItem.ParseValue(Lexer, Reader);
      RawItemsAdd(SingleItem);
    end;
  finally FreeAndNil(SingleItem) end;
end;

procedure TX3DSimpleMultField.SaveToStreamValue(Writer: TX3DWriter);
var
  i: integer;
  WriteIndentNextTime: boolean;
  IndentMultiValueFields: boolean;
begin
  case Writer.Encoding of
    xeClassic:
      { The general "for I := ..." code below can handle correctly any RawItems.Count
        value. But for aesthetics, i.e. more clear output for humans,
        I handle the RawItems.Count = 0 and 1 cases separately. }
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
    else raise EInternalError.Create('TX3DSimpleMultField.SaveToStreamValue Encoding?');
  end;
end;

function TX3DSimpleMultField.SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := true;
end;

function TX3DSimpleMultField.Equals(SecondValue: TX3DField): boolean;
begin
  Result :=
    (inherited Equals(SecondValue)) and
    (SecondValue is TX3DSimpleMultField) and
    Items.Equals(TX3DSimpleMultField(SecondValue).Items);
end;

function TX3DSimpleMultField.EqualsDefaultValue: boolean;
var
  Tmp: TMF_DYN_STATIC_ITEM_ARRAY;
begin
  Result := Count = DefaultValuesCount;

  if Result then
  begin
    if DefaultValuesCount = 1 then
    begin
      Tmp := TMF_DYN_STATIC_ITEM_ARRAY.Create;
      try
        Tmp.Add(DefaultValue);
        Result := Tmp.PerfectlyEquals(Items);
      finally FreeAndNil(Tmp) end;
    end else
    if DefaultValuesCount <> 0 then
      // we cannot check in case DefaultValuesCount >= 2
      Result := false;
  end;
end;

procedure TX3DSimpleMultField.WritelnWarning_InvalidIndex(
  const Index, ACount: Integer);
const
  MaxInvalidIndexWarnings = 10;
begin
  Inc(InvalidIndexWarnings);
  if InvalidIndexWarnings < MaxInvalidIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Invalid index for field %s (%s): index is %d, but we have only %d items', [NiceName, X3DType, Index, ACount])) else
  if InvalidIndexWarnings = MaxInvalidIndexWarnings then
    WritelnWarning('VRML/X3D', Format('Invalid index for field %s (%s) reported for the %dth time. Further warnings regarding this field will not be reported (to avoid wasting time on printing countless warnings...)',
      [NiceName, X3DType, InvalidIndexWarnings]));
end;

constructor TX3DSimpleMultField.CreateUndefined(AParentNode: TX3DFileItem;
  const AName: string; const AExposed: boolean);
begin
  inherited;
  RawItems := TMF_DYN_STATIC_ITEM_ARRAY.Create;
  DefaultValuesCount := -1;
end;

constructor TX3DSimpleMultField.Create(AParentNode: TX3DFileItem; const AName: string;
  const InitialContent: array of TMF_STATIC_ITEM);
begin
  inherited Create(AParentNode, AName);
  Items.AddRange(InitialContent);
  AssignDefaultValueFromValue;
end;

destructor TX3DSimpleMultField.Destroy;
begin
  FreeAndNil(RawItems);
  inherited;
end;

function TX3DSimpleMultField.GetCount: SizeInt;
begin
  Result := RawItems.Count;
end;

procedure TX3DSimpleMultField.SetCount(const Value: SizeInt);
begin
  RawItems.Count := Value;
end;

function TX3DSimpleMultField.GetCapacity: SizeInt;
begin
  Result := RawItems.Capacity;
end;

procedure TX3DSimpleMultField.SetCapacity(const Value: SizeInt);
begin
  RawItems.Capacity := Value;
end;

procedure TX3DSimpleMultField.Clear;
begin
  RawItems.Clear;
end;

function TX3DSimpleMultField.GetItems: TMF_DYN_STATIC_ITEM_ARRAY;
begin
  Result := TMF_DYN_STATIC_ITEM_ARRAY(RawItems)
end;

// TODO: better names for TMF_ TSF_ macros

// TODO: unused?
procedure TX3DSimpleMultField.SetItems(const Value: TMF_DYN_STATIC_ITEM_ARRAY);
begin
  TMF_DYN_STATIC_ITEM_ARRAY(RawItems).Assign(Value);
end;

procedure TX3DSimpleMultField.RawItemsAdd(Item: TMF_CLASS_ITEM);
begin
  Items.Add(Item.Value);
end;

procedure TX3DSimpleMultField.Assign(Source: TPersistent);
begin
  if Source is TX3DSimpleMultField then
  begin
    DefaultValuesCount := TX3DSimpleMultField(Source).DefaultValuesCount;
    DefaultValue       := TX3DSimpleMultField(Source).DefaultValue;
    Items.Assign(TX3DSimpleMultField(Source).Items);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TX3DSimpleMultField.AssignValue(Source: TX3DField);
begin
  if Source is TX3DSimpleMultField then
  begin
    inherited;
    Items.Assign(TX3DSimpleMultField(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TX3DSimpleMultField.AssignDefaultValueFromValue;
begin
  inherited;

  (* initialize default value of the field: DefaultValuesCount, and (only
     in case of DefaultValuesCount = 1 for now) initialize also DefaultValue *)
  case Count of
    0: DefaultValuesCount := 0;
    1: begin
         DefaultValuesCount := 1;
         DefaultValue := Items.L[0];
       end;
    else DefaultValuesCount := -1;
  end;
end;

function TX3DSimpleMultField.GetItemsSafe(Index: Integer): TMF_STATIC_ITEM;
begin
  if (Index >= 0) and (Index < Items.Count) then
    Result := Items.L[Index] else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
    Result := Default(TMF_STATIC_ITEM);
  end;
end;

procedure TX3DSimpleMultField.SetItemsSafe(Index: Integer; const Value: TMF_STATIC_ITEM);
begin
  if (Index >= 0) and (Index < Items.Count) then
    Items.L[Index] := Value else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
  end;
end;

class function TX3DSimpleMultField.CreateEvent(const AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean): TX3DEvent;
begin
  Result := TMF_EVENT.Create(AParentNode, AName, AInEvent);
end;

procedure TX3DSimpleMultField.Send(const AValue: array of TMF_STATIC_ITEM);
// This works in FPC (Delphi and ObjFpc mode), but doesn't in actual Delphi:
// type
//   TMyClass = TX3DSimpleMultField {$ifndef CASTLE_OBJFPC}<...TODO...>{$endif};
//   TMyClassClass = class of TMyClass;
var
  FieldValue: TX3DSimpleMultField;
begin
  { We want to create an instance of ClassType (actual current class type),
    not just an instance of TX3DSimpleMultField<T> (which is only an ancestor
    of other MF field classes). }
  // This works in FPC (Delphi and ObjFpc mode), but doesn't in actual Delphi:
  // FieldValue := TX3DFieldClass(ClassType).Create(ParentNode, X3DName, AValue);

  FieldValue := TX3DSimpleMultField(
    TX3DFieldClass(ClassType).CreateUndefined(ParentNode, X3DName, true));
  try
    FieldValue.Items.AddRange(AValue);
    Send(FieldValue);
  finally FreeAndNil(FieldValue) end;
end;

{$endif read_implementation}
