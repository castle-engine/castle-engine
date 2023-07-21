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


  { X3D field with a list of values.
    This is an ancestor of most multiple-value X3D fields. }
  {$ifdef FPC}generic{$endif}
  TX3DSimpleMultField<
    TItem;
    TSingleField{$ifndef FPC}: TX3DSingleField{$endif};
    TItemList{$ifndef FPC}: TStructList<TItem>, constructor{$endif}> = class(TX3DMultField)
  strict private
    WarningDoneInvalidIndex: boolean;

    { Items are kept in a TItemList, that may be an efficient
      Generics.Collections.TList<T>.
      This allows speed (the TList<T> has trivial cache-friendly layout).
      That's why TX3DSimpleMultField is not a descendant of TX3DSingleFieldsList,
      as then we would have a list of references, which could be quite memory-consuming
      (imagine a milion vertexes in MFVec3f, which is absolutely possible)
      and not cache friendly. }
    FItems: TItemList;

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
    DefaultValue: TItem;
    function GetItems: TItemList;
    procedure SetItems(const Value: TItemList);
    function GetItemsSafe(Index: Integer): TItem;
    procedure SetItemsSafe(Index: Integer; const Value: TItem);
  strict protected
    { Create new instance of a corresponding single-value field. It doesn't need to
      have initialized value (so it can be created by
      CreateUndefined), since we'll call his Parse method immediately.
      Default implementation in this class uses simply
      calls TSingleField.CreateUndefined. }
    function CreateItemBeforeParse: TSingleField; virtual; abstract;
    procedure AddToList(const ItemList: TItemList; const Item: TSingleField); virtual; abstract;

    procedure WritelnWarning_InvalidIndex(const Index, ACount: Integer);

    function GetCount: TListSize; override;
    procedure SetCount(const Value: TListSize); override;

    { SaveToStreamValue overriden for MF fields. This class handles
      SaveToStreamValue fully, no need to override it again in
      descendants. }
    procedure SaveToStreamValue(Writer: TX3DWriter); override;

    { RawItemToString(i) must change FItems[i] into a string that can be used to
      store this is text stream. In descendants, you have to override this. }
    function RawItemToString(ItemNum: integer; const Encoding: TX3DEncoding): string; virtual; abstract;

    { This says when we should do newline when writing this field into a stream.
      It's has purely aesthetical meaning, nothing more.
      In this class SaveToStreamDoNewLineAfterRawItem always returns @true
      (although SaveToStreamValue may sometimes ignore it, if it knows better). }
    function SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean; virtual;

    procedure Clear;
    function GetCapacity: TListSize;
    procedure SetCapacity(const Value: TListSize);
    property Capacity: TListSize read GetCapacity write SetCapacity;
  public
    constructor Create(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string;
      const InitialContent: array of TItem);
    constructor CreateUndefined(const AParentNode: TX3DFileItem;
      const AExposed: boolean; const AName: string); override;
    destructor Destroy; override;

    property Items: TItemList read GetItems write SetItems;

    { Copy another field value, default value and other properties. }
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(Source: TX3DField); override;
    procedure AssignDefaultValueFromValue; override;
    procedure UnassignDefaultValue; override;

    { Parse MF field. This class handles parsing fully, usually no need to
      override this more in descendants. It uses TSingleField.Parse method. }
    procedure ParseValue(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Checks equality between this and SecondValue field.
      In addition to inherited(Equals), this checks the actual contents
      of the items.
      Note that floating-point values are compared with some tolerance
      for errors by this method. }
    function Equals(SecondValue: TX3DField): boolean; override;
    function EqualsDefaultValue: boolean; override;

    procedure ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader); override;

    { Access items, making nice warning in case of an invalid index.
      In case of an invalid index,
      getting will return default value (filled with zeroes),
      setting will do nothing,
      and both will emit user-friendly WritelnWarning. }
    property ItemsSafe[Index: Integer]: TItem read GetItemsSafe write SetItemsSafe;

    { Set the field's value in a correct way (by sending X3D event, or at least notifying
      the parent scene). }
    procedure Send(const AValue: array of TItem); overload;
    procedure Send(const AValue: TItemList); overload;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DSimpleMultField ------------------------------------------------------- }

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  ParseValue(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TSingleField;
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

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  ParseXMLAttributeLexer(Lexer: TX3DLexer; Reader: TX3DReader);
var
  SingleItem: TSingleField;
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
      AddToList(Items, SingleItem);
    end;
  finally FreeAndNil(SingleItem) end;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SaveToStreamValue(Writer: TX3DWriter);
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

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SaveToStreamDoNewLineAfterRawItem(ItemNum: integer): boolean;
begin
  Result := true;
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Equals(SecondValue: TX3DField): boolean;
begin
  Result :=
    (inherited Equals(SecondValue)) and
    (SecondValue is TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField, TItemList>{$endif}) and
    Items.Equals(TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField, TItemList>{$endif}(SecondValue).Items);
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  EqualsDefaultValue: boolean;
var
  Tmp: TItemList;
begin
  Result := Count = DefaultValuesCount;

  if Result then
  begin
    if DefaultValuesCount = 1 then
    begin
      Tmp := TItemList.Create;
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

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  WritelnWarning_InvalidIndex(
  const Index, ACount: Integer);
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

constructor TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  CreateUndefined(const AParentNode: TX3DFileItem;
  const AExposed: boolean; const AName: string);
begin
  inherited;
  FItems := TItemList.Create;
  DefaultValuesCount := -1;
end;

constructor TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Create(
  const AParentNode: TX3DFileItem; const AExposed: boolean; const AName: string;
  const InitialContent: array of TItem);
begin
  inherited Create(AParentNode, AExposed, AName);
  Items.AddRange(InitialContent);
  AssignDefaultValueFromValue;
end;

destructor TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  GetCount: TListSize;
begin
  Result := FItems.Count;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SetCount(const Value: TListSize);
begin
  FItems.Count := Value;
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  GetCapacity: TListSize;
begin
  Result := FItems.Capacity;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SetCapacity(const Value: TListSize);
begin
  FItems.Capacity := Value;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Clear;
begin
  FItems.Clear;
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  GetItems: TItemList;
begin
  Result := TItemList(FItems);
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SetItems(const Value: TItemList);
begin
  TItemList(FItems).Assign(Value);
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Assign(Source: TPersistent);
begin
  if Source is TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
    TItemList>{$endif} then
  begin
    DefaultValuesCount := TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
      TItemList>{$endif}(Source).DefaultValuesCount;
    DefaultValue       := TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
      TItemList>{$endif}(Source).DefaultValue;
    Items.Assign(TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
      TItemList>{$endif}(Source).Items);
    VRMLFieldAssignCommon(TX3DField(Source));
  end else
    inherited;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  AssignValue(Source: TX3DField);
begin
  if Source is TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
    TItemList>{$endif} then
  begin
    inherited;
    Items.Assign(TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
    TItemList>{$endif}(Source).Items);
  end else
    AssignValueRaiseInvalidClass(Source);
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  AssignDefaultValueFromValue;
begin
  inherited;

  (* initialize default value of the field: DefaultValuesCount, and (only
     in case of DefaultValuesCount = 1 for now) initialize also DefaultValue *)
  case Count of
    0: DefaultValuesCount := 0;
    1: begin
         DefaultValuesCount := 1;
         DefaultValue := Items{$ifdef FPC}[0]{$else}.InternalGetItem(0){$endif};
       end;
    else DefaultValuesCount := -1;
  end;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  UnassignDefaultValue;
begin
  inherited;
  DefaultValuesCount := -1;
end;

function TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  GetItemsSafe(Index: Integer): TItem;
begin
  if (Index >= 0) and (Index < Items.Count) then
    Result := Items.{$ifdef FPC}L[Index]{$else}InternalGetItem(Index){$endif} else
  begin
    WritelnWarning_InvalidIndex(Index, Count);
    Result := Default(TItem);
  end;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  SetItemsSafe(Index: Integer; const Value: TItem);
begin
  if (Index >= 0) and (Index < Items.Count) then
    {$ifdef FPC}
    Items.L[Index] := Value else
    {$else}
    Items.InternalSetItem(Index, Value) else
    {$endif}
  begin
    WritelnWarning_InvalidIndex(Index, Count);
  end;
end;

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Send(const AValue: array of TItem);
// This works in FPC (Delphi and ObjFpc mode), but doesn't in actual Delphi:
// type
//   TMyClass = TX3DSimpleMultField {$ifndef FPC}<...>{$endif};
//   TMyClassClass = class of TMyClass;
var
  FieldValue: TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
    TItemList>{$endif};
begin
  if SendsByEvent then
  begin
    { We want to create an instance of ClassType (actual current class type),
      not just an instance of TX3DSimpleMultField<T> (which is only an ancestor
      of other MF field classes). }
    // This works in FPC (Delphi and ObjFpc mode), but doesn't in actual Delphi:
    // FieldValue := TX3DFieldClass(ClassType).Create(ParentNode, false, X3DName, AValue);

    { AExposed = false below, because not needed otherwise. }
    FieldValue := TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
      TItemList>{$endif}(
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

procedure TX3DSimpleMultField{$ifndef FPC} <
    TItem;
    TSingleField;
    TItemList
  > {$endif} .
  Send(const AValue: TItemList);
var
  FieldValue: TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
    TItemList>{$endif};
begin
  { This is a copy-paste of implementation of Send(const AValue: array of TItem) }
  if SendsByEvent then
  begin
    FieldValue := TX3DSimpleMultField{$ifndef FPC}<TItem, TSingleField,
      TItemList>{$endif}(
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

{$endif read_implementation}
