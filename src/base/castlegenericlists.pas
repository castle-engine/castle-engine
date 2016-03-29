{ Generic list of any type (TGenericStructList) and map with keys of any type
  (TGenericStructMap).

  This unit is ostensibly done to avoid bug
  http://bugs.freepascal.org/view.php?id=15480 with unit FGL.

  Based on FPC FGL unit, copyright by FPC team.
  License of FPC RTL is the same as our engine (modified LGPL,
  see COPYING.txt for details).
  Fixed to compile also under FPC 2.4.0 and 2.2.4.
  Some small comfortable methods added. }
unit CastleGenericLists;

{$I castleconf.inc}

{$ifdef VER2_2} {$define OldSyntax} {$endif}
{$ifdef VER2_4} {$define OldSyntax} {$endif}

{$define HAS_ENUMERATOR}
{$ifdef VER2_2} {$undef HAS_ENUMERATOR} {$endif}
{$ifdef VER2_4_0} {$undef HAS_ENUMERATOR} {$endif}
{ Just undef enumerator always, in FPC 2.7.1 it's either broken
  or I shouldn't overuse TFPGListEnumeratorSpec. }
{$undef HAS_ENUMERATOR}

{ FPC < 2.6.0 had buggy version of the Extract function,
  also with different interface, see http://bugs.freepascal.org/view.php?id=19960. }
{$define HAS_EXTRACT}
{$ifdef VER2_2} {$undef HAS_EXTRACT} {$endif}
{$ifdef VER2_4} {$undef HAS_EXTRACT} {$endif}

interface

uses FGL;

type
  { Generic list of types that are compared by CompareByte.

    This is equivalent to TFPGList, except it doesn't override IndexOf,
    so your type doesn't need to have a "=" operator built-in inside FPC.
    When calling IndexOf or Remove, it will simply compare values using
    CompareByte, this is what TFPSList.IndexOf uses.
    This way it works to create lists of records, vectors (constant size arrays),
    old-style TP objects, and also is suitable to create a list of methods
    (since for methods, the "=" is broken, for Delphi compatibility,
    see http://bugs.freepascal.org/view.php?id=9228).

    We also add some trivial helper methods like @link(Add) and @link(L). }
  generic TGenericStructList<T> = class(TFPSList)
  private
    type
      TCompareFunc = function(const Item1, Item2: T): Integer;
      TTypeList = array[0..MaxGListSize] of T;
      PTypeList = ^TTypeList;
      PT = ^T;
  {$ifdef HAS_ENUMERATOR} TFPGListEnumeratorSpec = specialize TFPGListEnumerator<T>; {$endif}
    {$ifndef OldSyntax}protected var{$else}var protected{$endif}
      FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    function  Get(Index: Integer): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    constructor Create;
    function Add(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    {$ifdef HAS_EXTRACT} function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif} {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    {$ifdef HAS_ENUMERATOR} function GetEnumerator: TFPGListEnumeratorSpec; {$ifdef CLASSESINLINE} inline; {$endif} {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
{$ifndef OldSyntax}
    procedure Assign(Source: TGenericStructList);
{$endif OldSyntax}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;

    { Pointer to items. Exactly like @link(List), but this points to a single item,
      which means you can access particular item by @code(L[I]) instead of
      @code(List^[I]) in FPC objfpc mode.

      This is just trivial shortcut,  but we use direct access a @italic(lot)
      for structures. Reasons: using Items[] default
      property means copying the structures, which is
      @orderedList(
        @item(very dangerous (you can trivially easy modify a temporary result))
        @item(slow (important for us, since these are used for vector arrays that
         are crucial for renderer and various processing).)
      ) }
    function L: PT;

    { Increase Count and return pointer to new item.
      Comfortable and efficient way to add a new item that you want to immediately
      initialize. }
    function Add: PT;

    { Pointer to ith item. }
    function Ptr(I: Integer): PT;
  end;

  { Generic map of types, with keys compared by CompareByte.

    This is equivalent to TFPGMap, except our KeyCompare doesn't
    use < or > or even = operators, instead it compares by CompareByte.
    So it works with types that do not have built-in < or > or even = operator
    in FPC, like records or class instances.

    See also http://bugs.freepascal.org/view.php?id=15480 . }
  generic TGenericStructMap<TKey, TData> = class(TFPSMap)
  private
    type
      TKeyCompareFunc = function(const Key1, Key2: TKey): Integer;
      TDataCompareFunc = function(const Data1, Data2: TData): Integer;
    {$ifndef OldSyntax}protected var{$else}var protected{$endif}
      FOnKeyCompare: TKeyCompareFunc;
      {$ifndef OldSyntax} FOnDataCompare: TDataCompareFunc; {$endif}
    procedure CopyItem(Src, Dest: Pointer); override;
    procedure CopyKey(Src, Dest: Pointer); override;
    procedure CopyData(Src, Dest: Pointer); override;
    procedure Deref(Item: Pointer); override;
    procedure InitOnPtrCompare; override;
    function GetKey(Index: Integer): TKey; {$ifdef CLASSESINLINE} inline; {$endif}
    function GetKeyData(const AKey: TKey): TData; {$ifdef CLASSESINLINE} inline; {$endif}
    function GetData(Index: Integer): TData; {$ifdef CLASSESINLINE} inline; {$endif}
    function KeyCompare(Key1, Key2: Pointer): Integer;
    function KeyCustomCompare(Key1, Key2: Pointer): Integer;
    {$ifndef OldSyntax}
    //function DataCompare(Data1, Data2: Pointer): Integer;
    function DataCustomCompare(Data1, Data2: Pointer): Integer;
    {$endif}
    procedure PutKey(Index: Integer; const NewKey: TKey); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure PutKeyData(const AKey: TKey; const NewData: TData); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure PutData(Index: Integer; const NewData: TData); {$ifdef CLASSESINLINE} inline; {$endif}
    procedure SetOnKeyCompare(NewCompare: TKeyCompareFunc);
    {$ifndef OldSyntax}
    procedure SetOnDataCompare(NewCompare: TDataCompareFunc);
    {$endif}
  public
    constructor Create;
    function Add(const AKey: TKey; const AData: TData): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Add(const AKey: TKey): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Find(const AKey: TKey; out Index: Integer): Boolean; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const AKey: TKey): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOfData(const AData: TData): Integer;
    procedure InsertKey(Index: Integer; const AKey: TKey);
    procedure InsertKeyData(Index: Integer; const AKey: TKey; const AData: TData);
    function Remove(const AKey: TKey): Integer;
    property Keys[Index: Integer]: TKey read GetKey write PutKey;
    property Data[Index: Integer]: TData read GetData write PutData;
    property KeyData[const AKey: TKey]: TData read GetKeyData write PutKeyData; default;
    property OnCompare: TKeyCompareFunc read FOnKeyCompare write SetOnKeyCompare; //deprecated;
    property OnKeyCompare: TKeyCompareFunc read FOnKeyCompare write SetOnKeyCompare;
    {$ifndef OldSyntax}
    property OnDataCompare: TDataCompareFunc read FOnDataCompare write SetOnDataCompare;
    {$endif}
  end;

implementation

{ TGenericStructList --------------------------------------------------------- }

constructor TGenericStructList.Create;
begin
  inherited Create(sizeof(T));
end;

procedure TGenericStructList.CopyItem(Src, Dest: Pointer);
begin
  T(Dest^) := T(Src^);
end;

procedure TGenericStructList.Deref(Item: Pointer);
begin
  Finalize(T(Item^));
end;

function TGenericStructList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index)^);
end;

function TGenericStructList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TGenericStructList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(T(Item1^), T(Item2^));
end;

procedure TGenericStructList.Put(Index: Integer; const Item: T);
begin
  inherited Put(Index, @Item);
end;

function TGenericStructList.Add(const Item: T): Integer;
begin
  Result := inherited Add(@Item);
end;

{$ifdef HAS_EXTRACT}
function TGenericStructList.Extract(const Item: T): T;
begin
  inherited Extract(@Item, @Result);
end;
{$endif}

function TGenericStructList.First: T;
begin
  Result := T(inherited First^);
end;

{$ifdef HAS_ENUMERATOR}
function TGenericStructList.GetEnumerator: TFPGListEnumeratorSpec;
begin
  Result := TFPGListEnumeratorSpec.Create(Self);
end;
{$endif}

function TGenericStructList.IndexOf(const Item: T): Integer;
begin
  Result := inherited IndexOf(@Item);
end;

procedure TGenericStructList.Insert(Index: Integer; const Item: T);
begin
  T(inherited Insert(Index)^) := Item;
end;

function TGenericStructList.Last: T;
begin
  Result := T(inherited Last^);
end;

{$ifndef OldSyntax}
procedure TGenericStructList.Assign(Source: TGenericStructList);
var
  i: Integer;
begin
  Clear;
  for I := 0 to Source.Count - 1 do
    Add(Source[i]);
end;
{$endif OldSyntax}

function TGenericStructList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TGenericStructList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;

function TGenericStructList.L: PT;
begin
  Result := PT(FList);
end;

function TGenericStructList.Add: PT;
begin
  Count := Count + 1;
  Result := Addr(PTypeList(FList)^[Count - 1]);
end;

function TGenericStructList.Ptr(I: Integer): PT;
begin
  Result := Addr(PTypeList(FList)^[I]);
end;

{ TGenericStructMap ---------------------------------------------------------- }

constructor TGenericStructMap.Create;
begin
  inherited Create(SizeOf(TKey), SizeOf(TData));
end;

procedure TGenericStructMap.CopyItem(Src, Dest: Pointer);
begin
  CopyKey(Src, Dest);
  CopyData(PByte(Src)+KeySize, PByte(Dest)+KeySize);
end;

procedure TGenericStructMap.CopyKey(Src, Dest: Pointer);
begin
  TKey(Dest^) := TKey(Src^);
end;

procedure TGenericStructMap.CopyData(Src, Dest: Pointer);
begin
  TData(Dest^) := TData(Src^);
end;

procedure TGenericStructMap.Deref(Item: Pointer);
begin
  Finalize(TKey(Item^));
  Finalize(TData(Pointer(PByte(Item)+KeySize)^));
end;

function TGenericStructMap.GetKey(Index: Integer): TKey;
begin
  Result := TKey(inherited GetKey(Index)^);
end;

function TGenericStructMap.GetData(Index: Integer): TData;
begin
  Result := TData(inherited GetData(Index)^);
end;

function TGenericStructMap.GetKeyData(const AKey: TKey): TData;
begin
  Result := TData(inherited GetKeyData(@AKey)^);
end;

function TGenericStructMap.KeyCompare(Key1, Key2: Pointer): Integer;
begin
  Result := CompareByte(Key1^, Key2^, KeySize);
end;

function TGenericStructMap.KeyCustomCompare(Key1, Key2: Pointer): Integer;
begin
  Result := FOnKeyCompare(TKey(Key1^), TKey(Key2^));
end;

{$ifndef OldSyntax}
function TGenericStructMap.DataCustomCompare(Data1, Data2: Pointer): Integer;
begin
  Result := FOnDataCompare(TData(Data1^), TData(Data2^));
end;
{$endif}

procedure TGenericStructMap.SetOnKeyCompare(NewCompare: TKeyCompareFunc);
begin
  FOnKeyCompare := NewCompare;
  if NewCompare <> nil then
    {$ifndef OldSyntax} OnKeyPtrCompare {$else} OnPtrCompare {$endif} := @KeyCustomCompare
  else
    {$ifndef OldSyntax} OnKeyPtrCompare {$else} OnPtrCompare {$endif} := @KeyCompare;
end;

{$ifndef OldSyntax}
procedure TGenericStructMap.SetOnDataCompare(NewCompare: TDataCompareFunc);
begin
  FOnDataCompare := NewCompare;
  if NewCompare <> nil then
    OnDataPtrCompare := @DataCustomCompare
  else
    OnDataPtrCompare := nil;
end;
{$endif}

procedure TGenericStructMap.InitOnPtrCompare;
begin
  SetOnKeyCompare(nil);
{$ifndef OldSyntax}
  SetOnDataCompare(nil);
{$endif}
end;

procedure TGenericStructMap.PutKey(Index: Integer; const NewKey: TKey);
begin
  inherited PutKey(Index, @NewKey);
end;

procedure TGenericStructMap.PutData(Index: Integer; const NewData: TData);
begin
  inherited PutData(Index, @NewData);
end;

procedure TGenericStructMap.PutKeyData(const AKey: TKey; const NewData: TData);
begin
  inherited PutKeyData(@AKey, @NewData);
end;

function TGenericStructMap.Add(const AKey: TKey): Integer;
begin
  Result := inherited Add(@AKey);
end;

function TGenericStructMap.Add(const AKey: TKey; const AData: TData): Integer;
begin
  Result := inherited Add(@AKey, @AData);
end;

function TGenericStructMap.Find(const AKey: TKey; out Index: Integer): Boolean;
begin
  Result := inherited Find(@AKey, Index);
end;

function TGenericStructMap.IndexOf(const AKey: TKey): Integer;
begin
  Result := inherited IndexOf(@AKey);
end;

function TGenericStructMap.IndexOfData(const AData: TData): Integer;
begin
  { TODO: loop ? }
  Result := inherited IndexOfData(@AData);
end;

procedure TGenericStructMap.InsertKey(Index: Integer; const AKey: TKey);
begin
  inherited InsertKey(Index, @AKey);
end;

procedure TGenericStructMap.InsertKeyData(Index: Integer; const AKey: TKey; const AData: TData);
begin
  inherited InsertKeyData(Index, @AKey, @AData);
end;

function TGenericStructMap.Remove(const AKey: TKey): Integer;
begin
  Result := inherited Remove(@AKey);
end;

end.
