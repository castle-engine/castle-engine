{
  Based on FPC FGL unit, copyright by FPC team.
  License of FPC RTL is the same as our engine (modified LGPL,
  see COPYING.txt for details).
  Fixed to compile also under FPC 2.4.0 and 2.2.4.
  Some small comfortable methods added.
}

{ Generic list of any type (TGenericStructList). }
unit GenericStructList;

{$mode objfpc}{$H+}

{$IF defined(VER2_2)} {$DEFINE OldSyntax} {$IFEND}
{$IF defined(VER2_4)} {$DEFINE OldSyntax} {$IFEND}

{$define HAS_ENUMERATOR}
{$ifdef VER2_2} {$undef HAS_ENUMERATOR} {$endif}
{$ifdef VER2_4_0} {$undef HAS_ENUMERATOR} {$endif}

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

    The only new method is @link(Add) without parameters, that returns
    the pointer to newly created item. Comfortable and efficient way
    to add and initialize new item. }
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
    function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    {$ifdef HAS_ENUMERATOR} function GetEnumerator: TFPGListEnumeratorSpec; {$ifdef CLASSESINLINE} inline; {$endif} {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
{$ifndef OldSyntax}
    procedure Assign(Source: TFPGList);
{$endif OldSyntax}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;
    function Add: PT;
  end;

implementation

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

function TGenericStructList.Extract(const Item: T): T;
var
  ResPtr: Pointer;
begin
  ResPtr := inherited Extract(@Item);
  if ResPtr <> nil then
    Result := T(ResPtr^)
  else
    FillByte(Result, sizeof(T), 0);
end;

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

function TGenericStructList.Add: PT;
begin
  Count := Count + 1;
  Result := @(List^[Count - 1]);
end;

end.
