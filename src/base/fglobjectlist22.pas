{ Hacky implementation of TFPGObjectList for FPC 2.2.4.
  @exclude }
unit FGLObjectList22;

interface

{$ifndef VER2_2} {$fatal This unit should never be compiled and used by FPC > 2.2.} {$endif}

uses FGL;

const
  MaxGListSize = MaxInt div 1024;

type
  generic TFPGObjectList<T> = class(TFPSList)
  type public
    TCompareFunc = function(const Item1, Item2: T): Integer;
    TTypeList = array[0..MaxGListSize] of T;
    PTypeList = ^TTypeList;
    PT = ^T;
  var protected
    FOnCompare: TCompareFunc;
    procedure CopyItem(Src, Dest: Pointer); override;
    function  Get(Index: Integer): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function  GetList: PTypeList; {$ifdef CLASSESINLINE} inline; {$endif}
    function  ItemPtrCompare(Item1, Item2: Pointer): Integer;
    procedure Put(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
  public
    FreeObjects: boolean;
    constructor Create(AFreeObjects: boolean = true);
    destructor Destroy; override;
    function Add(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    function Extract(const Item: T): T; {$ifdef CLASSESINLINE} inline; {$endif}
    function First: T; {$ifdef CLASSESINLINE} inline; {$endif}
    function IndexOf(const Item: T): Integer;
    procedure Insert(Index: Integer; const Item: T); {$ifdef CLASSESINLINE} inline; {$endif}
    function Last: T; {$ifdef CLASSESINLINE} inline; {$endif}
    { $info FIXME: bug #10479: implement TFPGList<T>.Assign(TFPGList) to work somehow}
    {procedure Assign(Source: TFPGList);}
    function Remove(const Item: T): Integer; {$ifdef CLASSESINLINE} inline; {$endif}
    procedure Sort(Compare: TCompareFunc);
    property Items[Index: Integer]: T read Get write Put; default;
    property List: PTypeList read GetList;
  end;

implementation

uses
  rtlconsts;

{ Kambi implementation notes:

  - We don't free items in Deref. Instead, we free them only at destruction.
    Using Deref, it doesn't work
    (another problem of 2.2.x, since Deref is virtual?
    It works in FPC 2.4.2, this is how TFPGObjectList.FreeObjects works there.)

  - We don't derive from TFPGList, to avoid old FPC 2.2.x bugs
    related to overriding destructors in generics.
    Instead, this is just copied from FPC 2.2.4 sources of TFPGList,
    and adjusted.
}

constructor TFPGObjectList.Create(AFreeObjects: boolean);
begin
  inherited Create(sizeof(T));
  FreeObjects := AFreeObjects;
end;

destructor TFPGObjectList.Destroy;
var
  I: Integer;
begin
  if FreeObjects then
    for I := 0 to Count - 1 do
    begin
      Items[I].Free;
      Items[I] := nil;
    end;
  inherited;
end;

procedure TFPGObjectList.CopyItem(Src, Dest: Pointer);
begin
  T(Dest^) := T(Src^);
end;

function TFPGObjectList.Get(Index: Integer): T;
begin
  Result := T(inherited Get(Index)^);
end;

function TFPGObjectList.GetList: PTypeList;
begin
  Result := PTypeList(FList);
end;

function TFPGObjectList.ItemPtrCompare(Item1, Item2: Pointer): Integer;
begin
  Result := FOnCompare(T(Item1^), T(Item2^));
end;

procedure TFPGObjectList.Put(Index: Integer; const Item: T);
begin
  inherited Put(Index, @Item);
end;

function TFPGObjectList.Add(const Item: T): Integer;
begin
  Result := inherited Add(@Item);
end;

function TFPGObjectList.Extract(const Item: T): T;
var
  ResPtr: Pointer;
begin
  ResPtr := inherited Extract(@Item);
  if ResPtr <> nil then
    Result := T(ResPtr^)
  else
    FillByte(Result, 0, sizeof(T));
end;

function TFPGObjectList.First: T;
begin
  Result := T(inherited First^);
end;

function TFPGObjectList.IndexOf(const Item: T): Integer;
begin
  Result := 0;
  { $info TODO: fix inlining to work! InternalItems[Result]^}
  while (Result < FCount) and (PT(FList)[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TFPGObjectList.Insert(Index: Integer; const Item: T);
begin
  T(inherited Insert(Index)^) := Item;
end;

function TFPGObjectList.Last: T;
begin
  Result := T(inherited Last^);
end;

function TFPGObjectList.Remove(const Item: T): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TFPGObjectList.Sort(Compare: TCompareFunc);
begin
  FOnCompare := Compare;
  inherited Sort(@ItemPtrCompare);
end;

end.
