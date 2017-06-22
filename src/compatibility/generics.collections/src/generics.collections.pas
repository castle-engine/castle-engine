{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Maciej Izak (hnb)
    member of the Free Sparta development team (http://freesparta.com)

    Copyright(c) 2004-2014 DaThoX

    It contains the Free Pascal generics library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Generics.Collections;

{$MODE DELPHI}{$H+}
{$MACRO ON}
{$COPERATORS ON}
{$DEFINE CUSTOM_DICTIONARY_CONSTRAINTS := TKey, TValue, THashFactory}
{$DEFINE OPEN_ADDRESSING_CONSTRAINTS := TKey, TValue, THashFactory, TProbeSequence}
{$DEFINE CUCKOO_CONSTRAINTS := TKey, TValue, THashFactory, TCuckooCfg}
{$WARNINGS OFF}
{$HINTS OFF}
{$OVERFLOWCHECKS OFF}
{$RANGECHECKS OFF}

interface

uses
    Classes, SysUtils, Generics.MemoryExpanders, Generics.Defaults,
    Generics.Helpers, Generics.Strings;

{ FPC BUGS related to Generics.* (54 bugs, 19 fixed)
  REGRESSION: 26483, 26481
  FIXED REGRESSION: 26480, 26482

  CRITICAL: 24848(!!!), 24872(!), 25607(!), 26030, 25917, 25918, 25620, 24283, 24254, 24287 (Related to? 24872)
  IMPORTANT: 23862(!), 24097, 24285, 24286 (Similar to? 24285), 24098, 24609 (RTL inconsistency), 24534,
             25606, 25614, 26177, 26195
  OTHER: 26484, 24073, 24463, 25593, 25596, 25597, 25602, 26181 (or MYBAD?)
  CLOSED BUT IMO STILL TO FIX: 25601(!), 25594
  FIXED: 25610(!), 24064, 24071, 24282, 24458, 24867, 24871, 25604, 25600, 25605, 25598, 25603, 25929, 26176, 26180,
         26193, 24072
  MYBAD: 24963, 25599
}

{ LAZARUS BUGS related to Generics.* (7 bugs, 0 fixed)
  CRITICAL: 25613
  OTHER: 25595, 25612, 25615, 25617, 25618, 25619
}

type
  // bug #24254 workaround
  // should be TArray = record class procedure Sort<T>(...) etc.
  TCustomArrayHelper<T> = class abstract
  private
    type
      // bug #24282
      TComparerBugHack = TComparer<T>;
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>);
      virtual; abstract;
  public
    class procedure Sort(var AValues: array of T); overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>);   overload;
    class procedure Sort(var AValues: array of T;
      const AComparer: IComparer<T>; AIndex, ACount: SizeInt); overload;

    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; virtual; abstract; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt): Boolean; overload;
  end experimental; // will be renamed to TCustomArray (bug #24254)

  TArrayHelper<T> = class(TCustomArrayHelper<T>)
  protected
    // modified QuickSort from classes\lists.inc
    class procedure QuickSort(var AValues: array of T; ALeft, ARight: SizeInt; const AComparer: IComparer<T>); override;
  public
    class function BinarySearch(constref AValues: array of T; constref AItem: T;
      out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
      AIndex, ACount: SizeInt): Boolean; override; overload;
  end experimental; // will be renamed to TArray (bug #24254)

  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(ASender: TObject; constref AItem: T; AAction: TCollectionNotification)
    of object;

  { TEnumerator }

  TEnumerator<T> = class abstract
  protected
    function DoGetCurrent: T; virtual; abstract;
    function DoMoveNext: boolean; virtual; abstract;
  public
    property Current: T read DoGetCurrent;
    function MoveNext: boolean;
  end;

  { TEnumerable }

  TEnumerable<T> = class abstract
  protected
    function ToArrayImpl(ACount: SizeInt): TArray<T>; overload; // used by descendants
  protected
    function DoGetEnumerator: TEnumerator<T>; virtual; abstract;
  public
    function GetEnumerator: TEnumerator<T>; inline;
    function ToArray: TArray<T>; virtual; overload;
  end;

  // More info: http://stackoverflow.com/questions/5232198/about-vectors-growth
  // TODO: custom memory managers (as constraints)
  {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result + Result div 2} // ~approximation to golden ratio: n = n * 1.5 }
  // {$DEFINE CUSTOM_LIST_CAPACITY_INC := Result * 2} // standard inc
  TCustomList<T> = class abstract(TEnumerable<T>)
  protected
    type // bug #24282
      TArrayHelperBugHack = TArrayHelper<T>;
  private
    FOnNotify: TCollectionNotifyEvent<T>;
    function GetCapacity: SizeInt; inline;
  protected
    FItemsLength: SizeInt;
    FItems: array of T;

    function PrepareAddingItem: SizeInt; virtual;
    function PrepareAddingRange(ACount: SizeInt): SizeInt; virtual;
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); virtual;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; virtual;
    procedure SetCapacity(AValue: SizeInt); virtual; abstract;
    function GetCount: SizeInt; virtual;
  public
    function ToArray: TArray<T>; override; final;

    property Count: SizeInt read GetCount;
    property Capacity: SizeInt read GetCapacity write SetCapacity;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;
  end;

  TCustomListEnumerator<T> = class abstract(TEnumerator< T >)
  private
    FList: TCustomList<T>;
    FIndex: SizeInt;
  protected
    function DoMoveNext: boolean; override;
    function DoGetCurrent: T; override;
    function GetCurrent: T; virtual;
  public
    constructor Create(AList: TCustomList<T>);
  end;

  TList<T> = class(TCustomList<T>)
  private var
    FComparer: IComparer<T>;
  protected
    // bug #24287 - workaround for generics type name conflict (Identifier not found)
    // next bug workaround - for another error related to previous workaround
    // change order (method must be declared before TEnumerator declaration)
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    // with this type declaration i found #24285, #24285
    type
      // bug workaround
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    procedure SetCount(AValue: SizeInt);
  private
    function GetItem(AIndex: SizeInt): T;
    procedure SetItem(AIndex: SizeInt; const AValue: T);
  public
    constructor Create; overload;
    constructor Create(const AComparer: IComparer<T>); overload;
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;

    function Add(constref AValue: T): SizeInt;
    procedure AddRange(constref AValues: array of T); overload;
    procedure AddRange(const AEnumerable: IEnumerable<T>); overload;
    procedure AddRange(AEnumerable: TEnumerable<T>); overload;

    procedure Insert(AIndex: SizeInt; constref AValue: T);
    procedure InsertRange(AIndex: SizeInt; constref AValues: array of T); overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>); overload;
    procedure InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>); overload;

    function Remove(constref AValue: T): SizeInt;
    procedure Delete(AIndex: SizeInt); inline;
    procedure DeleteRange(AIndex, ACount: SizeInt);
    function ExtractIndex(const AIndex: SizeInt): T; overload;
    function Extract(constref AValue: T): T; overload;

    procedure Exchange(AIndex1, AIndex2: SizeInt);
    procedure Move(AIndex, ANewIndex: SizeInt);

    function First: T; inline;
    function Last: T; inline;

    procedure Clear;

    function Contains(constref AValue: T): Boolean; inline;
    function IndexOf(constref AValue: T): SizeInt; virtual;
    function LastIndexOf(constref AValue: T): SizeInt; virtual;

    procedure Reverse;

    procedure TrimExcess;

    procedure Sort; overload;
    procedure Sort(const AComparer: IComparer<T>); overload;
    function BinarySearch(constref AItem: T; out AIndex: SizeInt): Boolean; overload;
    function BinarySearch(constref AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean; overload;

    property Count: SizeInt read FItemsLength write SetCount;
    property Items[Index: SizeInt]: T read GetItem write SetItem; default;
  end;

  TThreadList<T> = class
  private
    FList: TList<T>;
    FDuplicates: TDuplicates;
    FLock: TRTLCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(constref AValue: T);
    procedure Remove(constref AValue: T);
    procedure Clear;

    function LockList: TList<T>;
    procedure UnlockList; inline;

    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
  end;

  TQueue<T> = class(TCustomList<T>)
  protected
    // bug #24287 - workaround for generics type name conflict (Identifier not found)
    // next bug workaround - for another error related to previous workaround
    // change order (function must be declared before TEnumerator declaration}
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>)
      public
        constructor Create(AQueue: TQueue<T>);
      end;

    function GetEnumerator: TEnumerator; reintroduce;
  private
    FLow: SizeInt;
  protected
    procedure SetCapacity(AValue: SizeInt); override;
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    function GetCount: SizeInt; override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Enqueue(constref AValue: T);
    function Dequeue: T;
    function Extract: T;
    function Peek: T;
    procedure Clear;
    procedure TrimExcess;
  end;

  TStack<T> = class(TCustomList<T>)
  protected
  // bug #24287 - workaround for generics type name conflict (Identifier not found)
  // next bug workaround - for another error related to previous workaround
  // change order (function must be declared before TEnumerator declaration}
    function DoGetEnumerator: {Generics.Collections.}TEnumerator<T>; override;
  public
    type
      TEnumerator = class(TCustomListEnumerator<T>);

    function GetEnumerator: TEnumerator; reintroduce;
  protected
    function DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T; override;
    procedure SetCapacity(AValue: SizeInt); override;
  public
    constructor Create(ACollection: TEnumerable<T>); overload;
    destructor Destroy; override;
    procedure Clear;
    procedure Push(constref AValue: T);
    function Pop: T; inline;
    function Peek: T;
    function Extract: T; inline;
    procedure TrimExcess;
  end;

  TObjectList<T: class> = class(TList<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  TObjectQueue<T: class> = class(TQueue<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    procedure Dequeue;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  TObjectStack<T: class> = class(TStack<T>)
  private
    FObjectsOwner: Boolean;
  protected
    procedure Notify(constref AValue: T; ACollectionNotification: TCollectionNotification); override;
  public
    constructor Create(AOwnsObjects: Boolean = True); overload;
    constructor Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean = True); overload;
    function Pop: T;
    property OwnsObjects: Boolean read FObjectsOwner write FObjectsOwner;
  end;

  PObject = ^TObject;

{$I inc\generics.dictionariesh.inc}

function InCircularRange(ABottom, AItem, ATop: SizeInt): Boolean;

implementation

function InCircularRange(ABottom, AItem, ATop: SizeInt): Boolean;
begin
  Result :=
       (ABottom < AItem) and (AItem <= ATop )
    or (ATop < ABottom) and (AItem > ABottom)
    or (ATop < ABottom ) and (AItem <= ATop );
end;

{ TCustomArrayHelper<T> }

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt; const AComparer: IComparer<T>): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, AComparer, Low(AValues), Length(AValues));
end;

class function TCustomArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt): Boolean;
begin
  Result := BinarySearch(AValues, AItem, AFoundIndex, TComparerBugHack.Default, Low(AValues), Length(AValues));
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T);
begin
  QuickSort(AValues, Low(AValues), High(AValues), TComparerBugHack.Default);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>);
begin
  QuickSort(AValues, Low(AValues), High(AValues), AComparer);
end;

class procedure TCustomArrayHelper<T>.Sort(var AValues: array of T;
  const AComparer: IComparer<T>; AIndex, ACount: SizeInt);
begin
  if ACount <= 1 then
    Exit;
  QuickSort(AValues, AIndex, Pred(AIndex + ACount), AComparer);
end;

{ TArrayHelper<T> }

class procedure TArrayHelper<T>.QuickSort(var AValues: array of T; ALeft, ARight: SizeInt;
  const AComparer: IComparer<T>);
var
  I, J: SizeInt;
  P, Q: T;
begin
  if ((ARight - ALeft) <= 0) or (Length(AValues) = 0) then
    Exit;
  repeat
    I := ALeft;
    J := ARight;
    P := AValues[ALeft + (ARight - ALeft) shr 1];
    repeat
        while AComparer.Compare(AValues[I], P) < 0 do
          I += 1;
        while AComparer.Compare(AValues[J], P) > 0 do
          J -= 1;
      if I <= J then
      begin
        if I <> J then
        begin
          Q := AValues[I];
          AValues[I] := AValues[J];
          AValues[J] := Q;
        end;
        I += 1;
        J -= 1;
      end;
    until I > J;
    // sort the smaller range recursively
    // sort the bigger range via the loop
    // Reasons: memory usage is O(log(n)) instead of O(n) and loop is faster than recursion
    if J - ALeft < ARight - I then
    begin
      if ALeft < J then
        QuickSort(AValues, ALeft, J, AComparer);
      ALeft := I;
    end
    else
    begin
      if I < ARight then
        QuickSort(AValues, I, ARight, AComparer);
      ARight := J;
    end;
   until ALeft >= ARight;
end;

class function TArrayHelper<T>.BinarySearch(constref AValues: array of T; constref AItem: T;
  out AFoundIndex: SizeInt; const AComparer: IComparer<T>;
  AIndex, ACount: SizeInt): Boolean;
var
  imin, imax, imid: Int32;
  LCompare: SizeInt;
begin
  // continually narrow search until just one element remains
  imin := AIndex;
  imax := Pred(AIndex + ACount);

  // http://en.wikipedia.org/wiki/Binary_search_algorithm
  while (imin < imax) do
  begin
        imid := imin + ((imax - imin) shr 1);

        // code must guarantee the interval is reduced at each iteration
        // assert(imid < imax);
        // note: 0 <= imin < imax implies imid will always be less than imax

        LCompare := AComparer.Compare(AValues[imid], AItem);
        // reduce the search
        if (LCompare < 0) then
          imin := imid + 1
        else
        begin
          imax := imid;
          if LCompare = 0 then
          begin
            AFoundIndex := imid;
            Exit(True);
          end;
        end;
  end;
    // At exit of while:
    //   if A[] is empty, then imax < imin
    //   otherwise imax == imin

    // deferred test for equality

  LCompare := AComparer.Compare(AValues[imin], AItem);
  if (imax = imin) and (LCompare = 0) then
  begin
    AFoundIndex := imin;
    Exit(True);
  end
  else
  begin
    AFoundIndex := -1;
    Exit(False);
  end;
end;

{ TEnumerator<T> }

function TEnumerator<T>.MoveNext: boolean;
begin
  Exit(DoMoveNext);
end;

{ TEnumerable<T> }

function TEnumerable<T>.ToArrayImpl(ACount: SizeInt): TArray<T>;
var
  i: SizeInt;
  LEnumerator: TEnumerator<T>;
begin
  SetLength(Result, ACount);

  try
    LEnumerator := GetEnumerator;

    i := 0;
    while LEnumerator.MoveNext do
    begin
      Result[i] := LEnumerator.Current;
      Inc(i);
    end;
  finally
    LEnumerator.Free;
  end;
end;

function TEnumerable<T>.GetEnumerator: TEnumerator<T>;
begin
  Exit(DoGetEnumerator);
end;

function TEnumerable<T>.ToArray: TArray<T>;
var
  LEnumerator: TEnumerator<T>;
  LBuffer: TList<T>;
begin
  LBuffer := TList<T>.Create;
  try
    LEnumerator := GetEnumerator;

    while LEnumerator.MoveNext do
      LBuffer.Add(LEnumerator.Current);

    Result := LBuffer.ToArray;
  finally
    LBuffer.Free;
    LEnumerator.Free;
  end;
end;

{ TCustomList<T> }

function TCustomList<T>.PrepareAddingItem: SizeInt;
begin
  Result := Length(FItems);

  if (FItemsLength < 4) and (Result < 4) then
    SetLength(FItems, 4)
  else if FItemsLength = High(FItemsLength) then
    OutOfMemoryError
  else if FItemsLength = Result then
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);

  Result := FItemsLength;
  Inc(FItemsLength);
end;

function TCustomList<T>.PrepareAddingRange(ACount: SizeInt): SizeInt;
begin
  if ACount < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  if ACount = 0 then
    Exit(FItemsLength - 1);

  if (FItemsLength = 0) and (Length(FItems) = 0) then
    SetLength(FItems, 4)
  else if FItemsLength = High(FItemsLength) then
    OutOfMemoryError;

  Result := Length(FItems);
  while Pred(FItemsLength + ACount) >= Result do
  begin
    SetLength(FItems, CUSTOM_LIST_CAPACITY_INC);
    Result := Length(FItems);
  end;

  Result := FItemsLength;
  Inc(FItemsLength, ACount);
end;

function TCustomList<T>.ToArray: TArray<T>;
begin
  Result := ToArrayImpl(Count);
end;

function TCustomList<T>.GetCount: SizeInt;
begin
  Result := FItemsLength;
end;

procedure TCustomList<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(Self, AValue, ACollectionNotification);
end;

function TCustomList<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  if (AIndex < 0) or (AIndex >= FItemsLength) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  Dec(FItemsLength);

  FItems[AIndex] := Default(T);
  if AIndex <> FItemsLength then
  begin
    System.Move(FItems[AIndex + 1], FItems[AIndex], (FItemsLength - AIndex) * SizeOf(T));
    FillChar(FItems[FItemsLength], SizeOf(T), 0);
  end;

  Notify(Result, ACollectionNotification);
end;

function TCustomList<T>.GetCapacity: SizeInt;
begin
  Result := Length(FItems);
end;

{ TCustomListEnumerator<T> }

function TCustomListEnumerator<T>.DoMoveNext: boolean;
begin
  Inc(FIndex);
  Result := (FList.FItemsLength <> 0) and (FIndex < FList.FItemsLength)
end;

function TCustomListEnumerator<T>.DoGetCurrent: T;
begin
  Result := GetCurrent;
end;

function TCustomListEnumerator<T>.GetCurrent: T;
begin
  Result := FList.FItems[FIndex];
end;

constructor TCustomListEnumerator<T>.Create(AList: TCustomList<T>);
begin
  inherited Create;
  FIndex := -1;
  FList := AList;
end;

{ TList<T> }

constructor TList<T>.Create;
begin
  FComparer := TComparer<T>.Default;
end;

constructor TList<T>.Create(const AComparer: IComparer<T>);
begin
  FComparer := AComparer;
end;

constructor TList<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  Create;
  for LItem in ACollection do
    Add(LItem);
end;

destructor TList<T>.Destroy;
begin
  SetCapacity(0);
end;

procedure TList<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    Count := AValue;

  SetLength(FItems, AValue);
end;

procedure TList<T>.SetCount(AValue: SizeInt);
begin
  if AValue < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue > Capacity then
    Capacity := AValue;
  if AValue < Count then
    DeleteRange(AValue, Count - AValue);

  FItemsLength := AValue;
end;

function TList<T>.GetItem(AIndex: SizeInt): T;
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
end;

procedure TList<T>.SetItem(AIndex: SizeInt; const AValue: T);
begin
  if (AIndex < 0) or (AIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  FItems[AIndex] := AValue;
end;

function TList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TList<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TList<T>.Add(constref AValue: T): SizeInt;
begin
  Result := PrepareAddingItem;
  FItems[Result] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.AddRange(constref AValues: array of T);
begin
  InsertRange(Count, AValues);
end;

procedure TList<T>.AddRange(const AEnumerable: IEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.AddRange(AEnumerable: TEnumerable<T>);
var
  LValue: T;
begin
  for LValue in AEnumerable do
    Add(LValue);
end;

procedure TList<T>.Insert(AIndex: SizeInt; constref AValue: T);
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AIndex <> PrepareAddingItem then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + 1], ((Count - AIndex) - 1) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T), 0);
  end;

  FItems[AIndex] := AValue;
  Notify(AValue, cnAdded);
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; constref AValues: array of T);
var
  i: SizeInt;
  LLength: SizeInt;
  LValue: ^T;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LLength := Length(AValues);
  if LLength = 0 then
    Exit;

  if AIndex <> PrepareAddingRange(LLength) then
  begin
    System.Move(FItems[AIndex], FItems[AIndex + LLength], ((Count - AIndex) - LLength) * SizeOf(T));
    FillChar(FItems[AIndex], SizeOf(T) * LLength, 0);
  end;

  LValue := @AValues[0];
  for i := AIndex to Pred(AIndex + LLength) do
  begin
    FItems[i] := LValue^;
    Notify(LValue^, cnAdded);
    Inc(LValue);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: IEnumerable<T>);
var
  LValue: T;
  i: SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    Insert(Aindex + i, LValue);
    Inc(i);
  end;
end;

procedure TList<T>.InsertRange(AIndex: SizeInt; const AEnumerable: TEnumerable<T>);
var
  LValue: T;
  i:  SizeInt;
begin
  if (AIndex < 0) or (AIndex > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  i := 0;
  for LValue in AEnumerable do
  begin
    Insert(Aindex + i, LValue);
    Inc(i);
  end;
end;

function TList<T>.Remove(constref AValue: T): SizeInt;
begin
  Result := IndexOf(AValue);
  if Result >= 0 then
    DoRemove(Result, cnRemoved);
end;

procedure TList<T>.Delete(AIndex: SizeInt);
begin
  DoRemove(AIndex, cnRemoved);
end;

procedure TList<T>.DeleteRange(AIndex, ACount: SizeInt);
var
  LDeleted: array of T;
  i: SizeInt;
  LMoveDelta: SizeInt;
begin
  if ACount = 0 then
    Exit;

  if (ACount < 0) or (AIndex < 0) or (AIndex + ACount > Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  SetLength(LDeleted, Count);
  System.Move(FItems[AIndex], LDeleted[0], ACount * SizeOf(T));

  LMoveDelta := Count - (AIndex + ACount);

  if LMoveDelta = 0 then
    FillChar(FItems[AIndex], ACount * SizeOf(T), #0)
  else
  begin
    System.Move(FItems[AIndex + ACount], FItems[AIndex], LMoveDelta * SizeOf(T));
    FillChar(FItems[Count - ACount], ACount * SizeOf(T), #0);
  end;

  FItemsLength -= ACount;

  for i := 0 to High(LDeleted) do
    Notify(LDeleted[i], cnRemoved);
end;

function TList<T>.ExtractIndex(const AIndex: SizeInt): T;
begin
  Result := DoRemove(AIndex, cnExtracted);
end;

function TList<T>.Extract(constref AValue: T): T;
var
  LIndex: SizeInt;
begin
  LIndex := IndexOf(AValue);
  if LIndex < 0 then
    Exit(Default(T));

  Result := DoRemove(LIndex, cnExtracted);
end;

procedure TList<T>.Exchange(AIndex1, AIndex2: SizeInt);
var
  LTemp: T;
begin
  LTemp := FItems[AIndex1];
  FItems[AIndex1] := FItems[AIndex2];
  FItems[AIndex2] := LTemp;
end;

procedure TList<T>.Move(AIndex, ANewIndex: SizeInt);
var
  LTemp: T;
begin
  if ANewIndex = AIndex then
    Exit;

  if (ANewIndex < 0) or (ANewIndex >= Count) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  LTemp := FItems[AIndex];
  FItems[AIndex] := Default(T);

  if AIndex < ANewIndex then
    System.Move(FItems[Succ(AIndex)], FItems[AIndex], (ANewIndex - AIndex) * SizeOf(T))
  else
    System.Move(FItems[ANewIndex], FItems[Succ(ANewIndex)], (AIndex - ANewIndex) * SizeOf(T));

  FillChar(FItems[ANewIndex], SizeOf(T), #0);
  FItems[ANewIndex] := LTemp;
end;

function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Pred(Count)];
end;

procedure TList<T>.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TList<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

function TList<T>.Contains(constref AValue: T): Boolean;
begin
  Result := IndexOf(AValue) >= 0;
end;

function TList<T>.IndexOf(constref AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := 0 to Count - 1 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

function TList<T>.LastIndexOf(constref AValue: T): SizeInt;
var
  i: SizeInt;
begin
  for i := Count - 1 downto 0 do
    if FComparer.Compare(AValue, FItems[i]) = 0 then
      Exit(i);
  Result := -1;
end;

procedure TList<T>.Reverse;
var
  a, b: SizeInt;
  LTemp: T;
begin
  a := 0;
  b := Count - 1;
  while a < b do
  begin
    LTemp := FItems[a];
    FItems[a] := FItems[b];
    FItems[b] := LTemp;
    Inc(a);
    Dec(b);
  end;
end;

procedure TList<T>.Sort;
begin
  TArrayHelperBugHack.Sort(FItems, FComparer, 0, Count);
end;

procedure TList<T>.Sort(const AComparer: IComparer<T>);
begin
  TArrayHelperBugHack.Sort(FItems, AComparer, 0, Count);
end;

function TList<T>.BinarySearch(constref AItem: T; out AIndex: SizeInt): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex);
end;

function TList<T>.BinarySearch(constref AItem: T; out AIndex: SizeInt; const AComparer: IComparer<T>): Boolean;
begin
  Result := TArrayHelperBugHack.BinarySearch(FItems, AItem, AIndex, AComparer);
end;

{ TThreadList<T> }

constructor TThreadList<T>.Create;
begin
  inherited Create;
  FDuplicates:=dupIgnore;
  InitCriticalSection(FLock);
  FList := TList<T>.Create;
end;

destructor TThreadList<T>.Destroy;
begin
  LockList;
  try
    FList.Free;
    inherited Destroy;
  finally
    UnlockList;
    DoneCriticalSection(FLock);
  end;
end;

procedure TThreadList<T>.Add(constref AValue: T);
begin
  LockList;
  try
    if (Duplicates = dupAccept) or (FList.IndexOf(AValue) = -1) then
      FList.Add(AValue)
    else if Duplicates = dupError then
      raise EArgumentException.CreateRes(@SDuplicatesNotAllowed);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Remove(constref AValue: T);
begin
  LockList;
  try
    FList.Remove(AValue);
  finally
    UnlockList;
  end;
end;

procedure TThreadList<T>.Clear;
begin
  LockList;
  try
    FList.Clear;
  finally
    UnlockList;
  end;
end;

function TThreadList<T>.LockList: TList<T>;
begin
  Result:=FList;
  System.EnterCriticalSection(FLock);
end;

procedure TThreadList<T>.UnlockList;
begin
  System.LeaveCriticalSection(FLock);
end;

{ TQueue<T>.TEnumerator }

constructor TQueue<T>.TEnumerator.Create(AQueue: TQueue<T>);
begin
  inherited Create(AQueue);

  FIndex := Pred(AQueue.FLow);
end;

{ TQueue<T> }

function TQueue<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TQueue<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

function TQueue<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  Notify(Result, ACollectionNotification);
  FLow += 1;
  if FLow = FItemsLength then
  begin
    FLow := 0;
    FItemsLength := 0;
  end;
end;

procedure TQueue<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  if AValue = FItemsLength then
    Exit;

  if (Count > 0) and (FLow > 0) then
  begin
    Move(FItems[FLow], FItems[0], Count * SizeOf(T));
    FillChar(FItems[Count], (FItemsLength - Count) * SizeOf(T), #0);
  end;

  SetLength(FItems, AValue);
  FItemsLength := Count;
  FLow := 0;
end;

function TQueue<T>.GetCount: SizeInt;
begin
  Result := FItemsLength - FLow;
end;

constructor TQueue<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Enqueue(LItem);
end;

destructor TQueue<T>.Destroy;
begin
  Clear;
end;

procedure TQueue<T>.Enqueue(constref AValue: T);
var
  LIndex: SizeInt;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TQueue<T>.Dequeue: T;
begin
  Result := DoRemove(FLow, cnRemoved);
end;

function TQueue<T>.Extract: T;
begin
  Result := DoRemove(FLow, cnExtracted);
end;

function TQueue<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FLow];
end;

procedure TQueue<T>.Clear;
begin
  while Count <> 0 do
    Dequeue;
  FLow := 0;
  FItemsLength := 0;
end;

procedure TQueue<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TStack<T> }

function TStack<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(Self);
end;

function TStack<T>.DoGetEnumerator: {Generics.Collections.}TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

constructor TStack<T>.Create(ACollection: TEnumerable<T>);
var
  LItem: T;
begin
  for LItem in ACollection do
    Push(LItem);
end;

function TStack<T>.DoRemove(AIndex: SizeInt; ACollectionNotification: TCollectionNotification): T;
begin
  if AIndex < 0 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[AIndex];
  FItems[AIndex] := Default(T);
  FItemsLength -= 1;
  Notify(Result, ACollectionNotification);
end;

destructor TStack<T>.Destroy;
begin
  Clear;
end;

procedure TStack<T>.Clear;
begin
  while Count <> 0 do
    Pop;
end;

procedure TStack<T>.SetCapacity(AValue: SizeInt);
begin
  if AValue < Count then
    AValue := Count;

  SetLength(FItems, AValue);
end;

procedure TStack<T>.Push(constref AValue: T);
var
  LIndex: SizeInt;
begin
  LIndex := PrepareAddingItem;
  FItems[LIndex] := AValue;
  Notify(AValue, cnAdded);
end;

function TStack<T>.Pop: T;
begin
  Result := DoRemove(FItemsLength - 1, cnRemoved);
end;

function TStack<T>.Peek: T;
begin
  if (Count = 0) then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);

  Result := FItems[FItemsLength - 1];
end;

function TStack<T>.Extract: T;
begin
  Result := DoRemove(FItemsLength - 1, cnExtracted);
end;

procedure TStack<T>.TrimExcess;
begin
  SetCapacity(Count);
end;

{ TObjectList<T> }

procedure TObjectList<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);

  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectList<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(const AComparer: IComparer<T>; AOwnsObjects: Boolean);
begin
  inherited Create(AComparer);

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectList<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

{ TObjectQueue<T> }

procedure TObjectQueue<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectQueue<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectQueue<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

procedure TObjectQueue<T>.Dequeue;
begin
  inherited Dequeue;
end;

{ TObjectStack<T> }

procedure TObjectStack<T>.Notify(constref AValue: T; ACollectionNotification: TCollectionNotification);
begin
  inherited Notify(AValue, ACollectionNotification);
  if FObjectsOwner and (ACollectionNotification = cnRemoved) then
    TObject(AValue).Free;
end;

constructor TObjectStack<T>.Create(AOwnsObjects: Boolean);
begin
  inherited Create;

  FObjectsOwner := AOwnsObjects;
end;

constructor TObjectStack<T>.Create(ACollection: TEnumerable<T>; AOwnsObjects: Boolean);
begin
  inherited Create(ACollection);

  FObjectsOwner := AOwnsObjects;
end;

function TObjectStack<T>.Pop: T;
begin
  Result := inherited Pop;
end;

{$I inc\generics.dictionaries.inc}

end.
