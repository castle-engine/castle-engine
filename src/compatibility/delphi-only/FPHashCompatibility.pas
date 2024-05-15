{
 Based on Contnrs from FPC:

 This file is part of the Free Component Library (FCL)
    Copyright (c) 2002 by Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}

unit FPHashCompatibility;

interface

uses
  SysUtils,Classes, Types;

type
  TObjectListCallback = Procedure(data:TObject;arg:pointer) of object;
  TObjectListStaticCallback = Procedure(data:TObject;arg:pointer);
  TListCallback = procedure(data,arg:pointer) of object;
  TListStaticCallback = procedure(data,arg:pointer);


  THashItem=record
    HashValue : UInt32;
    StrIndex  : Integer;
    NextIndex : Integer;
    Data      : Pointer;
  end;
  PHashItem=^THashItem;

const
  MaxHashListSize = Maxint div SizeOf(THashItem);
  MaxHashStrSize  = Maxint;
  MaxHashTableSize = Maxint div SizeOf(Integer);

  MaxItemsPerHash = 3;
  SListIndexError               = 'List index (%d) out of bounds';
  SListCapacityError            = 'List capacity (%d) exceeded.';
  SListCountError               = 'List count (%d) out of bounds.';

 type
  PHashItemList = ^THashItemList;
  THashItemList = array[0..MaxHashListSize - 1] of THashItem;
  PHashTable = ^THashTable;
  THashTable = array[0..MaxHashTableSize - 1] of Integer;

  TFPHashList = class(TObject)
  private
    { ItemList }
    FHashList     : PHashItemList;
    FCount,
    FCapacity : Integer;
    { Hash }
    FHashTable    : PHashTable;
    FHashCapacity : Integer;
    { Strings }
    FStrs     : PAnsiChar;
    FStrCount,
    FStrCapacity : Integer;
    Function InternalFind(AHash:UInt32;const AName:shortstring;out PrevIndex:Integer):Integer;
  protected
    Function Get(Index: Integer): Pointer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Put(Index: Integer; Item: Pointer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer);
    Procedure SetCount(NewCount: Integer);
    Procedure RaiseIndexError(Index : Integer);
    Function  AddStr(const s:shortstring): Integer;
    Procedure AddToHashTable(Index: Integer);
    Procedure StrExpand(MinIncSize:Integer);
    Procedure SetStrCapacity(NewCapacity: Integer);
    Procedure SetHashCapacity(NewCapacity: Integer);
    Procedure ReHash;
  public
    constructor Create;
    destructor Destroy; override;
    Function Add(const AName:shortstring;Item: Pointer): Integer;
    Procedure Clear;
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): UInt32; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer;
    Procedure Delete(Index: Integer);
    class Procedure Error(const Msg: string; Data: NativeInt);
    Function Expand: TFPHashList;
    Function Extract(item: Pointer): Pointer;
    Function IndexOf(Item: Pointer): Integer;
    Function Find(const AName:shortstring): Pointer;
    Function FindIndexOf(const AName:shortstring): Integer;
    Function FindWithHash(const AName:shortstring;AHash:UInt32): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer;
    Function Remove(Item: Pointer): Integer;
    Procedure Pack;
    Procedure ShowStatistics;
    Procedure ForEachCall(proc2call:TListCallback;arg:pointer); overload;
    Procedure ForEachCall(proc2call:TListStaticCallback;arg:pointer); overload;
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property List: PHashItemList read FHashList;
    property Strs: PAnsiChar read FStrs;
  end;


{*******************************************************
        TFPHashObjectList (From fcl/inc/contnrs.pp)
********************************************************}

  TFPHashObjectList = class;

  { TFPHashObject }

  TFPHashObject = class
  private
    FOwner     : TFPHashObjectList;
    FCachedStr : pshortstring;
    FStrIndex  : Integer;
    Procedure InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
  protected
    Function GetName:shortstring;virtual;
    Function GetHash:UInt32;virtual;
  public
    constructor CreateNotOwned;
    constructor Create(HashObjectList:TFPHashObjectList;const s:shortstring);
    Procedure ChangeOwner(HashObjectList:TFPHashObjectList); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Rename(const ANewName:shortstring);
    property Name:shortstring read GetName;
    property Hash:UInt32 read GetHash;
  end;

  TFPHashObjectList = class(TObject)
  private
    FFreeObjects : Boolean;
    FHashList: TFPHashList;
    Function GetCount: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCount(const AValue: integer); {$ifdef CCLASSESINLINE}inline;{$endif}
  protected
    Function GetItem(Index: Integer): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetItem(Index: Integer; AObject: TObject); {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure SetCapacity(NewCapacity: Integer); {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetCapacity: integer; {$ifdef CCLASSESINLINE}inline;{$endif}
  public
    constructor Create(FreeObjects : boolean = True);
    destructor Destroy; override;
    Procedure Clear;
    Function Add(const AName:shortstring;AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function NameOfIndex(Index: Integer): ShortString; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function HashOfIndex(Index: Integer): UInt32; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function GetNextCollision(Index: Integer): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure Delete(Index: Integer);
    Function Expand: TFPHashObjectList; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Extract(Item: TObject): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Remove(AObject: TObject): Integer;
    Function IndexOf(AObject: TObject): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function Find(const s:shortstring): TObject; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindIndexOf(const s:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindWithHash(const AName:shortstring;AHash:UInt32): Pointer;
    Function Rename(const AOldName,ANewName:shortstring): Integer; {$ifdef CCLASSESINLINE}inline;{$endif}
    Function FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt: Integer): Integer;
    Procedure Pack; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ShowStatistics; {$ifdef CCLASSESINLINE}inline;{$endif}
    Procedure ForEachCall(proc2call:TObjectListCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif} overload;
    Procedure ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer); {$ifdef CCLASSESINLINE}inline;{$endif} overload;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property OwnsObjects: Boolean read FFreeObjects write FFreeObjects;
    property Items[Index: Integer]: TObject read GetItem write SetItem; default;
    property List: TFPHashList read FHashList;
  end;

implementation

uses CastleUtils;

{*****************************************************************************
                            TFPHashList
*****************************************************************************}

    Function FPHash(const s:shortstring):UInt32; overload;
    var
      p,pmax : PAnsiChar;
    begin
{$I norqcheckbegin.inc}
      Result:=0;
      p:=@s[1];
      pmax:=@s[length(s)+1];
      while (p<pmax) do
        begin
          Result:=UInt32(Int32(Result shl 5) - Int32(Result)) xor UInt32(P^);
          Inc(p);
        end;
{$I norqcheckend.inc}
    end;

    Function FPHash(P: PAnsiChar; Len: Integer): UInt32; overload;
    var
      pmax : PAnsiChar;
    begin
{$I norqcheckbegin.inc}
      Result:=0;
      pmax:=p+len;
      while (p<pmax) do
        begin
          Result:=UInt32(Int32(Result shl 5) - Int32(Result)) xor UInt32(P^);
          Inc(p);
        end;
{$I norqcheckend.inc}
    end;


Procedure TFPHashList.RaiseIndexError(Index : Integer);
begin
  Error(SListIndexError, Index);
end;


Function TFPHashList.Get(Index: Integer): Pointer;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].Data;
end;


Procedure TFPHashList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  FHashList^[Index].Data:=Item;
end;


Function TFPHashList.NameOfIndex(Index: Integer): shortstring;
begin
  if (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  with FHashList^[Index] do
    begin
    if StrIndex>=0 then
      Result:=PShortString(@FStrs[StrIndex])^
    else
      Result:='';
    end;
end;


Function TFPHashList.HashOfIndex(Index: Integer): UInt32;
begin
  If (Index < 0) or (Index >= FCount) then
    RaiseIndexError(Index);
  Result:=FHashList^[Index].HashValue;
end;


Function TFPHashList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=-1;
  if ((Index > -1) and (Index < FCount)) then
    Result:=FHashList^[Index].NextIndex;
end;


Function TFPHashList.Extract(item: Pointer): Pointer;
var
  i : Integer;
begin
  Result:=nil;
  i:=IndexOf(item);
  if i >= 0 then
    begin
    Result:=item;
    Delete(i);
    end;
end;


Procedure TFPHashList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxHashListSize) then
     Error (SListCapacityError, NewCapacity);
  if NewCapacity = FCapacity then
    Exit;
  ReallocMem(FHashList, NewCapacity*SizeOf(THashItem));
  FCapacity:=NewCapacity;
  { Maybe expand hash also }
  if FCapacity>FHashCapacity*MaxItemsPerHash then
    SetHashCapacity(FCapacity div MaxItemsPerHash);
end;


Procedure TFPHashList.SetCount(NewCount: Integer);
begin
  if (NewCount < 0) or (NewCount > MaxHashListSize)then
    Error(SListCountError, NewCount);
  if NewCount > FCount then
    begin
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if FCount < NewCount then
      FillChar(FHashList^[FCount], (NewCount-FCount) div SizeOf(THashItem), 0);
    end;
  FCount:=NewCount;
end;


Procedure TFPHashList.SetStrCapacity(NewCapacity: Integer);
begin
  {$warn COMPARISON_FALSE off}
  if (NewCapacity < FStrCount) or (NewCapacity > MaxHashStrSize) then
    Error(SListCapacityError, NewCapacity);
  {$warn COMPARISON_FALSE on}
  if NewCapacity = FStrCapacity then
    Exit;
  ReallocMem(FStrs, NewCapacity);
  FStrCapacity:=NewCapacity;
end;


Procedure TFPHashList.SetHashCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < 1) then
    Error(SListCapacityError, NewCapacity);
  if FHashCapacity=NewCapacity then
    Exit;
  FHashCapacity:=NewCapacity;
  ReallocMem(FHashTable, FHashCapacity*SizeOf(Integer));
  ReHash;
end;


Procedure TFPHashList.ReHash;
var
  i : Integer;
begin
  FillDword(FHashTable^, FHashCapacity, High(UInt32));
  for i:=0 to FCount-1 do
    AddToHashTable(i);
end;


constructor TFPHashList.Create;
begin
  SetHashCapacity(1);
end;


destructor TFPHashList.Destroy;
begin
  Clear;
  if Assigned(FHashTable) then
    FreeMem(FHashTable);
  inherited Destroy;
end;


Function TFPHashList.AddStr(const s:shortstring): Integer;
var
  Len : Integer;
begin
  len:=Length(s)+1;
  if FStrCount+Len >= FStrCapacity then
    StrExpand(Len);
  System.Move(s[0],FStrs[FStrCount],Len);
  Result:=FStrCount;
  Inc(FStrCount,Len);
end;


Procedure TFPHashList.AddToHashTable(Index: Integer);
var
  HashIndex : Integer;
begin
  with FHashList^[Index] do
    begin
    if not Assigned(Data) then
      Exit;
    HashIndex:=HashValue mod UInt32(FHashCapacity);
    NextIndex:=FHashTable^[HashIndex];
    FHashTable^[HashIndex]:=Index;
    end;
end;


Function TFPHashList.Add(const AName:shortstring;Item: Pointer): Integer;
begin
  if FCount = FCapacity then
    Expand;
  with FHashList^[FCount] do
    begin
    HashValue:=FPHash(AName);
    Data:=Item;
    StrIndex:=AddStr(AName);
    end;
  AddToHashTable(FCount);
  Result:=FCount;
  Inc(FCount);
end;

Procedure TFPHashList.Clear;
begin
  if Assigned(FHashList) then
    begin
    FCount:=0;
    SetCapacity(0);
    FHashList:=nil;
    end;
  SetHashCapacity(1);
  FHashTable^[0]:=(-1); // sethashcapacity does not always call rehash
  if Assigned(FStrs) then
    begin
    FStrCount:=0;
    SetStrCapacity(0);
    FStrs:=nil;
    end;
end;

Procedure TFPHashList.Delete(Index: Integer);
begin
  if (Index<0) or (Index>=FCount) then
    Error(SListIndexError, Index);
  { Remove from HashList }
  Dec(FCount);
  System.Move(FHashList^[Index+1], FHashList^[Index], (FCount - Index) * SizeOf(THashItem));
  { All indexes are updated, we need to build the hashtable again }
  ReHash;
  { Shrink the list if appropriate }
  if (FCapacity > 256) and (FCount < FCapacity shr 2) then
    begin
    FCapacity:=FCapacity shr 1;
    ReAllocMem(FHashList, SizeOf(THashItem) * FCapacity);
    end;
end;

Function TFPHashList.Remove(Item: Pointer): Integer;
begin
  Result:=IndexOf(Item);
  If Result <> -1 then
    Self.Delete(Result);
end;

class Procedure TFPHashList.Error(const Msg: string; Data: PtrInt);
begin
  ///raise EListError.CreateFmt(Msg,[Data]) at get_caller_addr(get_frame), get_caller_frame(get_frame);
  raise EListError.Create(Msg);
end;

Function TFPHashList.Expand: TFPHashList;
var
  IncSize : Int32;
begin
  Result:=Self;
  if FCount < FCapacity then
    Exit;
  IncSize:=SizeOf(PtrInt)*2;
  if FCapacity > 127 then
    Inc(IncSize, FCapacity shr 2)
  else if FCapacity > SizeOf(PtrInt)*3 then
    Inc(IncSize, FCapacity shr 1)
  else if FCapacity >= SizeOf(PtrInt) then
    Inc(IncSize,sizeof(PtrInt));
  SetCapacity(FCapacity + IncSize);
end;

Procedure TFPHashList.StrExpand(MinIncSize:Integer);
var
  IncSize : Int32;
begin
  if FStrCount+MinIncSize < FStrCapacity then
    Exit;
  IncSize:=64;
  if FStrCapacity > 255 then
    Inc(IncSize, FStrCapacity shr 2);
  SetStrCapacity(FStrCapacity + IncSize + MinIncSize);
end;

Function TFPHashList.IndexOf(Item: Pointer): Integer;
var
  psrc  : PHashItem;
  Index : integer;
begin
  Result:=-1;
  psrc:=@FHashList^[0];
  for Index:=0 to FCount-1 do
    begin
    if psrc^.Data=Item then
      begin
      Result:=Index;
      Exit;
      end;
    Inc(psrc);
    end;
end;

Function TFPHashList.InternalFind(AHash:UInt32;const AName:shortstring;out PrevIndex:Integer):Integer;
var
  HashIndex : Integer;
  Len,
  LastChar  : AnsiChar;
begin
  HashIndex:=AHash mod UInt32(FHashCapacity);
  Result:=FHashTable^[HashIndex];
  Len:=AnsiChar(Length(AName));
  LastChar:=AName[Byte(Len)];
  PrevIndex:=-1;
  while Result<>-1 do
    with FHashList^[Result] do
      begin
      if Assigned(Data) and
         (HashValue=AHash) and
         (Len=FStrs[StrIndex]) and
         (LastChar=FStrs[StrIndex+Byte(Len)]) and
         (AName=PShortString(@FStrs[StrIndex])^) then
        Exit;
      PrevIndex:=Result;
      Result:=NextIndex;
      end;
end;


Function TFPHashList.Find(const AName:shortstring): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(FPHash(AName),AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TFPHashList.FindIndexOf(const AName:shortstring): Integer;
var
  PrevIndex : Integer;
begin
  Result:=InternalFind(FPHash(AName),AName,PrevIndex);
end;


Function TFPHashList.FindWithHash(const AName:shortstring;AHash:UInt32): Pointer;
var
  Index,
  PrevIndex : Integer;
begin
  Result:=nil;
  Index:=InternalFind(AHash,AName,PrevIndex);
  if Index=-1 then
    Exit;
  Result:=FHashList^[Index].Data;
end;


Function TFPHashList.Rename(const AOldName,ANewName:shortstring): Integer;
var
  PrevIndex,
  Index : Integer;
  OldHash : UInt32;
begin
  Result:=-1;
  OldHash:=FPHash(AOldName);
  Index:=InternalFind(OldHash,AOldName,PrevIndex);
  if Index=-1 then
    Exit;
  { Remove from current Hash }
  if PrevIndex<>-1 then
    FHashList^[PrevIndex].NextIndex:=FHashList^[Index].NextIndex
  else
    FHashTable^[OldHash mod UInt32(FHashCapacity)]:=FHashList^[Index].NextIndex;
  { Set new name and hash }
  with FHashList^[Index] do
    begin
    HashValue:=FPHash(ANewName);
    StrIndex:=AddStr(ANewName);
    end;
  { Insert back in Hash }
  AddToHashTable(Index);
  { Return Index }
  Result:=Index;
end;

Procedure TFPHashList.Pack;
var
  NewCount,
  i : integer;
  pdest,
  psrc : PHashItem;
  FOldStr : PAnsichar;
begin
  NewCount:=0;
  psrc:=@FHashList^[0];
  FOldStr:=FStrs;
  try
    FStrs:=nil;
    FStrCount:=0;
    FStrCapacity:=0;
    pdest:=psrc;
    for I:=0 to FCount-1 do
      begin
      if Assigned(psrc^.Data) then
        begin
        pdest^:=psrc^;
        pdest^.StrIndex:=AddStr(PShortString(@FOldStr[PDest^.StrIndex])^);
        Inc(pdest);
        Inc(NewCount);
        end;
      Inc(psrc);
      end;
  finally
    FreeMem(FoldStr);
  end;
  FCount:=NewCount;
  { We need to ReHash to update the IndexNext }
  ReHash;
  { Release over-capacity }
  SetCapacity(FCount);
  SetStrCapacity(FStrCount);
end;


Procedure TFPHashList.ShowStatistics;
var
  HashMean,
  HashStdDev : Double;
  Index,
  i,j : Integer;
begin
  { Calculate Mean and StdDev }
  HashMean:=0;
  HashStdDev:=0;
  for i:=0 to FHashCapacity-1 do
    begin
    j:=0;
    Index:=FHashTable^[i];
    while (Index<>-1) do
      begin
      Inc(j);
      Index:=FHashList^[Index].NextIndex;
      end;
    HashMean:=HashMean+j;
    HashStdDev:=HashStdDev+Sqr(j);
    end;
  HashMean:=HashMean/FHashCapacity;
  HashStdDev:=(HashStdDev-FHashCapacity*Sqr(HashMean));
  if FHashCapacity>1 then
    HashStdDev:=Sqrt(HashStdDev/(FHashCapacity-1))
  else
    HashStdDev:=0;
  { Print info to stdout }
  Writeln('HashSize   : ',FHashCapacity);
  Writeln('HashMean   : ',HashMean:1:4);
  Writeln('HashStdDev : ',HashStdDev:1:4);
  Writeln('ListSize   : ',FCount,'/',FCapacity);
  Writeln('StringSize : ',FStrCount,'/',FStrCapacity);
end;


Procedure TFPHashList.ForEachCall(proc2call:TListCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


Procedure TFPHashList.ForEachCall(proc2call:TListStaticCallback;arg:pointer);
var
  i : integer;
  p : pointer;
begin
  for i:=0 to Count-1 Do
    begin
    p:=FHashList^[i].Data;
    if Assigned(p) then
      proc2call(p,arg);
    end;
end;


{*****************************************************************************
                               TFPHashObject
*****************************************************************************}

Procedure TFPHashObject.InternalChangeOwner(HashObjectList:TFPHashObjectList;const s:shortstring);
var
  Index : integer;
begin
  FOwner:=HashObjectList;
  Index:=HashObjectList.Add(s,Self);
  FStrIndex:=HashObjectList.List.List^[Index].StrIndex;
  FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
end;


constructor TFPHashObject.CreateNotOwned;
begin
  FStrIndex:=-1;
end;


constructor TFPHashObject.Create(HashObjectList:TFPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


Procedure TFPHashObject.ChangeOwner(HashObjectList:TFPHashObjectList);
begin
  InternalChangeOwner(HashObjectList,PShortString(@FOwner.List.Strs[FStrIndex])^);
end;


Procedure TFPHashObject.ChangeOwnerAndName(HashObjectList:TFPHashObjectList;const s:shortstring);
begin
  InternalChangeOwner(HashObjectList,s);
end;


Procedure TFPHashObject.Rename(const ANewName:shortstring);
var
  Index : integer;
begin
  Index:=FOwner.Rename(PShortString(@FOwner.List.Strs[FStrIndex])^,ANewName);
  if Index<>-1 then
    begin
    FStrIndex:=FOwner.List.List^[Index].StrIndex;
    FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    end;
end;


Function TFPHashObject.GetName:shortstring;
begin
  if FOwner<>nil then
    begin
    FCachedStr:=PShortString(@FOwner.List.Strs[FStrIndex]);
    Result:=FCachedStr^;
    end
  else
    Result:='';
end;


Function TFPHashObject.GetHash:UInt32;
begin
  if FOwner<>nil then
    Result:=FPHash(PShortString(@FOwner.List.Strs[FStrIndex])^)
  else
    Result:=$ffffffff;
end;


{*****************************************************************************
            TFPHashObjectList (Copied from rtl/objpas/classes/lists.inc)
*****************************************************************************}

constructor TFPHashObjectList.Create(FreeObjects : boolean = True);
begin
  inherited Create;
  FHashList:=TFPHashList.Create;
  FFreeObjects:=Freeobjects;
end;

destructor TFPHashObjectList.Destroy;
begin
  if (FHashList <> nil) then
    begin
    Clear;
    FHashList.Destroy;
    end;
  inherited Destroy;
end;

Procedure TFPHashObjectList.Clear;
var
  i: integer;
begin
  if FFreeObjects then
    for i:=0 to FHashList.Count - 1 do
      TObject(FHashList[i]).Free;
  FHashList.Clear;
end;

Function TFPHashObjectList.GetCount: integer;
begin
  Result:=FHashList.Count;
end;

Procedure TFPHashObjectList.SetCount(const AValue: integer);
begin
  if FHashList.Count <> AValue then
    FHashList.Count:=AValue;
end;

Function TFPHashObjectList.GetItem(Index: Integer): TObject;
begin
  Result:=TObject(FHashList[Index]);
end;

Procedure TFPHashObjectList.SetItem(Index: Integer; AObject: TObject);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList[Index]:=AObject;
end;

Procedure TFPHashObjectList.SetCapacity(NewCapacity: Integer);
begin
  FHashList.Capacity:=NewCapacity;
end;

Function TFPHashObjectList.GetCapacity: integer;
begin
  Result:=FHashList.Capacity;
end;

Function TFPHashObjectList.Add(const AName:shortstring;AObject: TObject): Integer;
begin
  Result:=FHashList.Add(AName,AObject);
end;

Function TFPHashObjectList.NameOfIndex(Index: Integer): shortstring;
begin
  Result:=FHashList.NameOfIndex(Index);
end;

Function TFPHashObjectList.HashOfIndex(Index: Integer): UInt32;
begin
  Result:=FHashList.HashOfIndex(Index);
end;

Function TFPHashObjectList.GetNextCollision(Index: Integer): Integer;
begin
  Result:=FHashList.GetNextCollision(Index);
end;

Procedure TFPHashObjectList.Delete(Index: Integer);
begin
  if OwnsObjects then
    TObject(FHashList[Index]).Free;
  FHashList.Delete(Index);
end;

Function TFPHashObjectList.Expand: TFPHashObjectList;
begin
  FHashList.Expand;
  Result:=Self;
end;

Function TFPHashObjectList.Extract(Item: TObject): TObject;
begin
  Result:=TObject(FHashList.Extract(Item));
end;

Function TFPHashObjectList.Remove(AObject: TObject): Integer;
begin
  Result:=IndexOf(AObject);
  if (Result <> -1) then
    begin
    if OwnsObjects then
      TObject(FHashList[Result]).Free;
    FHashList.Delete(Result);
    end;
end;

Function TFPHashObjectList.IndexOf(AObject: TObject): Integer;
begin
  Result:=FHashList.IndexOf(Pointer(AObject));
end;


Function TFPHashObjectList.Find(const s:shortstring): TObject;
begin
  Result:=TObject(FHashList.Find(s));
end;


Function TFPHashObjectList.FindIndexOf(const s:shortstring): Integer;
begin
  Result:=FHashList.FindIndexOf(s);
end;


Function TFPHashObjectList.FindWithHash(const AName:shortstring;AHash:UInt32): Pointer;
begin
  Result:=TObject(FHashList.FindWithHash(AName,AHash));
end;


Function TFPHashObjectList.Rename(const AOldName,ANewName:shortstring): Integer;
begin
  Result:=FHashList.Rename(AOldName,ANewName);
end;


Function TFPHashObjectList.FindInstanceOf(AClass: TClass; AExact: Boolean; AStartAt : Integer): Integer;
var
  I : Integer;
begin
  I:=AStartAt;
  Result:=-1;
  if AExact then
    while (I<Count) and (Result=-1) do
      if Items[i].ClassType=AClass then
        Result:=I
      else
        Inc(I)
  else
    while (I<Count) and (Result=-1) do
      if Items[i].InheritsFrom(AClass) then
        Result:=I
      else
        Inc(I);
end;


Procedure TFPHashObjectList.Pack;
begin
  FHashList.Pack;
end;


Procedure TFPHashObjectList.ShowStatistics;
begin
  FHashList.ShowStatistics;
end;


Procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListCallBack(proc2call),arg);
end;


Procedure TFPHashObjectList.ForEachCall(proc2call:TObjectListStaticCallback;arg:pointer);
begin
  FHashList.ForEachCall(TListStaticCallBack(proc2call),arg);
end;

end.