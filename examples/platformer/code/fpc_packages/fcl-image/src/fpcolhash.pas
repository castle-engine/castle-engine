{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2005 by Giulio Bernardi

    This file contains a color hash table.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{$mode objfpc}{$h+}
unit FPColHash;

interface

uses sysutils, classes, fpimage;

type TFPColorHashException = class(Exception);

type
  PColHashSubNode = ^TColHashSubNode;
  TColHashSubNode = packed record
    index : byte;
    data : pointer;
    next : PColHashSubNode;
  end;

type
  PColHashMainNode = ^TColHashMainNode;
  TColHashMainNode = packed record
    childs : array[0..16] of pointer; { can be either another MainNode or a SubNode }
  end;

{
  HashMap configuration:
  childs[MSN(A)]                                                   level 0
    |_childs[LSN(A)]                                               level 1
       |_childs[LSN(R)]                                            level 2
          |_childs[LSN(G)]                                         level 3
             |_childs[LSN(B)]                                      level 4
                |_childs[(MSN(R) MSN(G) MSN (B)) div 256]          level 5
                   |_element [(MSN(R) MSN(G) MSN (B)) mod 256]
  Very low accesses to reach an element, not much memory occupation if alpha is rarely used, event with
  images with 500.000 colors.
  For extremely colorful images (near 2^24 colors used) using only 5 bits per channel keeps the map
  small and efficient

}

type
  TFPPackedColor = record
    R, G, B, A : byte;
  end;

type
  TFPColorWeight = record
    Col : TFPPackedColor;
    Num : integer;
  end;
  PFPColorWeight = ^TFPColorWeight;
  TFPColorWeightArray = array of PFPColorWeight;


type
  TFPColorHashTable = class
  private
    Root : PColHashMainNode;
    AllIntegers : boolean;
    FCount : longword;
    procedure FreeAllData;
    function AllocateMainNode : PColHashMainNode;
    function AllocateSubNode : PColHashSubNode;
    procedure DeallocateLinkedList(node : PColHashSubNode);
    procedure DeallocateMainNode(node : PColHashMainNode; level : byte);
    procedure CalculateIndexes(Col : TFPPackedColor; var ahi, alo, ri, gi, bi, partial, sub : byte);
    function CalculateColor(const ahi, alo, ri, gi, bi, partial, sub : byte) : TFPPackedColor;
    function SearchSubNode(start : PColHashSubNode; const index : byte ) : PColHashSubNode;
    function SearchSubNodeAllocate(var start : PColHashSubNode; const index : byte ) : PColHashSubNode;
    function Search(const Col : TFPPackedColor) : PColHashSubNode;
    function SearchAllocate(const Col : TFPPackedColor) : PColHashSubNode;
  protected
  public
    procedure Insert(const Col : TFPColor; const Value : integer);
    procedure Insert(const Col : TFPColor; const Value : pointer);
    procedure Add(const Col : TFPColor; const Value : integer);
    function Get(const Col : TFPColor) : pointer;
    procedure Clear;
    function GetArray : TFPColorWeightArray;
    property Count : longword read FCount;
    constructor Create;
    destructor Destroy; override;
  end;

function FPColor2Packed(Col : TFPColor) : TFPPackedColor;
function Packed2FPColor(Col : TFPPackedColor) : TFPColor;

implementation

function FPColor2Packed(Col : TFPColor) : TFPPackedColor;
begin
  Result.R:=(Col.Red and $FF00) shr 8;
  Result.G:=(Col.Green and $FF00) shr 8;
  Result.B:=(Col.Blue and $FF00) shr 8;
  Result.A:=(Col.Alpha and $FF00) shr 8;
end;

function Packed2FPColor(Col : TFPPackedColor) : TFPColor;
begin
  Result.Red:=(Col.R shl 8) + Col.R;
  Result.Green:=(Col.G shl 8) + Col.G;
  Result.Blue:=(Col.B shl 8) + Col.B;
  Result.Alpha:=(Col.A shl 8) + Col.A;
end;

constructor TFPColorHashTable.Create;
begin
  Fcount:=0;
  AllIntegers:=true;
  Root:=nil;
end;

destructor TFPColorHashTable.Destroy;
begin
  FreeAllData;
  inherited Destroy;
end;

procedure TFPColorHashTable.CalculateIndexes(Col : TFPPackedColor; var ahi, alo, ri, gi, bi, partial, sub : byte);
var tmp : longword;
begin
  ahi := (Col.A and $F0) shr 4;
  alo := (Col.A and $F);
  ri := (Col.R and $F);
  gi := (Col.G and $F);
  bi := (Col.B and $F);
  tmp:=((Col.R and $F0) shl 4) or (Col.G and $F0) or ((Col.B and $F0) shr 4);
  partial:=tmp div 256;
  sub:=tmp mod 256;
end;

function TFPColorHashTable.CalculateColor(const ahi, alo, ri, gi, bi, partial, sub : byte) : TFPPackedColor;
var tmp : longword;
    col : TFPPackedColor;
begin
  tmp:=(partial shl 8) + sub; //partial*256 + sub;
  col.A:=(ahi shl 4) or alo;
  col.R:=((tmp and $F00) shr 4) + ri;
  col.G:=(tmp and $0F0) + gi;
  col.B:=((tmp and $00F) shl 4) + bi;
  Result:=col;
end;

procedure TFPColorHashTable.FreeAllData;
begin
  DeallocateMainNode(Root,0);
  Root:=nil;
  FCount:=0;
  AllIntegers:=true;
end;

function TFPColorHashTable.AllocateMainNode : PColHashMainNode;
var tmp : PColHashMainNode;
    i : byte;
begin
  Result:=nil;
  tmp:=getmem(sizeof(TColHashMainNode));
  if tmp=nil then raise TFPColorHashException.Create('Out of memory');
  for i:=0 to high(tmp^.childs) do
    tmp^.childs[i]:=nil;
  Result:=tmp;
end;

function TFPColorHashTable.AllocateSubNode : PColHashSubNode;
var tmp : PColHashSubNode;
begin
  Result:=nil;
  tmp:=getmem(sizeof(TColHashSubNode));
  if tmp=nil then raise TFPColorHashException.Create('Out of memory');
  tmp^.index:=0;
  tmp^.data:=nil;
  tmp^.next:=nil;
  inc(FCount);
  Result:=tmp;
end;

procedure TFPColorHashTable.DeallocateLinkedList(node : PColHashSubNode);
var tmp : PColHashSubNode;
begin
  while (node<>nil) do
  begin
    tmp:=node^.next;
    if node^.data<>nil then
      FreeMem(node^.data);
    FreeMem(node);
    node:=tmp;
  end;
end;

procedure TFPColorHashTable.DeallocateMainNode(node : PColHashMainNode; level : byte);
var i : byte;
begin
  if node=nil then exit;
  if level=5 then
  begin
    for i:=0 to high(node^.childs) do
      DeallocateLinkedList(node^.childs[i]);
  end
  else
    for i:=0 to high(node^.childs) do
      DeallocateMainNode(node^.childs[i],level+1);
  FreeMem(node);
end;

function TFPColorHashTable.SearchSubNode(start : PColHashSubNode; const index : byte ) : PColHashSubNode;
var cur : PColHashSubNode;
begin
  Result:=nil;
  cur:=start;
  while cur<>nil do
  begin
    if cur^.index=index then break
    else if cur^.index>index then exit; { exit and returns nil}
    cur:=cur^.next;
  end;
  Result:=cur;
end;

function TFPColorHashTable.SearchSubNodeAllocate(var start : PColHashSubNode; const index : byte ) : PColHashSubNode;
var tmp, cur, prev : PColHashSubNode;
begin
  Result:=nil;
  prev:=nil;
  cur:=start;
  while cur<>nil do
  begin
    if cur^.index=index then break
    else if cur^.index>index then {whoops, we must insert the new node before this one}
    begin
      tmp:=AllocateSubNode;
      tmp^.index:=index;
      tmp^.next:=cur;
      if prev<>nil then prev^.next:=tmp
      else start:=tmp;
      cur:=tmp;
      break;
    end;
    prev:=cur;
    cur:=cur^.next;
  end;
  if cur=nil then { not found! append to the end }
  begin
    cur:=AllocateSubNode;
    cur^.index:=index;
    prev^.next:=cur  { start is always <> nil}
  end;
  Result:=cur;
end;

function TFPColorHashTable.Search(const Col : TFPPackedColor) : PColHashSubNode;
var ahi, alo, ri, gi, bi, partial, sub : byte;
    tmpmain : PColHashMainNode;
begin
  Result:=nil;
  CalculateIndexes(Col, ahi, alo, ri, gi, bi, partial, sub);
  if Root=nil then exit;
  if Root^.childs[ahi]=nil then exit;
  tmpmain:=Root^.childs[ahi];
  if tmpmain^.childs[alo]=nil then exit;
  tmpmain:=tmpmain^.childs[alo];
  if tmpmain^.childs[ri]=nil then exit;
  tmpmain:=tmpmain^.childs[ri];
  if tmpmain^.childs[gi]=nil then exit;
  tmpmain:=tmpmain^.childs[gi];
  if tmpmain^.childs[bi]=nil then exit;
  tmpmain:=tmpmain^.childs[bi];

  if tmpmain^.childs[partial]=nil then exit;
  Result:=SearchSubNode(tmpmain^.childs[partial],sub);
end;

{ get the node; if there isn't, build the part of the tree }
function TFPColorHashTable.SearchAllocate(const Col : TFPPackedColor) : PColHashSubNode;
var ahi, alo, ri, gi, bi, partial, sub : byte;
   tmpmain : PColHashMainNode;
begin
  Result:=nil;
  CalculateIndexes(Col, ahi, alo, ri, gi, bi, partial, sub);
  if Root=nil then Root:=AllocateMainNode;
  if Root^.childs[ahi]=nil then Root^.childs[ahi]:=AllocateMainNode;
  tmpmain:=Root^.childs[ahi];
  if tmpmain^.childs[alo]=nil then tmpmain^.childs[alo]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[alo];
  if tmpmain^.childs[ri]=nil then tmpmain^.childs[ri]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[ri];
  if tmpmain^.childs[gi]=nil then tmpmain^.childs[gi]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[gi];
  if tmpmain^.childs[bi]=nil then tmpmain^.childs[bi]:=AllocateMainNode;
  tmpmain:=tmpmain^.childs[bi];

  if tmpmain^.childs[partial]=nil then  { newly-created linked list. }
  begin
    tmpmain^.childs[partial]:=AllocateSubNode;
    Result:=tmpmain^.childs[partial];
    Result^.index:=sub;
    exit;
  end;
  Result:=SearchSubNodeAllocate(tmpmain^.childs[partial],sub)
end;

procedure TFPColorHashTable.Insert(const Col : TFPColor; const Value : integer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  node^.data:=getmem(sizeof(Value));
  integer(node^.data^):=value;
end;

procedure TFPColorHashTable.Insert(const Col : TFPColor; const Value : pointer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  node^.data:=Value;
  AllIntegers:=false;
end;

procedure TFPColorHashTable.Add(const Col : TFPColor; const Value : integer);
var node : PColHashSubNode;
begin
  node:=SearchAllocate(FPColor2Packed(col));
  if node^.data=nil then
  begin
    node^.data:=getmem(sizeof(Value));
    integer(node^.data^):=0;
  end;
  inc(integer(node^.data^),value);
end;

function TFPColorHashTable.Get(const Col : TFPColor) : pointer;
var node : PColHashSubNode;
begin
  node:=Search(FPColor2Packed(col));
  if node<>nil then
    Result:=node^.data
  else
    Result:=nil;
end;

procedure TFPColorHashTable.Clear;
begin
  FreeAllData;
end;

function TFPColorHashTable.GetArray : TFPColorWeightArray;
var ahi, alo, ri, gi, bi, partial : byte;
    node : PColHashSubNode;
    i : longword;
    cw : PFPColorWeight;
    tmp1,tmp2,tmp3,tmp4,tmp5 : PColHashMainNode;
begin
  if not AllIntegers then
    raise TFPColorHashException.Create('Hashtable data is not made by integers.');
  SetLength(Result,FCount);
  if Root=nil then exit;
  i:=0;
  for ahi:=0 to 15 do
  begin
    if Root^.childs[ahi]=nil then continue;
    tmp1:=Root^.childs[ahi];
    for alo:=0 to 15 do
    begin
      if tmp1^.childs[alo]=nil then continue;
      tmp2:=tmp1^.childs[alo];
      for ri:=0 to 15 do
      begin
        if tmp2^.childs[ri]=nil then continue;
        tmp3:=tmp2^.childs[ri];
        for gi:=0 to 15 do
        begin
          if tmp3^.childs[gi]=nil then continue;
          tmp4:=tmp3^.childs[gi];
          for bi:=0 to 15 do
          begin
            if tmp4^.childs[bi]=nil then continue;
            tmp5:=tmp4^.childs[bi];
            for partial:=0 to 15 do
            begin
              node:=tmp5^.childs[partial];
              while (node<>nil) do
              begin
                getmem(cw,sizeof(TFPColorWeight));
                if cw=nil then
                  raise TFPColorHashException.Create('Out of memory');
                cw^.Col:=CalculateColor(ahi,alo,ri,gi,bi,partial,node^.index);
                cw^.Num:=integer(node^.data^);
                Result[i]:=cw;
                inc(i);
                node:=node^.next;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
