{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2005 by Giulio Bernardi

    This file contains classes used to quantize images.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}

{$mode objfpc}{$h+}
unit FPQuantizer;

interface

uses sysutils, classes, fpimage, fpcolhash;

type
  FPQuantizerException = class (exception);

type
  TFPQuantizerProgressEvent = procedure (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte;
                                         const Msg: AnsiString; var Continue : Boolean) of object;

type
  TFPColorQuantizer = class
    private
      FOnProgress : TFPQuantizerProgressEvent;
    protected
      FColNum : longword;
      FSupportsAlpha : boolean;
      FImages : array of TFPCustomImage;
      FCount : integer;
      function InternalQuantize : TFPPalette; virtual; abstract;
      procedure SetColNum(AColNum : longword); virtual;
      procedure Progress (Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean); virtual;
      function GetImage(Index : integer) : TFPCustomImage;
      procedure SetImage(Index : integer; const Img : TFPCustomImage);
      procedure SetCount(Value : integer);
    public
      property OnProgress : TFPQuantizerProgressEvent read FOnProgress write FOnProgress;
      property Images[Index : integer] : TFPCustomImage read GetImage write SetImage;
      property Count : integer read FCount write SetCount;
      property ColorNumber : longword read FColNum write SetColNum;
      property SupportsAlpha : boolean read FSupportsAlpha;
      procedure Clear;
      procedure Add(const Img : TFPCustomImage);
      function Quantize : TFPPalette;
      constructor Create; virtual;
      destructor Destroy; override;
  end;


type
  POctreeQNode = ^TOctreeQNode;
  TOctreeQChilds = array[0..7] of POctreeQNode;
  TOctreeQNode = record
    isleaf : boolean;
    count : longword;
    R, G, B : longword;
    Next : POctreeQNode; //used in the reduction list.
    Childs : TOctreeQChilds;
  end;


type
  TFPOctreeQuantizer = class(TFPColorQuantizer)
    private
      Root : POctreeQNode;
      ReductionList : TOctreeQChilds;
      LeafTot, MaxLeaf : longword;
      percent : byte;              { these values are used to call OnProgress event }
      percentinterval : longword;
      percentacc : longword;
      FContinue : boolean;
      procedure DisposeNode(var Node : POctreeQNode);
      procedure AddColor(var Node : POctreeQNode; const R, G, B, Level : byte);
      procedure AddToPalette(var Node : POctreeQNode; Palette : TFPPalette; var Current : integer);
      procedure Reduce;
      function BuildPalette : TFPPalette;
    protected
      function InternalQuantize : TFPPalette; override;
    public
  end;

type
  TMCBox = record
    total, startindex, endindex : longword;
  end;

const mcSlow = 0;
      mcNormal = 1;
      mcFast = 2;

type
  TFPMedianCutQuantizer = class(TFPColorQuantizer)
    private
      HashTable, palcache : TFPColorHashTable;
      arr : TFPColorWeightArray;
      boxes : array of TMCBox;
      Used : integer;
      percent : byte;              { these values are used to call OnProgress event }
      percentinterval : longword;
      percentacc : longword;
      FContinue : boolean;
      FMode : byte;
      function ColorCompare(const c1, c2 : TFPPackedColor; const Dim : byte) : shortint;
      function FindLargestDimension(const Box : TMCBox) : byte;
      procedure QuickSort(const l, r : integer; const Dim : byte);
      procedure QuickSortBoxes(const l, r : integer);
      function MeanBox(const box : TMCBox) : TFPColor;
      function BuildPalette : TFPPalette;
      procedure SetMode(const Amode : byte);
      function MaskColor(const col : TFPColor) : TFPColor;
    protected
      function InternalQuantize : TFPPalette; override;
    public
      constructor Create; override;
      property Mode : byte read FMode write SetMode;
  end;

implementation

function RGB2FPColor(const R, G, B : longword) : TFPColor;
begin
  Result.Red:=(R shl 8) + R;
  Result.Green:=(G shl 8) + G;
  Result.Blue:=(B shl 8) + B;
  Result.Alpha := AlphaOpaque;
end;

{ TFPColorQuantizer }

function TFPColorQuantizer.Quantize : TFPPalette;
begin
  Result:=InternalQuantize;
end;

constructor TFPColorQuantizer.Create;
begin
  FSupportsAlpha:=false;
  FColNum:=256; //default setting.
  FCount:=0;
  setlength(FImages,0);
end;

destructor TFPColorQuantizer.Destroy;
begin
  Setlength(FImages,0);
  inherited Destroy;
end;

procedure TFPColorQuantizer.SetColNum(AColNum : longword);
begin
  if AColNum<2 then
    raise FPQuantizerException.Create('Invalid color depth: '+IntToStr(AColNum));
  FColNum:=AColNum;
end;

procedure TFPColorQuantizer.Progress(Sender: TObject; Stage: TFPImgProgressStage; PercentDone: Byte; const Msg: AnsiString; var Continue : Boolean);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Sender,Stage,PercentDone,Msg,Continue);
end;

function TFPColorQuantizer.GetImage(Index : integer) : TFPCustomImage;
begin
  if Index>=FCount then
    raise FPQuantizerException.Create('Invalid image index: '+IntToStr(Index));
  Result:=FImages[index];
end;

procedure TFPColorQuantizer.SetImage(Index : integer; const Img : TFPCustomImage);
begin
  if Index>=FCount then
    raise FPQuantizerException.Create('Invalid image index: '+IntToStr(Index));
  FImages[Index]:=Img;
end;

procedure TFPColorQuantizer.SetCount(Value : integer);
var old, i : integer;
begin
  old:=FCount;
  setlength(FImages,Value);
  for i:=old to Value-1 do
    FImages[i]:=nil;
  FCount:=Value;
end;

procedure TFPColorQuantizer.Clear;
begin
  setlength(FImages,0);
  FCount:=0;
end;

procedure TFPColorQuantizer.Add(const Img : TFPCustomImage);
var i : integer;
begin
{ Find first unused slot }
  for i:=0 to FCount-1 do
    if FImages[i]=nil then
    begin
      Fimages[i]:=Img;
      exit;
    end;
 { If we reached this point there are no unused slot: let's enlarge the array }
  SetCount(Fcount+1);
  FImages[FCount-1]:=Img;
end;

{ TFPOctreeQuantizer }

const Mask : array[0..7] of byte = ($80, $40, $20, $10, $08, $04, $02, $01);

procedure TFPOctreeQuantizer.AddColor(var Node : POctreeQNode; const R, G, B, Level : byte);
var index, shift : byte;
begin
  if Node=nil then
  begin
    Node:=getmem(sizeof(TOctreeQNode));
    if Node=nil then
      raise FPQuantizerException.Create('Out of memory');
    FillByte(Node^,sizeof(TOctreeQNode),0);
    if level=7 then
    begin
      Node^.isleaf:=true;
      inc(LeafTot); { we just created a new leaf }
    end
    else
    begin { we don't put leaves in reduction list since this is unuseful }
      Node^.isleaf:=false;
      Node^.Next:=ReductionList[level]; { added on top of the reduction list for its level }
      ReductionList[level]:=Node;
    end;
  end;
  if Node^.isleaf then
  begin
    inc(Node^.R,R);
    inc(Node^.G,G);
    inc(Node^.B,B);
    inc(Node^.count);
  end
  else
  begin
    shift:=7-level;
    index:=((R and mask[level]) shr shift) shl 2;
    index:=index+((G and mask[level]) shr shift) shl 1;
    index:=index+((B and mask[level]) shr shift);
    AddColor(Node^.Childs[index],R,G,B,Level+1);
  end;
end;

procedure TFPOctreeQuantizer.DisposeNode(var Node : POctreeQNode);
var i : integer;
begin
  if Node=nil then exit;
  if not (Node^.isleaf) then
    for i:=0 to 7 do
      if Node^.childs[i]<>nil then
        DisposeNode(Node^.childs[i]);
  FreeMem(Node);
  Node:=nil;
end;

procedure TFPOctreeQuantizer.Reduce;
var i : integer;
    Node : POctreeQNode;
begin
  i:=6; { level 7 nodes don't have childs, start from 6 and go backward }
  while ((i>0) and (ReductionList[i]=nil)) do
    dec(i);

  { remove this node from the list}
  Node:=ReductionList[i];
  ReductionList[i]:=Node^.Next;

  for i:=0 to 7 do
    if Node^.childs[i]<>nil then
    begin
      inc(Node^.count,Node^.childs[i]^.count);
      inc(Node^.r,Node^.childs[i]^.r);
      inc(Node^.g,Node^.childs[i]^.g);
      inc(Node^.b,Node^.childs[i]^.b);
      DisposeNode(Node^.childs[i]);
      dec(LeafTot);
    end;
  Node^.isleaf:=true;
  inc(LeafTot); { this node is now a leaf! }
end;

procedure TFPOctreeQuantizer.AddToPalette(var Node : POctreeQNode; Palette : TFPPalette; var Current : integer);
var i : byte;
begin
  if not FContinue then exit;

  if Node^.isleaf then
  begin
    if (current >= LeafTot) then
      raise FPQuantizerException.Create('Octree Quantizer internal error: palette index too high.');
    Node^.r:= Node^.r div Node^.count;
    Node^.g:= Node^.g div Node^.count;
    Node^.b:= Node^.b div Node^.count;
    Palette.Color[Current]:=RGB2FPColor(Node^.r,Node^.g,Node^.b);
    inc(current);

    { ************************************************ }
    inc(percentacc);
    if percentacc>=percentinterval then
    begin
      dec(percentacc,percentinterval);
      inc(percent);
      Progress(self,psRunning,percent,'',FContinue);
    end;
    { ************************************************ }

  end
  else
  for i:=0 to 7 do
    if Node^.childs[i]<>nil then
      AddToPalette(Node^.childs[i],Palette,Current);
end;

function TFPOctreeQuantizer.BuildPalette : TFPPalette;
var pal : TFPPalette;
    i : integer;
begin
  if Root=nil then exit;
  pal:=TFPPalette.Create(LeafTot);
  i:=0;
  try
    AddToPalette(Root,pal,i);
  except
    pal.Free;
    pal:=nil;
    raise;
  end;
  if not FContinue then
  begin
    pal.Free;
    pal:=nil;
  end;
  Result:=pal;
end;

function TFPOctreeQuantizer.InternalQuantize : TFPPalette;
var i, j, k : integer;
    color : TFPColor;
begin
  Root:=nil;
  for i:=0 to high(ReductionList) do
    ReductionList[i]:=nil;
  LeafTot:=0;
  MaxLeaf:=FColNum;

  { ************************************************************** }
  { set up some values useful when calling OnProgress event        }
  { number of operations is:                                       }
  {    width*heigth for population                                 }
  {    initial palette count - final palette count for reduction   }
  {    final palette count for building the palette                }
  { total: width*heigth+initial palette count.                     }
  { if source image doesn't have a palette assume palette count as }
  { width*height (worst scenario) if it is < 2^24, or 2^24 else    }
  percentinterval:=0;
  percentacc:=0;
  for i:=0 to FCount-1 do
    if FImages[i]<>nil then
    begin
      percentinterval:=percentinterval+FImages[i].Width*FImages[i].Height;
      if FImages[i].UsePalette then
        percentacc:=percentacc+FImages[i].Palette.Count
      else
        percentacc:=percentacc+FImages[i].Width*FImages[i].Height;
    end;
  if percentacc>$1000000 then percentacc:=$1000000;

  percentinterval:=(percentacc+percentinterval) div 100;  { how many operations for 1% }
  if percentinterval=0 then percentinterval:=$FFFFFFFF;  { it's quick, call progress only when starting and ending }
  percent:=0;
  percentacc:=0;
  FContinue:=true;
  Progress (self,psStarting,0,'',FContinue);
  Result:=nil;
  if not FContinue then exit;
  { ************************************************************** }

  { populate the octree with colors }
  try
    for k:=0 to FCount-1 do
      if FImages[k]<>nil then
        for j:=0 to FImages[k].Height-1 do
          for i:=0 to FImages[k].Width-1 do
          begin
            Color:=FImages[k][i,j];
            AddColor(Root,(Color.Red and $FF00) shr 8,(Color.Green and $FF00) shr 8,(Color.Blue and $FF00) shr 8,0);
            { ************************************************* }
            inc(percentacc);
            if percentacc>=percentinterval then
            begin
              dec(percentacc,percentinterval);
              inc(percent);
              Progress(self,psRunning,percent,'',FContinue);
              if not FContinue then exit;
            end;
            { ************************************************* }
          end;
    { reduce number of colors until it is <= MaxLeaf }
    while LeafTot > MaxLeaf do
    begin
      Reduce;
      { ************************************************* }
      inc(percentacc);
      if percentacc>=percentinterval then
      begin
        dec(percentacc,percentinterval);
        inc(percent);
        Progress(self,psRunning,percent,'',FContinue);
        if not FContinue then exit;
      end;
      { ************************************************* }
    end;

    { build the palette }
    Result:=BuildPalette;
    if FContinue then Progress (self,psEnding,100,'',FContinue);
  finally
    DisposeNode(Root);
  end;
end;

{ TFPMedianCutQuantizer }

const DIM_ALPHA = 0;
      DIM_RED   = 1;
      DIM_GREEN = 2;
      DIM_BLUE  = 3;

constructor TFPMedianCutQuantizer.Create;
begin
  inherited Create;
  FSupportsAlpha:=true;
  FMode:=mcNormal;
end;

procedure TFPMedianCutQuantizer.SetMode(const Amode : byte);
begin
  if not (Amode in [mcSlow,mcNormal,mcFast]) then
    raise FPQuantizerException.Create('Invalid quantizer mode: '+IntToStr(Amode));
  FMode:=Amode;
end;

function TFPMedianCutQuantizer.FindLargestDimension(const Box : TMCBox) : byte;
var i : longword;
    col : TFPPackedColor;
    maxa, mina, maxr, minr, maxg, ming, maxb, minb : byte;
begin
  maxa:=0;   maxr:=0;   maxg:=0;   maxb:=0;
  mina:=$FF; minr:=$FF; ming:=$FF; minb:=$FF;
  for i:=box.startindex to box.endindex do
  begin
    col:=arr[i]^.Col;
    if col.A<mina then mina:=col.A;
    if col.A>maxa then maxa:=col.A;
    if col.R<minr then minr:=col.R;
    if col.R>maxr then maxr:=col.R;
    if col.G<ming then ming:=col.G;
    if col.G>maxg then maxg:=col.G;
    if col.B<minb then minb:=col.B;
    if col.B>maxb then maxb:=col.B;
  end;
  maxa:=maxa-mina;
  maxr:=maxr-minr;
  maxg:=maxg-ming;
  maxb:=maxb-minb;
  if ((maxa>maxr) and (maxa>maxg) and (maxa>maxb)) then Result:=DIM_ALPHA
  else if ((maxr>maxa) and (maxr>maxg) and (maxr>maxb)) then Result:=DIM_RED
  else if ((maxg>maxa) and (maxg>maxr) and (maxg>maxb)) then Result:=DIM_GREEN
  else Result:=DIM_BLUE;
end;

function TFPMedianCutQuantizer.ColorCompare(const c1, c2 : TFPPackedColor; const Dim : byte) : shortint;
var tmp : integer;
begin
  case Dim of
    DIM_ALPHA : tmp:=(c1.A-c2.A);
    DIM_RED   : tmp:=(c1.R-c2.R);
    DIM_GREEN : tmp:=(c1.G-c2.G);
    DIM_BLUE  : tmp:=(c1.B-c2.B)
  else raise FPQuantizerException.Create('Invalid dimension: '+IntToStr(Dim));
  end;
  if tmp>0 then Result:=1
  else if tmp<0 then Result:=-1
  else Result:=0;
end;

procedure TFPMedianCutQuantizer.QuickSort(const l, r : integer; const Dim : byte);
var i, j : integer;
    pivot, temp : PFPColorWeight;
begin
  if l<r then
  begin
    pivot:=arr[l];
    i:=l+1;
    j:=r;
    repeat
      while ((i<=r) and (ColorCompare(arr[i]^.Col,pivot^.Col,dim)<=0)) do
        inc(i);
      while (ColorCompare(arr[j]^.Col,pivot^.Col,dim)=1) do
        dec(j);
      if i<j then
      begin
        temp:=arr[i];
        arr[i]:=arr[j];
        arr[j]:=temp;
      end;
    until i > j;
    { don't swap if they are equal }
    if ColorCompare(arr[j]^.Col,pivot^.Col,dim)<>0 then
    begin
      arr[l]:=arr[j];
      arr[j]:=pivot;
    end;
    Quicksort(l,j-1,dim);
    Quicksort(i,r,dim);
  end;
end;

procedure TFPMedianCutQuantizer.QuickSortBoxes(const l, r : integer);
var i, j : integer;
    pivot, temp : TMCBox;
begin
  if l<r then
  begin
    pivot:=boxes[l];
    i:=l+1;
    j:=r;
    repeat
      while ((i<=r) and (boxes[i].total>=pivot.total)) do
        inc(i);
      while (boxes[j].total<pivot.total) do
        dec(j);
      if i<j then
      begin
        temp:=boxes[i];
        boxes[i]:=boxes[j];
        boxes[j]:=temp;
      end;
    until i > j;
    { don't swap if they are equal }
    if boxes[j].total<>pivot.total then
    begin
      boxes[l]:=boxes[j];
      boxes[j]:=pivot;
    end;
    QuicksortBoxes(l,j-1);
    QuicksortBoxes(i,r);
  end;
end;

function TFPMedianCutQuantizer.MeanBox(const box : TMCBox) : TFPColor;
var tota,totr,totg,totb, pixcount : longword;
    i : integer;
    col : TFPPackedColor;
    fpcol : TFPColor;
begin
  tota:=0; totr:=0; totg:=0; totb:=0; pixcount:=0;
  for i:=box.startindex to box.endindex do
  begin
    tota:=tota+(arr[i]^.Col.A*arr[i]^.Num);
    totr:=totr+(arr[i]^.Col.R*arr[i]^.Num);
    totg:=totg+(arr[i]^.Col.G*arr[i]^.Num);
    totb:=totb+(arr[i]^.Col.B*arr[i]^.Num);
    inc(pixcount,arr[i]^.Num);
  end;
  tota:=round(tota / pixcount);
  totr:=round(totr / pixcount);
  totg:=round(totg / pixcount);
  totb:=round(totb / pixcount);
  if tota>$FF then tota:=$FF;
  if totr>$FF then totr:=$FF;
  if totg>$FF then totg:=$FF;
  if totb>$FF then totb:=$FF;
  col.a:=tota;
  col.r:=totr;
  col.g:=totg;
  col.b:=totb;
  fpcol:=Packed2FPColor(col);
  if palcache.Get(fpcol)<>nil then { already found, try the middle color }
  begin
    fpcol:=Packed2FPColor(arr[(box.startindex+box.endindex) div 2]^.Col);
    if palcache.Get(fpcol)<>nil then { already found, try the first unused color }
      for i:=box.startindex to box.endindex do
      begin
        col.a:=arr[i]^.Col.A;
        col.r:=arr[i]^.Col.R;
        col.g:=arr[i]^.Col.G;
        col.b:=arr[i]^.Col.B;
        fpcol:=Packed2FPColor(col);
        if palcache.Get(fpcol)=nil then break;
      end;
  end;
  palcache.Insert(fpcol,nil);
  Result:=fpcol;
end;

function TFPMedianCutQuantizer.BuildPalette : TFPPalette;
var pal : TFPPalette;
    i : integer;
begin
  pal:=TFPPalette.Create(Used);
  try
    palcache:=TFPColorHashTable.Create;
    try
      for i:=0 to Used-1 do
      begin
        pal.Color[i]:=MeanBox(boxes[i]);
        { ************************************************* }
        inc(percentacc);
        if percentacc>=percentinterval then
        begin
          percentacc:=percentacc mod percentinterval;
          inc(percent);
          Progress(self,psRunning,percent,'',FContinue);
          if not FContinue then exit;
        end;
        { ************************************************* }
      end
    finally
      palcache.Free;
    end;
  except
    pal.Free;
    raise;
  end;
  Result:=pal;
end;

{ slow   mode: no filtering 
  normal mode: 8 bit r, 6 bit g, 6 bit b 
  fast   mode: 5 bit r, 5 bit g, 5 bit b }

const mask_r_normal = $FFFF;
      mask_g_normal = $FCFC;
      mask_b_normal = $FCFC;
      mask_r_fast   = $F8F8;
      mask_g_fast   = $F8F8;
      mask_b_fast   = $F8F8;

function TFPMedianCutQuantizer.MaskColor(const col : TFPColor) : TFPColor;
begin
  case FMode of
    mcNormal:
          begin
            Result.Red:=Col.Red and mask_r_normal;
            Result.Green:=Col.Green and mask_g_normal;
            Result.Blue:=Col.Blue and mask_b_normal;
          end;
    mcFast:
          begin
            Result.Red:=Col.Red and mask_r_fast;
            Result.Green:=Col.Green and mask_g_fast;
            Result.Blue:=Col.Blue and mask_b_fast;
          end
    else Result:=Col;
  end;
end;

function TFPMedianCutQuantizer.InternalQuantize : TFPPalette;
var box : ^TMCBox;
    i, j, k : integer;
    dim : byte;
    boxpercent : longword;
begin
  HashTable:=TFPColorHashTable.Create;
  try
  { *****************************************************************************
    Operations:
    width*height of each image (populate the hash table)
    number of desired colors for the box creation process (this should weight as the previous step)
    number of desired colors for building the palette.
  }
    percentinterval:=0;
    for k:=0 to FCount-1 do
      if FImages[k]<>nil then
        percentinterval:=percentinterval+FImages[k].Height*FImages[k].Width;
    boxpercent:=percentinterval div FColNum;
    percentinterval:=percentinterval*2+FColNum;

  percentinterval:=percentinterval div 100;  { how many operations for 1% }
  if percentinterval=0 then percentinterval:=$FFFFFFFF;  { it's quick, call progress only when starting and ending }
  percent:=0;
  percentacc:=0;
  FContinue:=true;
  Progress (self,psStarting,0,'',FContinue);
  if not FContinue then exit;
  { ***************************************************************************** }

  { For every color in the images, count how many pixels use it}
    for k:=0 to FCount-1 do
      if FImages[k]<>nil then
        for j:=0 to FImages[k].Height-1 do
          for i:=0 to FImages[k].Width-1 do
          begin
            HashTable.Add(MaskColor(FImages[k][i,j]),1);
            { ************************************************* }
            inc(percentacc);
            if percentacc>=percentinterval then
            begin
              percentacc:=percentacc mod percentinterval;
              inc(percent);
              Progress(self,psRunning,percent,'',FContinue);
              if not FContinue then exit;
            end;
            { ************************************************* }
          end;
  { Then let's have the list in array form }
    setlength(arr,0);
    arr:=HashTable.GetArray;
    try
      HashTable.Clear; { free some resources }

      setlength(boxes,FColNum);
      boxes[0].startindex:=0;
      boxes[0].endindex:=length(arr)-1;
      boxes[0].total:=boxes[0].endindex+1;
      Used:=1;

      while (used<FColNum) do
      begin
        box:=nil;
        { find a box with at least 2 colors }
        for i:=0 to Used-1 do
          if (boxes[i].total)>=2 then
          begin
            box:=@boxes[i];
            break;
          end;
        if box=nil then break;

        dim:=FindLargestDimension(box^);
        { sort the colors of the box along the largest dimension }
        QuickSort(box^.startindex,box^.endindex,dim);

        { Split the box: half of the colors in the first one, the rest in the second one }
        j:=(box^.startindex+box^.endindex) div 2;
        { This is the second box }
        boxes[Used].startindex:=j+1;
        boxes[Used].endindex:=box^.endindex;
        boxes[Used].total:=box^.endindex-j;
        { And here we update the first box }
        box^.endindex:=j;
        box^.total:=box^.endindex-box^.startindex+1;
        { Sort the boxes so that the first one is the one with higher number of colors }
        QuickSortBoxes(0,Used);
        inc(Used);

        { ************************************************* }
        inc(percentacc,boxpercent);
        if percentacc>=percentinterval then
        begin
          inc(percent,percentacc div percentinterval);
          percentacc:=percentacc mod percentinterval;
          Progress(self,psRunning,percent,'',FContinue);
          if not FContinue then exit;
        end;
        { ************************************************* }
      end;
      Result:=BuildPalette;
      if FContinue then Progress (self,psEnding,100,'',FContinue);
    finally
      setlength(boxes,0);
      for i:=0 to length(arr)-1 do
        FreeMem(arr[i]);
      setlength(arr,0);
    end;
  finally
    HashTable.Free;
  end;
end;

end.