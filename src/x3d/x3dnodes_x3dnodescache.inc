{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { @exclude Internal for TX3DNodesCache. }
  TCachedNode = class
  private
    URL: string;
    References: Cardinal;
    Node: TX3DRootNode;
  end;

  TCachedNodeList = class(specialize TObjectList<TCachedNode>)
    procedure Pack;
  end;

  { Cache for resources not specific to renderer (OpenGL).
    Includes all TTexturesVideosCache resources (texture, movie
    data) and adds cache for 3D models. }
  TX3DNodesCache = class(TTexturesVideosCache)
  strict private
    CachedNodes: TCachedNodeList;
    InsideFree3DNodeDelete: boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { Load 3D model, just like Load3D but with a cache.
      URL must be absolute (not relative).

      Note that this should not be used if you plan to modify the model graph
      (for example by VRML/X3D events). In such case, the cache should not
      be used, as it would make all the model instances shared.
      For example, if you inline the same model multiple times, you could not
      modify one instance independent from another. }
    function Load3D(const URL: string): TX3DRootNode;

    { Unload previously loaded here 3D model.
      Node may be @nil (then it's ignored), or something loaded by
      Load3D (then it's released and changed to @nil). }
    procedure Free3D(var Node: TX3DRootNode);

    function Empty: boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCachedNodeList ------------------------------------------------------------ }

procedure TCachedNodeList.Pack;
begin
  while Remove(nil) <> -1 do ;
end;

{ TX3DNodesCache ------------------------------------------------------------ }

{ $define DEBUG_CACHE}

var
  CurrentlyLoading: TCastleStringList;

constructor TX3DNodesCache.Create;
begin
  inherited;
  CachedNodes := TCachedNodeList.Create;
end;

destructor TX3DNodesCache.Destroy;
begin
  if CachedNodes <> nil then
  begin
    CachedNodes.Pack; { remove nil items, see InsideFree3DNodeDelete mechanism }
    Assert(CachedNodes.Count = 0, 'Some references to 3D models still exist when freeing TX3DNodesCache');
    FreeAndNil(CachedNodes);
  end;
  inherited;
end;

function TX3DNodesCache.Load3D(const URL: string): TX3DRootNode;
var
  I, Index: Integer;
  C: TCachedNode;
begin
  for I := 0 to CachedNodes.Count - 1 do
  begin
    C := CachedNodes[I];
    if (C <> nil) and (C.URL = URL) then
    begin
      Inc(C.References);

      {$ifdef DEBUG_CACHE}
      Writeln('++ : 3D model ', URL, ' : ', C.References);
      {$endif}

      Exit(C.Node);
    end;
  end;

  { Add URL to CurrentlyLoading, detecting an infinite loop,
    see https://sourceforge.net/p/castle-engine/tickets/11/ }
  if CurrentlyLoading.IndexOf(URL) <> -1 then
  begin
    raise EX3DError.CreateFmt('3D model references itself (through EXTERNPROTO or Inline), cannot load: %s',
      [URL]);
  end;
  CurrentlyLoading.Add(URL);

  { Initialize Result first, before calling CachedNodes.Add.
    That's because in case Load3D raises exception,
    we don't want to add image to cache (because caller would have
    no way to call Free3D later). }

  Result := X3DLoad.Load3D(URL, false);

  { Remove URL from CurrentlyLoading }
  Index := CurrentlyLoading.IndexOf(URL);
  Assert(Index <> -1);
  CurrentlyLoading.Delete(Index);

  C := TCachedNode.Create;
  CachedNodes.Add(C);
  C.References := 1;
  C.URL := URL;
  C.Node := Result;

  {$ifdef DEBUG_CACHE}
  Writeln('++ : 3D model ', URL, ' : ', 1);
  {$endif}
end;

procedure TX3DNodesCache.Free3D(var Node: TX3DRootNode);
var
  I: Integer;
  C: TCachedNode;
begin
  if Node = nil then Exit;

  for I := 0 to CachedNodes.Count - 1 do
  begin
    C := CachedNodes[I];
    if (C <> nil) and (C.Node = Node) then
    begin
      {$ifdef DEBUG_CACHE}
      Writeln('-- : 3D model ', C.URL, ' : ', C.References - 1);
      {$endif}

      Node := nil;

      if C.References = 1 then
      begin
        if InsideFree3DNodeDelete then
        begin
          { Deleting a node may cause recursive Free3D call that may also remove
            something, and shift our indexes.
            So only nil the item.
            Testcase when it's needed:
            http://www.web3d.org/x3d/content/examples/Basic/CAD/_pages/page02.html }
          FreeAndNil(C.Node);
          CachedNodes[I] := nil;
        end else
        begin
          InsideFree3DNodeDelete := true;
          FreeAndNil(C.Node);
          CachedNodes.Delete(I);
          InsideFree3DNodeDelete := false;
        end;
        CheckEmpty;
      end else
        Dec(C.References);

      Exit;
    end;
  end;

  raise EInternalError.CreateFmt('Free3D: no reference found for 3D model %s',
    [PointerToStr(Node)]);
end;

function TX3DNodesCache.Empty: boolean;
begin
  CachedNodes.Pack; { remove nil items, see InsideFree3DNodeDelete mechanism }
  Result := (inherited Empty) and (CachedNodes.Count = 0);
end;

{$endif read_implementation}
