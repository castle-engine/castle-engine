{%MainUnit x3dnodes.pas}
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

  { Cache for X3D nodes.
    Descends from TTexturesVideosCache, so can also cache textures and movies. }
  TX3DNodesCache = class(TTexturesVideosCache)
  strict private
    type
      TCachedNode = class
      private
        URL: String;
        References: Cardinal;
        Node: TX3DRootNode;
      end;
      TCachedNodeDictionary = {$ifdef FPC}specialize{$endif} TObjectDictionary<String, TCachedNode>;
    var
      CachedNodes: TCachedNodeDictionary;
    function FindCachedNode(const Node: TX3DRootNode): TCachedNode;
  public
    Log: Boolean;

    constructor Create;
    destructor Destroy; override;

    { Load model, just like @link(X3DLoad.LoadNode) but with a cache.
      URL must be absolute (not relative).

      Note that this should not be used if you plan to modify the model graph
      (for example by VRML/X3D events). In such case, the cache should not
      be used, as it would make all the model instances shared.
      For example, if you inline the same model multiple times, you could not
      modify one instance independent from another. }
    function LoadNode(const URL: String): TX3DRootNode;

    { Is the given URL cached. }
    function IsCached(const URL: String): Boolean;

    { If the given URL is cached, return a DeepCopy of the node from cache.
      Otherwise returns @nil.
      This makes sense if you possibly already have this model in the cache,
      and now you want to load it again, *and* modify the X3D nodes tree afterwards. }
    function TryCopyNode(const URL: String): TX3DRootNode;

    { Unload previously loaded here model.
      Node may be @nil (then it's ignored), or something loaded by
      LoadNode (then it's released and changed to @nil). }
    procedure FreeNode(var Node: TX3DRootNode);

    function Empty: boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DNodesCache ------------------------------------------------------------ }

var
  CurrentlyLoading: TCastleStringList;

constructor TX3DNodesCache.Create;
begin
  inherited;
  CachedNodes := TCachedNodeDictionary.Create([doOwnsValues]);
end;

destructor TX3DNodesCache.Destroy;
begin
  if CachedNodes <> nil then
  begin
    Assert(CachedNodes.Count = 0, 'Some references to nodes still exist when freeing TX3DNodesCache');
    FreeAndNil(CachedNodes);
  end;
  inherited;
end;

function TX3DNodesCache.TryCopyNode(const URL: String): TX3DRootNode;
var
  C: TCachedNode;
begin
  if CachedNodes.TryGetValue(URL, C) then
    Result := C.Node.DeepCopy as TX3DRootNode
  else
    Result := nil;
end;

function TX3DNodesCache.LoadNode(const URL: String): TX3DRootNode;
var
  Index: Integer;
  C: TCachedNode;
begin
  if CachedNodes.TryGetValue(URL, C) then
  begin
    Inc(C.References);

    if Log then
      WritelnLog('++', 'Node %s : %d', [URIDisplay(URL), C.References]);

    Exit(C.Node);
  end;

  { Add URL to CurrentlyLoading, detecting an infinite loop,
    see https://sourceforge.net/p/castle-engine/tickets/11/ }
  if CurrentlyLoading.IndexOf(URL) <> -1 then
  begin
    raise EX3DError.CreateFmt('Model references itself (through EXTERNPROTO or Inline), cannot load: %s',
      [URL]);
  end;
  CurrentlyLoading.Add(URL);

  { Initialize Result first, before calling CachedNodes.Add.
    That's because in case LoadNode raises exception,
    we don't want to add image to cache (because caller would have
    no way to call FreeNode later). }

  Result := X3DLoad.LoadNode(URL);

  { Remove URL from CurrentlyLoading }
  Index := CurrentlyLoading.IndexOf(URL);
  Assert(Index <> -1);
  CurrentlyLoading.Delete(Index);

  C := TCachedNode.Create;
  CachedNodes.Add(URL, C);
  C.References := 1;
  C.URL := URL;
  C.Node := Result;

  if Log then
    WritelnLog('++', 'Node %s : %d', [URIDisplay(URL), 1]);
end;

function TX3DNodesCache.IsCached(const URL: String): Boolean;
begin
  Result := CachedNodes.ContainsKey(URL);
end;

function TX3DNodesCache.FindCachedNode(const Node: TX3DRootNode): TCachedNode;
begin
  for Result in CachedNodes.Values do
    if Result.Node = Node then
      Exit;
  Result := nil;
end;

procedure TX3DNodesCache.FreeNode(var Node: TX3DRootNode);
var
  URL: String;
  C: TCachedNode;
begin
  if Node = nil then Exit;

  C := FindCachedNode(Node);
  if C = nil then
    raise EInternalError.CreateFmt('FreeNode: no reference found for node %s',
      [PointerToStr(Node)]);

  if Log then
    WritelnLog('--', 'Node %s : %d', [URIDisplay(C.URL), C.References - 1]);

  Node := nil;

  if C.References = 1 then
  begin
    URL := C.URL;

    { Note that deleting a node may cause recursive FreeNode call that may also remove
      something, and shift our indexes.
      Testcase when it's needed (TODO link not working anymore, find this):
      http://www.web3d.org/x3d/content/examples/Basic/CAD/_pages/page02.html
      Current code works regardless of this. }
    FreeAndNil(C.Node);

    CachedNodes.Remove(URL);
    CheckEmpty;
  end else
    Dec(C.References);
end;

function TX3DNodesCache.Empty: boolean;
begin
  Result := (inherited Empty) and (CachedNodes.Count = 0);
end;

{$endif read_implementation}
