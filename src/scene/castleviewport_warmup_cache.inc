{%MainUnit castleviewport.pas}
{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

{$endif read_interface}

{$ifdef read_implementation}

{ TSceneFromCache ------------------------------------------------------------ }

type
  { Like TCastleScene but loads/unloads model in a special way, from X3DCache.
    This allows to utilize this cache when loading, in TCastleScene.Load. }
  TSceneFromCache = class(TCastleScene)
  strict private
    FRootNodeFromCache: TX3DRootNode;
  public
    procedure LoadFromCache(const AURL: String);
    destructor Destroy; override;
  end;

procedure TSceneFromCache.LoadFromCache(const AURL: String);
var
  TimeStart: TCastleProfilerTime;
begin
  { Do not load node again, if it is already in the cache.
    It would cause warnings
      Warning: X3D node xxx is already part of another TCastleScene instance
    if user uses the same URL twice in warmup_cache specification. }
  if X3DCache.IsCached(AURL) then Exit;

  TimeStart := Profiler.Start('Loading "' + UriDisplay(AURL) + '" (warming up cache)');
  try
    FRootNodeFromCache := X3DCache.LoadNode(AURL);
  finally Profiler.Stop(TimeStart) end;
  Load(FRootNodeFromCache, false);
end;

destructor TSceneFromCache.Destroy;
begin
  RootNode := nil; // unassign RootNode, as X3DCache.FreeNode may free it
  X3DCache.FreeNode(FRootNodeFromCache);
  inherited;
end;

{ TSceneCache -------------------------------------------------------------- }

var
  SceneCacheId: QWord;

type
  TSceneCache = class
    class procedure Event(const Cache: TWarmupCache;
      const Element: TDOMElement; const ElementBaseUrl: String);
  end;

class procedure TSceneCache.Event(const Cache: TWarmupCache;
  const Element: TDOMElement; const ElementBaseUrl: String);
var
  URL: String;
  Scene: TSceneFromCache;
  TempViewport: TCastleViewport;
begin
  URL := Element.AttributeURL('url', ElementBaseUrl);
  Scene := TSceneFromCache.Create(nil);
  Cache.OwnedObjects.Add(Scene);
  Inc(SceneCacheId);
  Cache.Name := 'SceneCache' + IntToStr(SceneCacheId);

  // Scene.URL := URL; // loads the scene
  // loads the scene using cache, this way other scenes may reload it by DeepCopy
  Scene.LoadFromCache(URL);

  TempViewport := TCastleViewport.Create(nil);
  try
    // loads the textures inside scene
    TempViewport.PrepareResources(Scene, TCastleViewport.DefaultPrepareOptions - [prRenderSelf]);
  finally FreeAndNil(TempViewport) end;
end;

procedure InitializeWarmupCache;
begin
  WarmupCacheFormats.RegisterFormat('scene', {$ifdef FPC}@{$endif}TSceneCache{$ifdef FPC}(nil){$endif}.Event);
end;

{$endif read_implementation}
