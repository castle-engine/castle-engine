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
  TimeStart := Profiler.Start('Loading "' + URIDisplay(AURL) + '" (warming up cache)');
  try
    FRootNodeFromCache := X3DCache.Load3D(AURL);
  finally Profiler.Stop(TimeStart) end;
  Load(FRootNodeFromCache, false);
end;

destructor TSceneFromCache.Destroy;
begin
  RootNode := nil; // unassign RootNode, as X3DCache.Free3D may free it
  X3DCache.Free3D(FRootNodeFromCache);
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
  TempSceneManager: TCastleSceneManager;
begin
  URL := Element.AttributeURL('url', ElementBaseUrl);
  Scene := TSceneFromCache.Create(nil);
  Cache.OwnedObjects.Add(Scene);
  Inc(SceneCacheId);
  Cache.Name := 'SceneCache' + IntToStr(SceneCacheId);

  // Scene.URL := URL; // loads the scene
  // loads the scene using cache, this way other scenes may reload it by DeepCopy
  Scene.LoadFromCache(URL);

  TempSceneManager := TCastleSceneManager.Create(nil);
  try
    // loads the textures inside scene
    TempSceneManager.PrepareResources(Scene);
  finally FreeAndNil(TempSceneManager) end;
end;

procedure InitializeWarmupCache;
begin
  WarmupCacheFormats.RegisterFormat('scene', @TSceneCache(nil).Event);
end;
