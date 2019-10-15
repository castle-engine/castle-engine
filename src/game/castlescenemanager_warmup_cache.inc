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
  Scene: TCastleScene;
  TempSceneManager: TCastleSceneManager;
begin
  URL := Element.AttributeURL('url', ElementBaseUrl);
  Scene := TCastleScene.Create(nil);
  Cache.OwnedObjects.Add(Scene);
  Inc(SceneCacheId);
  Cache.Name := 'SceneCache' + IntToStr(SceneCacheId);
  Scene.URL := URL; // loads the scene

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
