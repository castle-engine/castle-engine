program TestProject1;

{$mode objfpc}{$H+} // you can also set ObjFpc and long strings in project compiler options

uses CastleWindow, X3DNodes, CastleTiledMap, CastleViewport, CastleScene,
  SysUtils{, CastleConvertTiledMap, castle_window};
var
  Window: TCastleWindowBase;

  Viewport: TCastleViewport;
  Scene: TCastleScene;

  ATiledMap: TTiledMap;
  ARootNode: TX3DRootNode;
begin
  SetHeapTraceOutput('heaptrc.log');

  Window := TCastleWindowBase.Create(Application);
  Window.Open;

  //TiledMap := TCastleTiledMapControl.Create(Application);
  //TiledMap.URL := 'castle-data:/test.tmx';
  //TiledMap.Width := 1000;
  //TiledMap.Height := 250;
  //TiledMap.FullSize := True;
  //Window.Controls.InsertFront(TiledMap);

  ATiledMap := TTiledMap.Create('castle-data:/map512.tmx');
  try
    ARootNode := TX3DRootNode.Create; //ConvertTiledMap(ATiledMap);
  finally
    FreeAndNil(ATiledMap);
  end;


  Viewport := TCastleViewport.Create(Application);
  Viewport.Setup2D;
  Viewport.FullSize := true;
  Viewport.AutoCamera := true; // instead of this, you could do "Viewport.Camera.SetView(...)"
  Viewport.AutoNavigation := true; // instead of this, you could do "Viewport.Navigation := ..."
  Window.Controls.InsertFront(Viewport);

  Scene := TCastleScene.Create(Application);
  Scene.Setup2D;
  Scene.Load(ARootNode, True);
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Scene.ProcessEvents := true;

  Viewport.Items.Add(Scene);
  Viewport.Items.MainScene := Scene;

  Application.Run;
end.

