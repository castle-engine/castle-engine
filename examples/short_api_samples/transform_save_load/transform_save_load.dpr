{ TransformSave and TransformLoad example. }

{$apptype CONSOLE}

uses SysUtils, Classes,
  CastleLog, CastleVectors, CastleTransform, CastleScene, CastleComponentSerialize;
var
  Scene: TCastleScene;
  Transform: TCastleTransform;
  TransformOwner: TComponent;
begin
  InitializeLog;

  { Create TCastleTransform instance, with a TCastleScene child. }
  Scene := TCastleScene.Create(nil);
  Scene.Name := 'MyScene'; // will enable to find it later with FindRequiredComponent
  Scene.Load('castle-data:/teapot.x3dv');
  Transform := TCastleTransform.Create(nil);
  Transform.Translation := Vector3(1, 2, 3);
  Transform.Add(Scene);

  { Save it to file. }
  TransformSave(Transform, 'aaa.castle-transform');

  { You can destroy the instances now. }
  FreeAndNil(Scene);
  FreeAndNil(Transform);

  { Create a component that will own all loaded instances,
    allowing to easily free them,
    and to use FindRequiredComponent. }
  TransformOwner := TComponent.Create(nil);

  { Now you can load them from file, and check that they are equal. }
  Transform := TransformLoad('aaa.castle-transform', TransformOwner);
  WritelnLog('Loaded transform, with translation %s, with %d children', [
    Transform.Translation.ToString,
    Transform.Count
  ]);
  Scene := TransformOwner.FindRequiredComponent('MyScene') as TCastleScene;
  WritelnLog('Found scene in loaded transform, with url %s', [
    Scene.Url
  ]);

  { Free loaded stuff. }
  FreeAndNil(TransformOwner);
end.
