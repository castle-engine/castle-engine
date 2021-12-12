unit TestCastleSerialization;

interface

uses SysUtils, Classes, CastleComponentSerialize, CastleClassUtils;

procedure TestRTTICastle;

implementation

uses CastleLog;

type
  TFakeCastleScene = class(TCastleComponent)
  private
    FURL: String;
  published
    property URL: String read FURL write FURL;
  end;

  procedure TestRTTICastle;
  var
  Scene: TFakeCastleScene;
  SceneOwner: TComponent;
begin
  // InitializeLog;

  RegisterSerializableComponent(TFakeCastleScene, 'FakeScene');

  { Create TCastleTransform instance, with a TCastleScene child. }
  Scene := TFakeCastleScene.Create(nil);
  Scene.Name := 'MyScene'; // will enable to find it later with FindRequiredComponent
  Scene.URL := 'castle-data:/teapot.x3dv';

  { Save it to file. }
  ComponentSave(Scene, 'aaa.castle-transform');

  { You can destroy the instances now. }
  FreeAndNil(Scene);

  { Create a component that will own all loaded instances,
    allowing to easily free them,
    and to use FindRequiredComponent. }
  SceneOwner := TComponent.Create(nil);

  { Now you can load them from file, and check that they are equal. }
  Scene := ComponentLoad('aaa.castle-transform', SceneOwner) as TFakeCastleScene;
  WritelnLog('Found scene in loaded transform, with url %s', [
    Scene.Url
  ]);

  { Free loaded stuff. }
  FreeAndNil(SceneOwner);
end;

end.
