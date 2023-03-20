unit ${UNIT_NAME};

interface

uses CastleTransform;

type
  ${CLASS_NAME} = class(TCastleBehavior)
  public
    procedure ParentAfterAttach; override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses CastleComponentSerialize;

procedure ${CLASS_NAME}.ParentAfterAttach;
begin
  inherited;
  { Parent is available now. }
end;

procedure ${CLASS_NAME}.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited;
  { Executed every frame, can be used to act on Parent, e.g. move it. }
end;

initialization
  { Optional: If you want to make this behavior available in the editor,
    add ${UNIT_NAME} to the editor_units="..." in CastleEngineManifest.xml
    and reopen the project.
    It will build a custom editor with this behavior available at design-time.
    See https://castle-engine.io/custom_components .

    Note that you don't *need* to do this.
    Even without any change to CastleEngineManifest.xml,
    you can use the new behavior class in code,
    e.g. create it in the view TMyView.Start method:

      My${BASE_NAME} := ${CLASS_NAME}.Create(FreeAtStop);
      MyTransform.AddBehavior(My${BASE_NAME});
  }
  RegisterSerializableComponent(${CLASS_NAME}, '${BASE_NAME}');
end.
