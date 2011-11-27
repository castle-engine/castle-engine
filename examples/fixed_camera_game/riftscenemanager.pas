unit RiftSceneManager;

interface

uses X3DNodes, CastleSceneManager;

type
  TRiftSceneManager = class(TCastleSceneManager)
    function Headlight(out CustomHeadlight: TAbstractLightNode): boolean; override;
  end;

implementation

function TRiftSceneManager.Headlight(out CustomHeadlight: TAbstractLightNode): boolean;
begin
  Result := true;
  CustomHeadlight := nil;
end;

end.
