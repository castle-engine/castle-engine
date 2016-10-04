unit RiftSceneManager;

interface

uses X3DNodes, CastleSceneManager;

type
  TRiftSceneManager = class(TCastleSceneManager)
  private
    DefaultHeadlightNode: TDirectionalLightNode;
  protected
    function Headlight: TAbstractLightNode; override;
  public
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

function TRiftSceneManager.Headlight: TAbstractLightNode;
begin
  if DefaultHeadlightNode = nil then
    DefaultHeadlightNode := TDirectionalLightNode.Create;
  Result := DefaultHeadlightNode;
end;

destructor TRiftSceneManager.Destroy;
begin
  FreeAndNil(DefaultHeadlightNode);
  inherited;
end;

end.
