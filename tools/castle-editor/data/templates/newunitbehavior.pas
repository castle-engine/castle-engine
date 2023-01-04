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

end.
