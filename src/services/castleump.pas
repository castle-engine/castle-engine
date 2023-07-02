unit CastleUmp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TCastleUmp = class(TComponent)
  public
    constructor Create(AOwner: TComponent); override;

    procedure CheckConsent;
  end;


implementation

uses CastleMessaging;

{ TCastleUmp }

constructor TCastleUmp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

procedure TCastleUmp.CheckConsent;
begin
  Messaging.Send(['ump-check-consent']);
end;

end.

