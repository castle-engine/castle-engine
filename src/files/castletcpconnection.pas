unit castletcpconnection;

{$mode objfpc}{$H+}

interface


uses
  SysUtils
  {$ifdef ANDROID},
    CastleMessaging, CastleStringUtils
  {$endif};


type
  TRecieveMessage = procedure(const AMessage: String) of object;

{$ifdef ANDROID}
type
  TAndroidTCPConnectionService = class
    protected
      FRecieveMessage: TRecieveMessage;
      function MessageFilter (const Received: TCastleStringList): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
    public
      procedure CreateServer (const APort: Word);
      procedure CreateClient (const AHost: String; const APort: Word);
      procedure SendMessage (const AMessage: String);
    public
      property RecieveMessage: TRecieveMessage write FRecieveMessage;
  end;
{$endif}

implementation

{$ifdef ANDROID}
constructor TAndroidTCPConnectionService.Create;
begin
  Messaging.OnReceive.Add(@MessageFilter);
end;

destructor TAndroidTCPConnectionService.Destroy;
begin
  Messaging.OnReceive.Remove(@MessageFilter);

  inherited;
end;

function TAndroidTCPConnectionService.MessageFilter (const Received: TCastleStringList): Boolean;
begin
  Result := false;
  if (Received.Count = 2) and (Received[0] = 'tcp_message') then
  begin
    FRecieveMessage(Received[1]);
    Result := true;
  end;
end;

procedure TAndroidTCPConnectionService.CreateServer (const APort: Word);
begin
  Messaging.Send(['tcp_connection', 'server', IntToStr(APort)]);
end;

procedure TAndroidTCPConnectionService.CreateClient (const AHost: String; const APort: Word);
begin
  Messaging.Send(['tcp_connection', 'client', AHost, IntToStr(APort)]);
end;

procedure TAndroidTCPConnectionService.SendMessage (const AMessage: String);
begin
  Messaging.Send(['tcp_connection', 'send', AMessage]);
end;
{$endif}

end.
