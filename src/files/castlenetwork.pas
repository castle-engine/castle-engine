unit castletcpconnection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  {$ifdef ANDROID},
    CastleMessaging, CastleStringUtils
  {$endif};

type
  TMessageRecieved = procedure(const AMessage: String) of object;
  TProcedureObject = procedure of object;

{$ifdef ANDROID}
type
  TAndroidTCPConnectionService = class
    protected
      FMessageRecieved: TMessageRecieved;
      FConnected: TProcedureObject;
      FIsConnected: Boolean;
      function MessageFilter (const Received: TCastleStringList): Boolean;
    public
      constructor Create;
      destructor Destroy; override;
    public
      procedure CreateServer (const APort: Word);
      procedure CreateClient (const AHost: String; const APort: Word);
      procedure SendMessage (const AMessage: String);
    public
      property OnMessageRecieved: TMessageRecieved write FMessageRecieved;
      property OnConnected: TProcedureObject write FConnected;
      property IsConnected: Boolean read FIsConnected;
  end;
{$endif}

implementation

{$ifdef ANDROID}
uses
  CastleApplicationProperties;
{$endif}

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
    FMessageRecieved(Received[1]);
    Result := true;
  end
  else if (Received.Count = 1) and (Received[0] = 'tcp_connected') then
  begin
    FIsConnected := true;
    FConnected;
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
