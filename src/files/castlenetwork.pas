unit castlenetwork;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  {$ifdef ANDROID}
    CastleMessaging, CastleStringUtils
  {$else}
    IdTCPServer, IdTCPClient, IdContext, IdSocketHandle
  {$endif};

type
  TClientConnection = class
  protected
    {$ifndef ANDROID}
      Context: TIdContext;
    {$else}
      ID: String;
    {$endif}
  public
    constructor Create ({$ifdef ANDROID}AID: String{$else}AContext: TIdContext{$endif}); virtual;
  end;

  TProcedureObject = procedure of object;

  TConnectionEvent = procedure of object;
  TClientConnectionEvent = procedure (const AClientConnection: TClientConnection) of object;

  TMessageRecievedEvent = procedure(const AMessage: String) of object;
  TClientMessageRecievedEvent = procedure(const AMessage: String; const AClientConnection: TClientConnection) of object;

  TSynchronisedStringList = specialize TThreadList<String>;
  TClientConnectionList = specialize TThreadList<TClientConnection>; 

  {$ifndef ANDROID}
  type
    TMessageClientRecord = record
      Message: String;
      Client: TClientConnection;
    end;

    TMessageClientList = specialize TThreadList<TMessageClientRecord>;
    TClientContextList = specialize TThreadList<TIdContext>; //Because Indy version isn't generic in FPC.
  {$endif}

{$ifndef ANDROID}
  type
    TThreadSynchronisationHandler = class
    protected
      class var IsInitialised: Boolean;
      class procedure Update (Sender: TObject);
    public
      class procedure Initialise;
    end;
{$endif}

{$ifdef ANDROID}
  type
    TConnectedDictionary = specialize TDictionary<String, Boolean>;

    TAndroidTCPConnectionService = class
      strict protected
        FMessageRecieved: TClientMessageRecievedEvent;
        FOnConnected: TClientConnectionEvent;
        FConnectedDictionary: TConnectedDictionary;
        FKey: String;
        FIsActive: Boolean;
        function MessageFilter (const Received: TCastleStringList): Boolean;
      public
        constructor Create;
        destructor Destroy; override;
      public
        procedure CreateServer (const APort: Word);
        procedure CreateClient (const AHost: String; const APort: Word);
        procedure SendMessage (const AMessage: String; const AClient: TClientConnection = nil);
        procedure Close (const AClient: TClientConnection = nil);
      public
        property OnMessageRecieved: TClientMessageRecievedEvent write FMessageRecieved;
        property OnConnected: TClientConnectionEvent write FOnConnected;
      public
        function IsConnected (const AClient: TClientConnection = nil): Boolean;
    end;
{$else}
  type
    TCastleClientThread = class (TThread)
      strict protected
        FClient: TIdTCPClient;
        FOnConnected: TProcedureObject;
        FOnMessageRecieved: TProcedureObject;
      strict protected
        FMessageList: TSynchronisedStringList;
      protected
        procedure Execute; override;
      public
        constructor Create (AClient: TIdTCPClient; AOnMessageRecieved: TProcedureObject; AOnConnected: TProcedureObject); virtual;
        destructor Destroy; override;
      public
        FreeClientOnTerminate: Boolean;
        property MessageList: TSynchronisedStringList read FMessageList;
    end;
{$endif}

type
  TCastleSocket = class
  strict protected
    {$ifdef ANDROID}
      FTCPConnectionService: TAndroidTCPConnectionService;
    {$endif}
    FPort: Word;
  public
    property Port: Word read FPort write FPort;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TCastleServer = class (TCastleSocket)
  strict protected
    {$ifndef ANDROID}
      FServer: TIdTCPServer;
      FClientConnectedList: TClientContextList; //Temporary list for all NEW connected clients.
      FClientConnectionList: TClientContextList; //List for all CURRENTLY connected clients.
      FMessageList: TMessageClientList;
      procedure ServerOnConnect (AContext: TIdContext);
      procedure ServerOnDisconnect (AContext: TIdContext);
      procedure ServerOnExecute (AContext: TIdContext);
      procedure ServerOnClientConnected;
      procedure ServerOnMessageRecieved;
    {$endif}
  strict protected
     FOnConnected: TClientConnectionEvent;
     FOnMessageRecieved: TClientMessageRecievedEvent;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Disconnect (const AClient: TClientConnection); virtual;
    procedure SendToClient (const AMessage: String; const AClient: TClientConnection); virtual;
    procedure SendToAll (const AMessage: String); virtual;
  public
    function IsConnected (const AClient: TClientConnection = nil): Boolean; virtual;
  public
    property OnConnected: TClientConnectionEvent read FOnConnected write FOnConnected;
    property OnMessageRecieved: TClientMessageRecievedEvent read FOnMessageRecieved write FOnMessageRecieved;
  end;

  TCastleClient = class (TCastleSocket)
  strict protected
    FHostname: String;
    {$ifdef ANDROID}
      procedure ClientOnClientConnected (const AClientConnection: TClientConnection);
      procedure ClientOnMessageRecieved (const AMessage: String; const AClientConnection: TClientConnection);
    {$else}
      FClient: TIdTCPClient;
      FClientThread: TCastleClientThread;
      procedure ClientOnMessageRecieved;
    {$endif}
  strict protected
    FOnConnected: TConnectionEvent;
    FOnMessageRecieved: TMessageRecievedEvent;
  public
    property Hostname: String read FHostname write FHostname;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Send (const AMessage: String); virtual;
  public
    function IsConnected: Boolean; virtual;
  public
    property OnConnected: TConnectionEvent read FOnConnected write FOnConnected;
    property OnMessageRecieved: TMessageRecievedEvent read FOnMessageRecieved write FOnMessageRecieved;
  end;

implementation

uses
  CastleApplicationProperties;

//
//TThreadSynchronisationHandler
//

{$ifndef ANDROID}
  class procedure TThreadSynchronisationHandler.Update (Sender: TObject);
  begin
    CheckSynchronize; //This has to be done in the main thread regularly to enable Synchronisation between threads.
  end;

  class procedure TThreadSynchronisationHandler.Initialise;
  begin
    if not IsInitialised then
    begin
      ApplicationProperties.OnUpdate.Add({$ifdef CASTLE_OBJFPC}@{$endif}Update);
      IsInitialised := true;
    end;
  end;
{$endif}

//
//TAndroidTCPConnectionService
//

{$ifdef ANDROID}
  constructor TAndroidTCPConnectionService.Create;
  begin
    FConnectedDictionary := TConnectedDictionary.Create;

    FKey := IntToHex(PtrUInt(Self), 1); //Key used for the identification is the unique pointer value of the class instance as hex string.
    Messaging.OnReceive.Add(@MessageFilter);
  end;

  destructor TAndroidTCPConnectionService.Destroy;
  begin
    Close;

    Messaging.OnReceive.Remove(@MessageFilter);

    FConnectedDictionary.Free;

    inherited;
  end;

  function TAndroidTCPConnectionService.MessageFilter (const Received: TCastleStringList): Boolean;
  begin
    Result := false;
    if (Received.Count = 3) and (Received[0] = 'tcp_message') then //tcp_message message clientid
    begin
      if Assigned(FMessageRecieved) then
        FMessageRecieved(Received[1], TClientConnection.Create(Received[2]));
      Result := true;
    end
    else if (Received.Count = 2) and (Received[0] = 'tcp_connected') then //tcp_connected clientid
    begin
      FConnectedDictionary.AddOrSetValue(Received[1], true);
      if Assigned(FOnConnected) then
        FOnConnected(TClientConnection.Create(Received[1]));
      Result := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateServer (const APort: Word);
  begin
    if not FIsActive then
      Messaging.Send(['tcp_connection', FKey, 'server', IntToStr(APort)]);
  end;

  procedure TAndroidTCPConnectionService.CreateClient (const AHost: String; const APort: Word);
  begin
    if not FIsActive then
      Messaging.Send(['tcp_connection', FKey, 'client', AHost, IntToStr(APort)]);
  end;

  procedure TAndroidTCPConnectionService.SendMessage (const AMessage: String; const AClient: TClientConnection = nil);
  var
    AClientID: String;
  begin
    if FIsActive then
    begin
      if Assigned(AClient) then
        AClientID := AClient.ID
      else
        AClientID := 'all';

      Messaging.Send(['tcp_connection', FKey, 'send', AMessage, AClientID]);
    end;
  end;

  procedure TAndroidTCPConnectionService.Close (const AClient: TClientConnection = nil);
  var
    AClientID: String;
  begin
    if FIsActive then
    begin
      if Assigned(AClient) then
        AClientID := AClient.ID
      else
        AClientID := 'all';

      Messaging.Send(['tcp_connection', FKey, 'close', AClientID]);

      FConnectedDictionary.Clear;
      FIsActive := false;
    end;
  end;

  function TAndroidTCPConnectionService.IsConnected (const AClient: TClientConnection = nil): Boolean;
  var
    AClientID: String;
  begin
    if Assigned(AClient) then
      AClientID := AClient.ID
    else
      AClientID := 'client';

    Result := false;
    FConnectedDictionary.TryGetValue(AClientID, Result);
  end;
{$endif}

//
//TCastleClientThread
//

{$ifndef ANDROID}
  constructor TCastleClientThread.Create (AClient: TIdTCPClient; AOnMessageRecieved: TProcedureObject; AOnConnected: TProcedureObject);
  begin
    FClient := AClient;
    FOnMessageRecieved := AOnMessageRecieved;
    FOnConnected := AOnConnected;

    FMessageList := TSynchronisedStringList.Create;

    FreeOnTerminate := true;
    inherited Create(false);
  end;

  destructor TCastleClientThread.Destroy;
  begin
    FMessageList.Free;

    inherited;
  end;

  procedure TCastleClientThread.Execute;
  var
    LMessage: String;
  begin
    FClient.Connect;

    if Assigned(FOnConnected) then
      FOnConnected;

    while not Terminated do
    begin
      LMessage := FClient.IOHandler.ReadLn('', 100); //Result is empty when timeout reached.

      if LMessage <> '' then
      begin
        FMessageList.Add(LMessage);
        Queue(FOnMessageRecieved);
      end
      else if not FClient.Connected then
        FClient.Disconnect;
    end;

    FClient.Disconnect;
    if FreeClientOnTerminate then
      FClient.Free;
  end;
{$endif}

//
//TClientConnection
//

constructor TClientConnection.Create ({$ifdef ANDROID}AID: String{$else}AContext: TIdContext{$endif});
begin
  {$ifdef ANDROID}
    ID := AID;
  {$else}
    Context := AContext;
  {$endif}
end;

//
//TCastleSocket
//

constructor TCastleSocket.Create;
begin
  {$ifdef ANDROID}
    FTCPConnectionService := TAndroidTCPConnectionService.Create;
  {$endif}
end;

destructor TCastleSocket.Destroy;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Free;
  {$endif}

  inherited;
end;

//
//TCastleServer
//

constructor TCastleServer.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FClientConnectedList := TClientContextList.Create;
    FClientConnectionList := TClientContextList.Create;
    FMessageList := TMessageClientList.Create;
    FServer := TIdTCPServer.Create;
    FServer.OnConnect := @ServerOnConnect;
    FServer.OnDisconnect := @ServerOnDisconnect;
    FServer.OnExecute := @ServerOnExecute;
  {$endif}
end;

destructor TCastleServer.Destroy;
begin
  {$ifndef ANDROID}
    FServer.Free;
    FClientConnectedList.Free;
    FClientConnectionList.Free;
    FMessageList.Free;
  {$endif}

  inherited;
end;

procedure TCastleServer.Start;
{$ifdef UNIX}{$ifndef ANDROID}
  var
    Binding: TIdSocketHandle;
{$endif}{$endif}
begin
  {$ifdef ANDROID}
    FTCPConnectionService.OnMessageRecieved := FOnMessageRecieved;
    FTCPConnectionService.OnConnected := FOnConnected;
    FTCPConnectionService.CreateServer(FPort);
  {$else}
    {$ifdef UNIX}
      Binding := FServer.Bindings.Add;
      Binding.IP := '0.0.0.0';
      Binding.Port := FPort;
    {$else}
      FServer.DefaultPort := FPort;
    {$endif}
    FServer.Active := true;
  {$endif}
end;

procedure TCastleServer.Stop;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close;
  {$else}
    FServer.Active := false;
  {$endif}
end;

procedure TCastleServer.Disconnect (const AClient: TClientConnection);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close (AClient);
  {$else}
    AClient.Context.Connection.Disconnect;
  {$endif}
end;

procedure TCastleServer.SendToClient (const AMessage: String; const AClient: TClientConnection);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage, AClient);
  {$else}
    AClient.Context.Connection.IOHandler.WriteLn(AMessage);
  {$endif}
end;

procedure TCastleServer.SendToAll (const AMessage: String);
{$ifndef ANDROID}
  var
    LContext: TIdContext;
{$endif}
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage);
  {$else}
    for LContext in FClientConnectionList.LockList do
      LContext.Connection.IOHandler.WriteLn(AMessage);
    FClientConnectionList.UnlockList;
  {$endif}
end;

function TCastleServer.IsConnected (const AClient: TClientConnection = nil): Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected(AClient);
  {$else}
    if Assigned(AClient) then
    begin
      Result := FClientConnectionList.LockList.IndexOf(AClient.Context) > -1;
      FClientConnectionList.UnlockList;
    end
    else
    begin
      Result := FClientConnectionList.LockList.Count > 0;
      FClientConnectionList.UnlockList;
    end;
  {$endif}
end;

{$ifndef ANDROID}
  procedure TCastleServer.ServerOnConnect (AContext: TIdContext);
  begin
    FClientConnectionList.Add(AContext);

    if Assigned(FOnConnected) then
      TThread.Queue(nil, @ServerOnClientConnected);
  end;

  procedure TCastleServer.ServerOnDisconnect (AContext: TIdContext);
  begin
    FClientConnectionList.Remove(AContext);
  end;

  procedure TCastleServer.ServerOnExecute (AContext: TIdContext);
  var
    LMessageClientRecord: TMessageClientRecord;
  begin
    LMessageClientRecord.Message := AContext.Connection.IOHandler.ReadLn;
    LMessageClientRecord.Client := TClientConnection.Create(AContext);

    FMessageList.Add(LMessageClientRecord);

    TThread.Queue(nil, @ServerOnMessageRecieved);
  end;

  procedure TCastleServer.ServerOnClientConnected;
  var
    LContext: TIdContext;
  begin
    if Assigned(FOnConnected) then
    begin
      for LContext in FClientConnectedList.LockList do
        FOnConnected(TClientConnection.Create(LContext));
      FClientConnectedList.Clear;
      FClientConnectedList.UnlockList;
    end;  
  end;

  procedure TCastleServer.ServerOnMessageRecieved;
  var
    LMessageClientRecord: TMessageClientRecord;
  begin
    if Assigned(FOnMessageRecieved) then
    begin
      for LMessageClientRecord in FMessageList.LockList do
        FOnMessageRecieved(LMessageClientRecord.Message, LMessageClientRecord.Client);
      FMessageList.Clear;
      FMessageList.UnlockList;
    end;
  end;
{$endif}

//
//TCastleClient
//

constructor TCastleClient.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FClient := TIdTCPClient.Create;
  {$endif}
end;

destructor TCastleClient.Destroy;
begin
  {$ifndef ANDROID}
    FClientThread.FreeClientOnTerminate := true;
    FClientThread.Terminate;
  {$endif}
  
  inherited;
end;

procedure TCastleClient.Connect;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.OnConnected := @ClientOnClientConnected;
    FTCPConnectionService.OnMessageRecieved := @ClientOnMessageRecieved;
    FTCPConnectionService.CreateClient(FHostname, FPort);
  {$else}
    FClient.Port := FPort;
    FClient.Host := FHostname;
    FClient.ConnectTimeout := 5000;

    FClientThread := TCastleClientThread.Create(FClient, @ClientOnMessageRecieved, FOnConnected);
  {$endif}
end;

procedure TCastleClient.Disconnect;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close;
  {$else}
    FClientThread.Terminate;
  {$endif}
end;

procedure TCastleClient.Send(const AMessage: String);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage);
  {$else}
    FClient.IOHandler.WriteLn(AMessage);
  {$endif}
end;

function TCastleClient.IsConnected: Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected;
  {$else}
    Result := FClient.Connected;
  {$endif}
end;

{$ifdef ANDROID}
  procedure TCastleClient.ClientOnClientConnected (const AClientConnection: TClientConnection);
  begin
    FOnConnected();
  end;

  procedure TCastleClient.ClientOnMessageRecieved (const AMessage: String; const AClientConnection: TClientConnection);
  begin
    FOnMessageRecieved(AMessage);
  end;
{$else}
  procedure TCastleClient.ClientOnMessageRecieved;
  var
    LMessage: String;
  begin
    if Assigned(FOnMessageRecieved) then
    begin
      for LMessage in FClientThread.MessageList.LockList do
        FOnMessageRecieved(LMessage);
      FClientThread.MessageList.Clear;
      FClientThread.MessageList.UnlockList;
    end;
  end;
{$endif}

end.
