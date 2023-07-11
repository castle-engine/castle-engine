{
  Copyright 2018 Benedikt Magnus.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Client and server for network communication using TCP (TCastleTCPClient, TCastleTCPServer).
  Use this in your games for easy client/server communication.
  See https://castle-engine.io/manual_network.php#section_multi_player .

  On Android, it requires adding
  @url(https://castle-engine.io/android-Services
  client_server service in your project).
  On other platforms, it requires having @url(http://www.indyproject.org/ Indy) available. }
unit CastleClientServer;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  {$ifdef ANDROID}
    CastleMessaging, CastleStringUtils
  {$else}
    IdTCPServer, IdTCPClient, IdContext, IdSocketHandle, IdGlobal
  {$endif};

type
  TClientConnection = record
  private
    {$ifndef ANDROID}
      Context: TIdContext;
    {$else}
      ID: String;
    {$endif}
  public
    constructor Create({$ifdef ANDROID}AID: String{$else}AContext: TIdContext{$endif});
  end;

  TProcedureObject = procedure of object;

  TConnectionEvent = procedure of object;
  TClientConnectionEvent = procedure(AClientConnection: TClientConnection) of object;

  TMessageReceivedEvent = procedure(const AMessage: String) of object;
  TClientMessageReceivedEvent = procedure(const AMessage: String; AClientConnection: TClientConnection) of object;

  TSynchronisedStringList = {$ifdef FPC}specialize{$endif} TThreadList<String>;

{$ifndef ANDROID}
type
  TMessageClientRecord = record
    Message: String;
    Client: TClientConnection;
  end;

  TMessageClientList = {$ifdef FPC}specialize{$endif} TThreadList<TMessageClientRecord>;
  TClientContextList = {$ifdef FPC}specialize{$endif} TThreadList<TIdContext>; //Because Indys version isn't generic in FPC.
{$endif}

{$ifndef ANDROID}
  type
    TThreadSynchronisationHandler = class
    protected
      class var IsInitialised: Boolean;
      class procedure Update(Sender: TObject);
    public
      class procedure Initialise;
    end;
{$endif}

{$ifdef ANDROID}
  type
    TConnectedDictionary = specialize TDictionary<String, Boolean>;

    TAndroidTCPConnectionService = class
      strict protected
        const AndroidServiceName = 'client-server';
      strict protected
        FMessageReceived: TClientMessageReceivedEvent;
        FOnConnected: TClientConnectionEvent;
        FOnDisconnected: TClientConnectionEvent;
        FConnectedDictionary: TConnectedDictionary;
        FKey: String;
        FIsActive: Boolean;
        function MessageFilter(const Received: TCastleStringList;
          const ReceivedStream: TMemoryStream): Boolean;
      public
        constructor Create;
        destructor Destroy; override;
      public
        procedure CreateServer(const APort: Word);
        procedure CreateClient(const AHost: String; const APort: Word);
        procedure SendMessage(const AMessage: String); overload;
        procedure SendMessage(const AMessage: String; AClient: TClientConnection); overload;
        procedure Close; overload;
        procedure Close(AClient: TClientConnection); overload;
      public
        property OnConnected: TClientConnectionEvent write FOnConnected;
        property OnDisconnected: TClientConnectionEvent write FOnDisconnected;
        property OnMessageReceived: TClientMessageReceivedEvent write FMessageReceived;
        {$ifdef FPC} property OnMessageRecieved: TClientMessageReceivedEvent write FMessageReceived; deprecated 'use OnMessageReceived'; {$endif}
      public
        function IsConnected: Boolean; overload;
        function IsConnected(AClient: TClientConnection): Boolean; overload;
    end;
{$else}
  type
    TCastleTCPClientThread = class(TThread)
      strict protected
        FClient: TIdTCPClient;
        FOnConnected: TProcedureObject;
        FOnDisconnected: TProcedureObject;
        FOnMessageReceived: TProcedureObject;
      strict protected
        FMessageList: TSynchronisedStringList;
      protected
        procedure Execute; override;
      public
        constructor Create(const AClient: TIdTCPClient; const AOnMessageReceived, AOnConnected, AOnDisconnected: TProcedureObject); virtual;
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

  TCastleTCPServer = class(TCastleSocket)
  strict protected
    {$ifndef ANDROID}
      FServer: TIdTCPServer;
      FClientConnectedList: TClientContextList; //Temporary list for all NEW connected clients.
      FClientDisconnectedList: TClientContextList; //Same for all disconnected clients.
      FClientConnectionsList: TClientContextList; //List for all CURRENTLY connected clients.
      FMessageList: TMessageClientList;
      procedure ServerOnConnect(AContext: TIdContext);
      procedure ServerOnDisconnect(AContext: TIdContext);
      procedure ServerOnExecute(AContext: TIdContext);
      procedure ServerOnClientConnected;
      procedure ServerOnClientDisconnected;
      procedure ServerOnMessageReceived;
    {$endif}
  strict protected
     FOnConnected: TClientConnectionEvent;
     FOnDisconnected: TClientConnectionEvent;
     FOnMessageReceived: TClientMessageReceivedEvent;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Disconnect(AClient: TClientConnection); virtual;
    procedure SendToClient(const AMessage: String; AClient: TClientConnection); virtual;
    procedure SendToAll(const AMessage: String); virtual;
  public
    function IsConnected: Boolean; overload; virtual;
    function IsConnected(AClient: TClientConnection): Boolean; overload; virtual;
  public
    property OnConnected: TClientConnectionEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TClientConnectionEvent read FOnDisconnected write FOnDisconnected;
    property OnMessageReceived: TClientMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    {$ifdef FPC} property OnMessageRecieved: TClientMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived; deprecated 'use OnMessageReceived'; {$endif}
  end;

  TCastleTCPClient = class(TCastleSocket)
  strict protected
    FHostname: String;
    {$ifdef ANDROID}
      procedure ClientOnClientConnected(AClientConnection: TClientConnection);
      procedure ClientOnClientDisconnected(AClientConnection: TClientConnection);
      procedure ClientOnMessageReceived(const AMessage: String; AClientConnection: TClientConnection);
    {$else}
      FClient: TIdTCPClient;
      FClientThread: TCastleTCPClientThread;
      procedure ClientOnMessageReceived;
    {$endif}
  strict protected
    FOnConnected: TConnectionEvent;
    FOnDisconnected: TConnectionEvent;
    FOnMessageReceived: TMessageReceivedEvent;
  public
    property Hostname: String read FHostname write FHostname;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Send(const AMessage: String); virtual;
  public
    function IsConnected: Boolean; virtual;
  public
    property OnConnected: TConnectionEvent read FOnConnected write FOnConnected;
    property OnDisconnected: TConnectionEvent read FOnDisconnected write FOnDisconnected;
    property OnMessageReceived: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived;
    {$ifdef FPC} property OnMessageRecieved: TMessageReceivedEvent read FOnMessageReceived write FOnMessageReceived; deprecated 'use OnMessageReceived'; {$endif}
  end;

implementation

uses
  CastleApplicationProperties;

//
//TThreadSynchronisationHandler
//

{$ifndef ANDROID}
  class procedure TThreadSynchronisationHandler.Update(Sender: TObject);
  begin
    CheckSynchronize; //This has to be done in the main thread regularly to enable Synchronisation between threads.
  end;

  class procedure TThreadSynchronisationHandler.Initialise;
  begin
    if not IsInitialised then
    begin
      ApplicationProperties.OnUpdate.Add({$ifdef FPC}@{$endif}Update);
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

  function TAndroidTCPConnectionService.MessageFilter(const Received: TCastleStringList;
    const ReceivedStream: TMemoryStream): Boolean;
  var
    LConnectionStatus: Boolean;
  begin
    Result := false;
    if (Received.Count = 4) and (Received[0] = AndroidServiceName) and (Received[1] = 'message') then //"client-server" "message" message clientid
    begin
      if Assigned(FMessageReceived) then
        FMessageReceived(Received[2], TClientConnection.Create(Received[3]));
      Result := true;
    end
    else if (Received.Count = 3) and (Received[0] = AndroidServiceName) and (Received[1] = 'connected') then //"client-server" "connected" clientid
    begin
      FConnectedDictionary.AddOrSetValue(Received[2], true);
      if Assigned(FOnConnected) then
        FOnConnected(TClientConnection.Create(Received[2]));
      Result := true;
    end
    else if (Received.Count = 3) and (Received[0] = AndroidServiceName) and (Received[1] = 'disconnected') then //client-server "disconnected" clientid
    begin
      LConnectionStatus := false;
      if Assigned(FOnDisconnected) and FConnectedDictionary.TryGetValue(Received[2], LConnectionStatus) and LConnectionStatus then
        FOnDisconnected(TClientConnection.Create(Received[2])); //Only fire the event if the connection wasn't closed by us.

      FConnectedDictionary.Remove(Received[2]);

      if FConnectedDictionary.Count = 0 then
        FIsActive := false;

      Result := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateServer(const APort: Word);
  begin
    if not FIsActive then
    begin
      Messaging.Send([AndroidServiceName, FKey, 'server', 'tcp', IntToStr(APort)]);
      FIsActive := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateClient(const AHost: String; const APort: Word);
  begin
    if not FIsActive then
    begin
      Messaging.Send([AndroidServiceName, FKey, 'client', 'tcp', AHost, IntToStr(APort)]);
      FIsActive := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.SendMessage(const AMessage: String);
  begin
    SendMessage(AMessage, TClientConnection.Create('all'));
  end;

  procedure TAndroidTCPConnectionService.SendMessage(const AMessage: String; AClient: TClientConnection);
  begin
    if FIsActive then
      Messaging.Send([AndroidServiceName, FKey, 'send', AMessage, AClient.ID]);
  end;

  procedure TAndroidTCPConnectionService.Close;
  begin
    Close(TClientConnection.Create('all'));
  end;

  procedure TAndroidTCPConnectionService.Close(AClient: TClientConnection);
  begin
    if FIsActive then
    begin
      FConnectedDictionary.AddOrSetValue(AClient.ID, false);
      Messaging.Send([AndroidServiceName, FKey, 'close', AClient.ID]);
    end;
  end;

  function TAndroidTCPConnectionService.IsConnected: Boolean;
  begin
    Result := IsConnected(TClientConnection.Create('client'));
  end;

  function TAndroidTCPConnectionService.IsConnected(AClient: TClientConnection): Boolean;
  begin
    Result := false;
    if FIsActive then
      FConnectedDictionary.TryGetValue(AClient.ID, Result);
  end;
{$endif}

//
//TCastleTCPClientThread
//

{$ifndef ANDROID}
  constructor TCastleTCPClientThread.Create(const AClient: TIdTCPClient; const AOnMessageReceived, AOnConnected, AOnDisconnected: TProcedureObject);
  begin
    FClient := AClient;
    FOnMessageReceived := AOnMessageReceived;
    FOnConnected := AOnConnected;
    FOnDisconnected := AOnDisconnected;

    FMessageList := TSynchronisedStringList.Create;

    FreeOnTerminate := true;
    inherited Create(false);
  end;

  destructor TCastleTCPClientThread.Destroy;
  begin
    FMessageList.Free;

    inherited;
  end;

  procedure TCastleTCPClientThread.Execute;
  var
    LMessage: String;
  begin
    FClient.Connect;
    FClient.IOHandler.ReadTimeout := 100;

    if Assigned(FOnConnected) then
      Queue(FOnConnected);

    while not Terminated do
    begin
      try
        LMessage := FClient.IOHandler.ReadLn; //Result is empty when timeout reached.

        if LMessage <> '' then
        begin
          FMessageList.Add(LMessage);
          Queue(FOnMessageReceived);
        end;
      except
        //Exception occurs when connection was disconnected (gracefully).
        Terminate;
        if Assigned(FOnDisconnected) then
          Synchronize(FOnDisconnected); //Must be synchronize, not queue, because queued messages get lost.
      end;
    end;
    FClient.Disconnect;
    if FreeClientOnTerminate then
      FClient.Free;
  end;
{$endif}

//
//TClientConnection
//

constructor TClientConnection.Create({$ifdef ANDROID}AID: String{$else}AContext: TIdContext{$endif});
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
  {$else}
    TThreadSynchronisationHandler.Initialise;
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
//TCastleTCPServer
//

constructor TCastleTCPServer.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FClientConnectedList := TClientContextList.Create;
    FClientDisconnectedList := TClientContextList.Create;
    FClientConnectionsList := TClientContextList.Create;
    FMessageList := TMessageClientList.Create;

    FServer := TIdTCPServer.Create;
    FServer.OnConnect := {$ifdef FPC}@{$endif} ServerOnConnect;
    FServer.OnDisconnect := {$ifdef FPC}@{$endif} ServerOnDisconnect;
    FServer.OnExecute := {$ifdef FPC}@{$endif} ServerOnExecute;
  {$endif}
end;

destructor TCastleTCPServer.Destroy;
begin
  {$ifndef ANDROID}
    FServer.Free;
    FClientConnectedList.Free;
    FClientDisconnectedList.Free;
    FClientConnectionsList.Free;
    FMessageList.Free;
  {$endif}

  inherited;
end;

procedure TCastleTCPServer.Start;
{$ifdef UNIX}{$ifndef ANDROID}
  var
    Binding: TIdSocketHandle;
{$endif}{$endif}
begin
  {$ifdef ANDROID}
    FTCPConnectionService.OnMessageReceived := FOnMessageReceived;
    FTCPConnectionService.OnConnected := FOnConnected;
    FTCPConnectionService.OnDisconnected := FOnDisconnected;
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

procedure TCastleTCPServer.Stop;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close;
  {$else}
    FServer.Active := false;
  {$endif}
end;

procedure TCastleTCPServer.Disconnect(AClient: TClientConnection);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close(AClient);
  {$else}
    AClient.Context.Data := AClient.Context; //Prevents the OnDisconnect event to fire after disconnect on our site.
    AClient.Context.Connection.Disconnect;
  {$endif}
end;

procedure TCastleTCPServer.SendToClient(const AMessage: String; AClient: TClientConnection);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage, AClient);
  {$else}
    AClient.Context.Connection.IOHandler.WriteLn(AMessage);
  {$endif}
end;

procedure TCastleTCPServer.SendToAll(const AMessage: String);
{$ifndef ANDROID}
  var
    LContext: TIdContext;
{$endif}
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage);
  {$else}
    for LContext in FClientConnectionsList.LockList do
      LContext.Connection.IOHandler.WriteLn(AMessage);
    FClientConnectionsList.UnlockList;
  {$endif}
end;

function TCastleTCPServer.IsConnected: Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected;
  {$else}
    Result := FClientConnectionsList.LockList.Count > 0;
    FClientConnectionsList.UnlockList;
  {$endif}
end;

function TCastleTCPServer.IsConnected(AClient: TClientConnection): Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected(AClient);
  {$else}
    Result := FClientConnectionsList.LockList.IndexOf(AClient.Context) > -1;
    FClientConnectionsList.UnlockList;
  {$endif}
end;

{$ifndef ANDROID}
  procedure TCastleTCPServer.ServerOnConnect(AContext: TIdContext);
  begin
    FClientConnectedList.Add(AContext);
    FClientConnectionsList.Add(AContext);

    AContext.Connection.IOHandler.ReadTimeout := 100;

    if Assigned(FOnConnected) then
      TThread.Queue(nil, {$ifdef FPC}@{$endif} ServerOnClientConnected);
  end;

  procedure TCastleTCPServer.ServerOnDisconnect(AContext: TIdContext);
  begin
    FClientConnectionsList.Remove(AContext);
    FClientDisconnectedList.Add(AContext);

    if Assigned(FOnDisconnected) and (AContext.Data = nil) and FServer.Active then
      TThread.Synchronize(nil, {$ifdef FPC}@{$endif} ServerOnClientDisconnected); //Disconnect must be synchronized, not queued, because else the queued message gets losed.
  end;

  procedure TCastleTCPServer.ServerOnExecute(AContext: TIdContext);
  var
    LMessageClientRecord: TMessageClientRecord;
  begin
    LMessageClientRecord.Message := AContext.Connection.IOHandler.ReadLn;

    if LMessageClientRecord.Message <> '' then
    begin
      LMessageClientRecord.Client := TClientConnection.Create(AContext);

      FMessageList.Add(LMessageClientRecord);

      TThread.Queue(nil, {$ifdef FPC}@{$endif} ServerOnMessageReceived);
    end;
  end;

  procedure TCastleTCPServer.ServerOnClientConnected;
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

  procedure TCastleTCPServer.ServerOnClientDisconnected;
  var
    LContext: TIdContext;
  begin
    if Assigned(FOnDisconnected) then
    begin
      for LContext in FClientDisconnectedList.LockList do
        FOnDisconnected(TClientConnection.Create(LContext));
      FClientDisconnectedList.Clear;
      FClientDisconnectedList.UnlockList;
    end;
  end;

  procedure TCastleTCPServer.ServerOnMessageReceived;
  var
    LMessageClientRecord: TMessageClientRecord;
  begin
    if Assigned(FOnMessageReceived) then
    begin
      for LMessageClientRecord in FMessageList.LockList do
        FOnMessageReceived(LMessageClientRecord.Message, LMessageClientRecord.Client);
      FMessageList.Clear;
      FMessageList.UnlockList;
    end;
  end;
{$endif}

//
//TCastleTCPClient
//

constructor TCastleTCPClient.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FClient := TIdTCPClient.Create;
    FClient.ConnectTimeout := 5000;
  {$endif}
end;

destructor TCastleTCPClient.Destroy;
begin
  {$ifndef ANDROID}
    FClientThread.FreeClientOnTerminate := true;
    FClientThread.Terminate;
  {$endif}

  inherited;
end;

procedure TCastleTCPClient.Connect;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.OnConnected := @ClientOnClientConnected;
    FTCPConnectionService.OnDisconnected := @ClientOnClientDisconnected;
    FTCPConnectionService.OnMessageReceived := @ClientOnMessageReceived;
    FTCPConnectionService.CreateClient(FHostname, FPort);
  {$else}
    FClient.Port := FPort;
    FClient.Host := FHostname;

    FClientThread := TCastleTCPClientThread.Create(FClient,
      {$ifdef FPC}@{$endif} ClientOnMessageReceived, FOnConnected, FOnDisconnected);
  {$endif}
end;

procedure TCastleTCPClient.Disconnect;
begin
  {$ifdef ANDROID}
    FTCPConnectionService.Close;
  {$else}
    FClientThread.Terminate;
  {$endif}
end;

procedure TCastleTCPClient.Send(const AMessage: String);
begin
  if not IsConnected then
    raise Exception.Create('Cannot send message because not connected');

  {$ifdef ANDROID}
  FTCPConnectionService.SendMessage(AMessage);
  {$else}
  Assert(FClient.IOHandler <> nil); // checking IsConnected should guarantee this
  FClient.IOHandler.WriteLn(AMessage);
  {$endif}
end;

function TCastleTCPClient.IsConnected: Boolean;
begin
  {$ifdef ANDROID}
  Result := FTCPConnectionService.IsConnected;
  {$else}
  Result := FClient.Connected;
  if Result then
    Assert(FClient.IOHandler <> nil);
  {$endif}
end;

{$ifdef ANDROID}
  procedure TCastleTCPClient.ClientOnClientConnected(AClientConnection: TClientConnection);
  begin
    if Assigned(FOnConnected) then
      FOnConnected();
  end;

  procedure TCastleTCPClient.ClientOnClientDisconnected(AClientConnection: TClientConnection);
  begin
    if Assigned(FOnDisconnected) then
      FOnDisconnected();
  end;

  procedure TCastleTCPClient.ClientOnMessageReceived(const AMessage: String; AClientConnection: TClientConnection);
  begin
    if Assigned(FOnMessageReceived) then
      FOnMessageReceived(AMessage);
  end;
{$else}
  procedure TCastleTCPClient.ClientOnMessageReceived;
  var
    LMessage: String;
  begin
    if Assigned(FOnMessageReceived) then
    begin
      for LMessage in FClientThread.MessageList.LockList do
        FOnMessageReceived(LMessage);
      FClientThread.MessageList.Clear;
      FClientThread.MessageList.UnlockList;
    end;
  end;
{$endif}

end.
