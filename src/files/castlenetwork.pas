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

unit castlenetwork;

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

{$ifndef ANDROID}
type
  TMessageClientRecord = record
    Message: String;
    Client: TClientConnection;
  end;

  TMessageClientList = specialize TThreadList<TMessageClientRecord>;
  TClientContextList = specialize TThreadList<TIdContext>; //Because Indys version isn't generic in FPC.
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
        FOnDisconnected: TClientConnectionEvent;
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
        property OnConnected: TClientConnectionEvent write FOnConnected;
        property OnDisconnected: TClientConnectionEvent write FOnDisconnected;
        property OnMessageRecieved: TClientMessageRecievedEvent write FMessageRecieved;
      public
        function IsConnected (const AClient: TClientConnection = nil): Boolean;
    end;
{$else}
  type
    TCastleClientThread = class (TThread)
      strict protected
        FClient: TIdTCPClient;
        FOnConnected: TProcedureObject;
        FOnDisconnected: TProcedureObject;
        FOnMessageRecieved: TProcedureObject;
      strict protected
        FMessageList: TSynchronisedStringList;
      protected
        procedure Execute; override;
      public
        constructor Create (const AClient: TIdTCPClient; const AOnMessageRecieved, AOnConnected, AOnDisconnected: TProcedureObject); virtual;
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
      FClientDisconnectedList: TClientContextList; //Same for all disconnected clients.
      FClientConnectionsList: TClientContextList; //List for all CURRENTLY connected clients.
      FMessageList: TMessageClientList;
      procedure ServerOnConnect (AContext: TIdContext);
      procedure ServerOnDisconnect (AContext: TIdContext);
      procedure ServerOnExecute (AContext: TIdContext);
      procedure ServerOnClientConnected;
      procedure ServerOnClientDisconnected;
      procedure ServerOnMessageRecieved;
    {$endif}
  strict protected
     FOnConnected: TClientConnectionEvent;
     FOnDisconnected: TClientConnectionEvent;
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
    property OnDisconnected: TClientConnectionEvent read FOnDisconnected write FOnDisconnected;
    property OnMessageRecieved: TClientMessageRecievedEvent read FOnMessageRecieved write FOnMessageRecieved;
  end;

  TCastleClient = class (TCastleSocket)
  strict protected
    FHostname: String;
    {$ifdef ANDROID}
      procedure ClientOnClientConnected (const AClientConnection: TClientConnection);
      procedure ClientOnClientDisconnected (const AClientConnection: TClientConnection);
      procedure ClientOnMessageRecieved (const AMessage: String; const AClientConnection: TClientConnection);
    {$else}
      FClient: TIdTCPClient;
      FClientThread: TCastleClientThread;
      procedure ClientOnMessageRecieved;
    {$endif}
  strict protected
    FOnConnected: TConnectionEvent;
    FOnDisconnected: TConnectionEvent;
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
    property OnDisconnected: TConnectionEvent read FOnDisconnected write FOnDisconnected;
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
  var
    LConnectionStatus: Boolean;
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
    end
    else if (Received.Count = 2) and (Received[0] = 'tcp_disconnected') then //tcp_disconnected clientid
    begin
      LConnectionStatus := false;
      if Assigned(FOnDisconnected) and FConnectedDictionary.TryGetValue(Received[1], LConnectionStatus) and LConnectionStatus then
        FOnDisconnected(TClientConnection.Create(Received[1])); //Only fire the event if the connection wasn't closed by us.

      FConnectedDictionary.Remove(Received[1]);

      if FConnectedDictionary.Count = 0 then
        FIsActive := false;

      Result := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateServer (const APort: Word);
  begin
    if not FIsActive then
    begin
      Messaging.Send(['tcp_connection', FKey, 'server', IntToStr(APort)]);
      FIsActive := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateClient (const AHost: String; const APort: Word);
  begin
    if not FIsActive then
    begin
      Messaging.Send(['tcp_connection', FKey, 'client', AHost, IntToStr(APort)]);
      FIsActive := true;
    end;
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

      FConnectedDictionary.AddOrSetValue(AClientID, false);
      Messaging.Send(['tcp_connection', FKey, 'close', AClientID]);
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
  constructor TCastleClientThread.Create (const AClient: TIdTCPClient; const AOnMessageRecieved, AOnConnected, AOnDisconnected: TProcedureObject);
  begin
    FClient := AClient;
    FOnMessageRecieved := AOnMessageRecieved;
    FOnConnected := AOnConnected;
    FOnDisconnected := AOnDisconnected;

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
          Queue(FOnMessageRecieved);
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
//TCastleServer
//

constructor TCastleServer.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FClientConnectedList := TClientContextList.Create;
    FClientDisconnectedList := TClientContextList.Create;
    FClientConnectionsList := TClientContextList.Create;
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
    FClientDisconnectedList.Free;
    FClientConnectionsList.Free;
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
    FTCPConnectionService.Close(AClient);
  {$else}
    AClient.Context.Data := AClient.Context;
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
    for LContext in FClientConnectionsList.LockList do
      LContext.Connection.IOHandler.WriteLn(AMessage);
    FClientConnectionsList.UnlockList;
  {$endif}
end;

function TCastleServer.IsConnected (const AClient: TClientConnection = nil): Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected(AClient);
  {$else}
    if Assigned(AClient) then
    begin
      Result := FClientConnectionsList.LockList.IndexOf(AClient.Context) > -1;
      FClientConnectionsList.UnlockList;
    end
    else
    begin
      Result := FClientConnectionsList.LockList.Count > 0;
      FClientConnectionsList.UnlockList;
    end;
  {$endif}
end;

{$ifndef ANDROID}
  procedure TCastleServer.ServerOnConnect (AContext: TIdContext);
  begin
    FClientConnectedList.Add(AContext);
    FClientConnectionsList.Add(AContext);

    AContext.Connection.IOHandler.ReadTimeout := 100;

    if Assigned(FOnConnected) then
      TThread.Queue(nil, @ServerOnClientConnected);
  end;

  procedure TCastleServer.ServerOnDisconnect (AContext: TIdContext);
  begin
    FClientConnectionsList.Remove(AContext);
    FClientDisconnectedList.Add(AContext);

    if Assigned(FOnDisconnected) and (AContext.Data = nil) and FServer.Active then
      TThread.Synchronize(nil, @ServerOnClientDisconnected); //Disconnect must be synchronized, not queued, because else the queued message gets losed.
  end;

  procedure TCastleServer.ServerOnExecute (AContext: TIdContext);
  var
    LMessageClientRecord: TMessageClientRecord;
  begin
    LMessageClientRecord.Message := AContext.Connection.IOHandler.ReadLn;

    if LMessageClientRecord.Message <> '' then
    begin
      LMessageClientRecord.Client := TClientConnection.Create(AContext);

      FMessageList.Add(LMessageClientRecord);

      TThread.Queue(nil, @ServerOnMessageRecieved);
    end;
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

  procedure TCastleServer.ServerOnClientDisconnected;
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
    FClient.ConnectTimeout := 5000;
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
    FTCPConnectionService.OnDisconnected := @ClientOnClientDisconnected;
    FTCPConnectionService.OnMessageRecieved := @ClientOnMessageRecieved;
    FTCPConnectionService.CreateClient(FHostname, FPort);
  {$else}
    FClient.Port := FPort;
    FClient.Host := FHostname;

    FClientThread := TCastleClientThread.Create(FClient, @ClientOnMessageRecieved, FOnConnected, FOnDisconnected);
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
    if Assigned(FOnConnected) then
      FOnConnected();
  end;

  procedure TCastleClient.ClientOnClientDisconnected (const AClientConnection: TClientConnection);
  begin
    if Assigned(FOnDisconnected) then
      FOnDisconnected();
  end;

  procedure TCastleClient.ClientOnMessageRecieved (const AMessage: String; const AClientConnection: TClientConnection);
  begin
    if Assigned(FOnMessageRecieved) then
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
