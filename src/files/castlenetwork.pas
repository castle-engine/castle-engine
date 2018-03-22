unit castlenetwork;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, Generics.Collections,
  //CastleStringUtils,
  {$ifdef ANDROID}
    CastleMessaging
  {$else}
    IdTCPServer, IdTCPClient, IdContext, IdSocketHandle
  {$endif};

type
  TMessageRecieved = procedure(const AMessage: String) of object;
  TProcedureObject = procedure of object;

type
  TSynchronisedObjectList<T: class> = class(TObjectList<T>)
  protected
    FCriticalSection: TCriticalSection;
  public
    constructor Create(AOwnsObjects: Boolean = true); overload;
    destructor Destroy; override;
    function Add(constref AValue: T): SizeInt; override;
    function Remove(constref AValue: T): SizeInt; override;
    function IndexOf(constref AValue: T): SizeInt; override;
    procedure Clear; reintroduce;
    procedure Enter; virtual;
    procedure Leave; virtual;
  end;

  TSynchronisedStringList = specialize TSynchronisedObjectList<String>;

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
    TAndroidTCPConnectionService = class
      strict protected
        FMessageRecieved: TMessageRecieved;
        FOnConnected: TProcedureObject;
        FIsConnected: Boolean;
        FKey: String;
        function MessageFilter (const Received: TCastleStringList): Boolean;
      public
        constructor Create;
        destructor Destroy; override;
      public
        procedure CreateServer (const APort: Word);
        procedure CreateClient (const AHost: String; const APort: Word);
        procedure SendMessage (const AMessage: String);
        procedure Close;
      public
        property OnMessageRecieved: TMessageRecieved write FMessageRecieved;
        property OnConnected: TProcedureObject write FOnConnected;
        property IsConnected: Boolean read FIsConnected;
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
    FOnConnected: TProcedureObject;
    FOnMessageRecieved: TMessageRecieved;
    procedure SetOnConnected (const AOnConnected: TProcedureObject); virtual;
    procedure SetOnMessageRecieved (const AOnMessageRecieved: TMessageRecieved); virtual;
  public
    property Port: Word read FPort write FPort;
    property OnConnected: TProcedureObject read FOnConnected write SetOnConnected;
    property OnMessageRecieved: TMessageRecieved read FOnMessageRecieved write SetOnMessageRecieved;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Send (const AMessage: String); virtual; abstract;
  public
    function IsConnected: Boolean; virtual; abstract;
  end;

  TCastleServer = class (TCastleSocket)
  strict protected
    {$ifndef ANDROID}
      FServer: TIdTCPServer;
      FContext: TIdContext;
      FMessageList: TSynchronisedStringList;
      procedure ServerOnConnect (AContext: TIdContext);
      procedure ServerOnDisconnect (AContext: TIdContext);
      procedure ServerOnExecute (AContext: TIdContext);
      procedure ServerOnMessageRecieved;
    {$endif}
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Send (const AMessage: String); override;
  public
    function IsConnected: Boolean; override;
  end;

  TCastleClient = class (TCastleSocket)
  strict protected
    FHostname: String;
    {$ifndef ANDROID}
      FClient: TIdTCPClient;
      FClientThread: TCastleClientThread;
      procedure ClientOnMessageRecieved;
    {$endif}
  public
    property Hostname: String read FHostname write FHostname;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Connect; virtual;
    procedure Disconnect; virtual;
    procedure Send (const AMessage: String); override;
  public
    function IsConnected: Boolean; override;
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
    FKey := IntToHex(PtrUInt(Self), 1); //Key used for the identification is the unique pointer value of the class instance as hex string.
    Messaging.OnReceive.Add(@MessageFilter);
  end;

  destructor TAndroidTCPConnectionService.Destroy;
  begin
    Close;

    Messaging.OnReceive.Remove(@MessageFilter);

    inherited;
  end;

  function TAndroidTCPConnectionService.MessageFilter (const Received: TCastleStringList): Boolean;
  begin
    Result := false;
    if (Received.Count = 2) and (Received[0] = 'tcp_message') then
    begin
      if Assigned(FMessageRecieved) then
        FMessageRecieved(Received[1]);
      Result := true;
    end
    else if (Received.Count = 1) and (Received[0] = 'tcp_connected') then
    begin
      FIsConnected := true;
      if Assigned(FOnConnected) then
        FOnConnected;
      Result := true;
    end;
  end;

  procedure TAndroidTCPConnectionService.CreateServer (const APort: Word);
  begin
    Messaging.Send(['tcp_connection', FKey, 'server', IntToStr(APort)]);
  end;

  procedure TAndroidTCPConnectionService.CreateClient (const AHost: String; const APort: Word);
  begin
    Messaging.Send(['tcp_connection', FKey, 'client', AHost, IntToStr(APort)]);
  end;

  procedure TAndroidTCPConnectionService.SendMessage (const AMessage: String);
  begin
    Messaging.Send(['tcp_connection', FKey, 'send', AMessage]);
  end;

  procedure TAndroidTCPConnectionService.Close;
  begin
    Messaging.Send(['tcp_connection', FKey, 'close']);
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
//TSynchronisedObjectList
//

constructor TSynchronisedObjectList<T>.Create (AOwnsObjects: Boolean);
begin
  inherited;

  FCriticalSection := TCriticalSection.Create;
end;

destructor TSynchronisedObjectList<T>.Destroy;
begin
  FCriticalSection.Enter;
  inherited;
  FCriticalSection.Leave;

  FCriticalSection.Free;
end;

function TSynchronisedObjectList<T>.Add (constref AValue: T): SizeInt;
begin
  FCriticalSection.Enter;
  Result := inherited;
  FCriticalSection.Leave;
end;

function TSynchronisedObjectList<T>.Remove (constref AValue: T): SizeInt;
begin
  FCriticalSection.Enter;
  Result := inherited;
  FCriticalSection.Leave;
end;

function TSynchronisedObjectList<T>.IndexOf(constref AValue: T): SizeInt; override;
begin
  FCriticalSection.Enter;
  Result := inherited;
  FCriticalSection.Leave;
end;

procedure TSynchronisedObjectList<T>.Clear; virtual;
begin
  FCriticalSection.Enter;
  inherited;
  FCriticalSection.Leave;
end;

procedure TSynchronisedObjectList<T>.Enter; virtual;
begin
  FCriticalSection.Enter;
end;

procedure TSynchronisedObjectList<T>.Leave; virtual;
begin
  FCriticalSection.Leave;
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

procedure TCastleSocket.SetOnConnected (const AOnConnected: TProcedureObject);
begin
  FOnConnected := AOnConnected;
end;

procedure TCastleSocket.SetOnMessageRecieved (const AOnMessageRecieved: TMessageRecieved);
begin
  FOnMessageRecieved := AOnMessageRecieved;
end;

//
//TCastleServer
//

constructor TCastleServer.Create;
begin
  inherited;

  {$ifndef ANDROID}
    FMessageList := TSynchronisedStringList.Create;
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

procedure TCastleServer.Send(const AMessage: String);
begin
  {$ifdef ANDROID}
    FTCPConnectionService.SendMessage(AMessage);
  {$else}
    FContext.Connection.IOHandler.WriteLn(AMessage);
  {$endif}
end;

function TCastleServer.IsConnected: Boolean;
begin
  {$ifdef ANDROID}
    Result := FTCPConnectionService.IsConnected;
  {$else}
    Result := Assigned(FContext);
  {$endif}
end;

{$ifndef ANDROID}
  procedure TCastleServer.ServerOnConnect(AContext: TIdContext);
  begin
    if Assigned(FContext) then
      AContext.Connection.Disconnect
    else
    begin
      FContext := AContext;
      if Assigned(FOnConnected) then
        TThread.Queue(nil, FOnConnected);
    end;
  end;

  procedure TCastleServer.ServerOnDisconnect(AContext: TIdContext);
  begin
    if AContext = FContext then
      FContext := nil;
  end;

  procedure TCastleServer.ServerOnExecute(AContext: TIdContext);
  var
    LMessage: String;
  begin
    LMessage := AContext.Connection.IOHandler.ReadLn;

    FMessageList.Add(LMessage);

    TThread.Queue(nil, @ServerOnMessageRecieved);
  end;

  procedure TCastleServer.ServerOnMessageRecieved;
  var
    LMessage: String;
  begin
    if Assigned(FOnMessageRecieved) then
    begin
      FMessageList.Enter;

      for LMessage in FMessageList do
        FOnMessageRecieved(LMessage);

      FMessageList.Clear;

      FMessageList.Leave;
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
    FTCPConnectionService.OnMessageRecieved := FOnMessageRecieved;
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

{$ifndef ANDROID}
  procedure TCastleClient.ClientOnMessageRecieved;
  var
    LMessage: String;
  begin
    if Assigned(FOnMessageRecieved) then
    begin
      FClientThread.MessageList.Enter;

      for LMessage in FClientThread.MessageList do
        FOnMessageRecieved(LMessage);

      FClientThread.MessageList.Clear;

      FClientThread.MessageList.Leave;
    end;
  end;
{$endif}

end.
