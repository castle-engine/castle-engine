unit SteamSubsystem;

interface
  uses Classes, SysUtils, CastleLog, SteamTypes;

type
  TSteamCallbackHook = function(const C: TCallbackMsg): Boolean of Object;

  TSteamSubsystem = class
  protected
    FAPIHandle: Pointer;
    FOnCallback: TSteamCallbackHook;
    function VerifyLoad(P: Pointer; const S: String): Boolean;
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); virtual;
    destructor Destroy; override;
    function RunCallback(const C: TCallbackMsg): Boolean;
    property OnCallback: TSteamCallbackHook read FOnCallback write FOnCallback;
  end;


implementation

{ TSteamSubsystem }

constructor TSteamSubsystem.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
end;

destructor TSteamSubsystem.Destroy;
begin
  FOnCallback := Nil;
  inherited;
end;

function TSteamSubsystem.RunCallback(const C: TCallbackMsg): Boolean;
begin
  Result := False;
  if Assigned(FOnCallback) then
    Result := FOnCallback(C);
end;

function TSteamSubsystem.VerifyLoad(P: Pointer; const S: String): Boolean;
begin
  Result := True;
  if P = Nil then
    begin
      WriteLnLog('Failed to load the %s subsystem', [S]);
      Result := False;
    end
  else
    WriteLnLog('Loaded the %s subsystem', [S]);
end;


end.
