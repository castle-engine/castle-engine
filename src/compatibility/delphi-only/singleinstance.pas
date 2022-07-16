unit singleinstance;

{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Ondrej Pokorny

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$H+}

interface

uses
  SysUtils, Classes;

type

  TBaseSingleInstance = class;

  //siServer: No other instance is running. The server is started.
  //siClient: There is another instance running. This instance is used as client.
  //siNotResponding: There is another instance running but it doesn't respond.
  TSingleInstanceStart = (siServer, siClient, siNotResponding);
  TSingleInstanceParamsEvent = procedure(Sender: TBaseSingleInstance; Params: TStringList) of object;
  TBaseSingleInstance = class(TComponent)
  private
    FStartResult: TSingleInstanceStart;
    FTimeOutMessages: Integer;
    FTimeOutWaitForInstances: Integer;
    FOnServerReceivedParams: TSingleInstanceParamsEvent;
  Protected  
    function GetIsClient: Boolean; virtual; abstract;
    function GetIsServer: Boolean; virtual; abstract;
    function GetStartResult: TSingleInstanceStart; virtual;
    procedure DoServerReceivedParams(const aParamsDelimitedText: string);
    Procedure SetStartResult(AValue : TSingleInstanceStart); 
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    //call Start when you want to start single instance checking
    function Start: TSingleInstanceStart; virtual; abstract;
    //stop single instance server or client
    procedure Stop; virtual; abstract;

    //check and handle pending messages on server
    procedure ServerCheckMessages; virtual; abstract;
    //post cmd parameters from client to server
    procedure ClientPostParams; virtual; abstract;
  public
    property TimeOutMessages: Integer read FTimeOutMessages write FTimeOutMessages;
    property TimeOutWaitForInstances: Integer read FTimeOutWaitForInstances write FTimeOutWaitForInstances;
    property OnServerReceivedParams: TSingleInstanceParamsEvent read FOnServerReceivedParams write FOnServerReceivedParams;
  public
    property StartResult: TSingleInstanceStart read GetStartResult;
    property IsServer: Boolean read GetIsServer;
    property IsClient: Boolean read GetIsClient;
  end;
  TBaseSingleInstanceClass = class of TBaseSingleInstance;

  ESingleInstance = class(Exception);

Var
  DefaultSingleInstanceClass : TBaseSingleInstanceClass = Nil;

implementation

{ TBaseSingleInstance }

constructor TBaseSingleInstance.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FTimeOutMessages := 1000;
  FTimeOutWaitForInstances := 100;
end;

destructor TBaseSingleInstance.Destroy;
begin
  Stop;

  inherited Destroy;
end;

procedure TBaseSingleInstance.DoServerReceivedParams(
  const aParamsDelimitedText: string);
var
  xSL: TStringList;
begin
  if not Assigned(FOnServerReceivedParams) then
    Exit;

  xSL := TStringList.Create;
  try
    xSL.DelimitedText := aParamsDelimitedText;
    FOnServerReceivedParams(Self, xSL);
  finally
    xSL.Free;
  end;
end;

function TBaseSingleInstance.GetStartResult: TSingleInstanceStart;
begin
  Result := FStartResult;
end;

Procedure TBaseSingleInstance.SetStartResult(AValue : TSingleInstanceStart);

begin
  FStartResult:=AValue;
end;   

end.

