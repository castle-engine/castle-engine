{
  Copyright 2023-2024 Michalis Kamburelis, Yevhen Loza.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration with Steam.
  See @url(https://castle-engine.io/steam Steam and Castle Game Engine documentation)
  for usage. }
unit steaminput;

interface

uses Classes, CTypes, SysUtils, SteamTypes, SteamSubsystem;

type

  TSteamInput = class(TSteamSubsystem)
  strict private
    FEnabled: Boolean;
    FDeviceCallbacks: Boolean;
    FEventCallbacks: Boolean;
    FCtrl: Array[0..STEAM_INPUT_MAX_COUNT-1] of TInputHandle;
    procedure SetEnabled(const AValue: Boolean);
  private
    function CallbackHandler(const C: TCallbackMsg): Boolean;
    procedure RunFrame;
    function GetSteamInputHandle(AValue: Integer): TInputHandle;
    procedure SetDeviceCallbacks(AValue: Boolean);
    procedure SetEventCallbacks(AValue: Boolean);
    procedure CallbackSteamInputConfigurationLoaded(P: PSteamInputConfigurationLoaded);
    procedure CallbackSteamInputDeviceConnected(P: PSteamInputDeviceConnected);
    procedure CallbackSteamInputGamepadSlotChange(
      P: PSteamInputGamepadSlotChange);
    procedure CallbackSteamInputDeviceDisconnected(
      P: PSteamInputDeviceDisconnected);
    procedure ActionCallback(P: PSteamInputActionEvent);
  public
    constructor Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32); override;
    destructor Destroy; override;
    function GetControllerForGamepadIndex(const AValue: Integer): TInputHandle;
    function GetConnectedControllers: Integer;
    procedure ShowBindingPanel(const AControl: TInputHandle);
    property Enabled: Boolean read FEnabled write SetEnabled;
    property InputHandle[Index: Integer]: TInputHandle read GetSteamInputHandle;
    property DeviceCallbacks: Boolean read FDeviceCallbacks write SetDeviceCallbacks;
    property EventCallbacks: Boolean read FEventCallbacks write SetEventCallbacks;
  end;

implementation

uses CastleInternalSteamApi,
  CastleLog, CastleUtils, CastleApplicationProperties;

{ TSteamInput }

constructor TSteamInput.Create(SteamClient: Pointer; SteamUserHandle: Int32; SteamPipeHandle: Int32);
begin
  inherited;
  // Init SteamInput
  FAPIHandle := SteamAPI_ISteamClient_GetISteamInput(
       SteamClient, SteamUserHandle, SteamPipeHandle, STEAMINPUT_INTERFACE_VERSION);
  VerifyLoad(FAPIHandle, Self.ClassName);
end;

destructor TSteamInput.Destroy;
begin
  if FEnabled then
    begin
      SteamAPI_ISteamInput_Shutdown(FAPIHandle);
    end;
  inherited;
end;

procedure TSteamInput.SetEnabled(const AValue: Boolean);
var
  BRes: Boolean;
begin
  if (FEnabled <> AValue) then
    begin
      if AValue then
        begin
          BRes := SteamAPI_ISteamInput_Init(FAPIHandle, True);
          if BRes then
            begin
              FOnCallback := {$ifdef fpc}@{$endif}CallbackHandler;
              WriteLnLog('=============> Input Enabled <=============');
            end
          else
            WriteLnLog('=============> Input Enable Failed <=============');
        end
      else
        begin
          BRes := SteamAPI_ISteamInput_Shutdown(FAPIHandle);
          WriteLnLog('=============> Input Disabled <=============')
        end;
      FEnabled := AValue;
      DeviceCallbacks := True;
      EventCallbacks := True;
    end;
end;

function TSteamInput.GetConnectedControllers: Integer;
begin
  if not FEnabled then
    Exit(0);

    Result := SteamAPI_ISteamInput_GetConnectedControllers(FAPIHandle, {$ifndef fpc}@{$endif}FCtrl);
end;

function TSteamInput.GetControllerForGamepadIndex(const AValue: Integer): TInputHandle;
begin
  if not FEnabled then
    Exit(0);

  Result := SteamAPI_ISteamInput_GetControllerForGamepadIndex(FAPIHandle, AValue);
end;

function TSteamInput.GetSteamInputHandle(AValue: Integer): TInputHandle;
begin
  if not FEnabled then
    Exit(0);

  if (AValue < 0) or (AValue >= STEAM_INPUT_MAX_COUNT) then
    Raise Exception.Create('Tried to read invalid steam input handle');

  Result := FCtrl[AValue];
end;

procedure TSteamInput.RunFrame;
var
  bReservedValue: LongBool;
begin
  bReservedValue := False;
  SteamAPI_ISteamInput_RunFrame(FAPIHandle, bReservedValue);
end;

procedure TSteamInput.ShowBindingPanel(const AControl: TInputHandle);
begin
  if FEnabled then
    SteamAPI_ISteamInput_ShowBindingPanel(FAPIHandle, AControl);
end;

procedure TSteamInput.SetDeviceCallbacks(AValue: Boolean);
begin
  if not FEnabled then
    Exit;
  if AValue then
    begin
      SteamAPI_ISteamInput_EnableDeviceCallbacks(FAPIHandle);
      FDeviceCallbacks := True;
    end;
end;

{$O-}
procedure TSteamInput.ActionCallback(P: PSteamInputActionEvent);
begin
  WritelnLog('Callbacks - PActionCallback : %d',[SizeOf(TSteamInputActionEvent)])
end;
{$O+}

procedure TSteamInput.SetEventCallbacks(AValue: Boolean);
begin
  if not FEnabled then
    Exit;
  if AValue then
    begin
      SteamAPI_ISteamInput_EnableActionEventCallbacks(FAPIHandle, {$ifdef fpc}@{$endif}ActionCallback);
      FEventCallbacks := True;
    end;
end;

function TSteamInput.CallbackHandler(const C: TCallbackMsg): Boolean;
begin
  Result := True;
  RunFrame;
  case C.m_iCallback of
    TSteamInputDeviceConnected.k_iCallback:
      begin
        if(SizeOf(TSteamInputDeviceConnected) <> C.m_cubParam) then
          WritelnLog('Callbacks', 'TSteamInputDeviceConnected Size = %d, should be %d)', [SizeOf(TSteamInputDeviceConnected), C.m_cubParam])
        else
          CallbackSteamInputDeviceConnected(PSteamInputDeviceConnected(C.m_pubParam));
      end;
    TSteamInputDeviceDisconnected.k_iCallback:
      begin
        if(SizeOf(TSteamInputDeviceDisconnected) <> C.m_cubParam) then
          WritelnLog('Callbacks', 'TSteamInputDeviceDisconnected Size = %d, should be %d)', [SizeOf(TSteamInputDeviceDisconnected), C.m_cubParam])
        else
          CallbackSteamInputDeviceDisconnected(PSteamInputDeviceDisconnected(C.m_pubParam));
      end;
    TSteamInputConfigurationLoaded.k_iCallback:
      begin
        if(SizeOf(TSteamInputConfigurationLoaded) <> C.m_cubParam) then
          WritelnLog('Callbacks', 'TSteamInputConfigurationLoaded Size = %d, should be %d)', [SizeOf(TSteamInputConfigurationLoaded), C.m_cubParam])
        else
          CallbackSteamInputConfigurationLoaded(PSteamInputConfigurationLoaded(C.m_pubParam));
      end;
    TSteamInputGamepadSlotChange.k_iCallback:
      begin
        if(SizeOf(TSteamInputGamepadSlotChange) <> C.m_cubParam) then
          WritelnLog('Callbacks', 'TSteamInputGamepadSlotChange Size = %d, should be %d)', [SizeOf(TSteamInputGamepadSlotChange), C.m_cubParam])
        else
          CallbackSteamInputGamepadSlotChange(PSteamInputGamepadSlotChange(C.m_pubParam));
      end
    else
      Result := False;
  end;
end;

{ Callback ID 2801 }
procedure TSteamInput.CallbackSteamInputDeviceConnected(
  P: PSteamInputDeviceConnected);
var
  InType: ESteamInputType;
begin
  InType := SteamAPI_ISteamInput_GetInputTypeForHandle(FAPIHandle, P{$ifdef fpc}^{$endif}.m_ulConnectedDeviceHandle);
  WritelnLog('>>> SteamInputDeviceConnected : InputHandle = %d, Type = %d', [P{$ifdef fpc}^{$endif}.m_ulConnectedDeviceHandle, Cardinal(InType)]);
end;

{ Callback ID 2802 }
procedure TSteamInput.CallbackSteamInputDeviceDisconnected(
  P: PSteamInputDeviceDisconnected);
begin
  writelnlog('>>> SteamInputDeviceDisconnected : InputHandle = %d',[P{$ifdef fpc}^{$endif}.m_ulDisconnectedDeviceHandle]);
end;

{ Callback ID 2803 }
procedure TSteamInput.CallbackSteamInputConfigurationLoaded(
  P: PSteamInputConfigurationLoaded);
begin
  writelnlog('>>> SteamInputConfigurationLoaded : InputHandle = %d',[P{$ifdef fpc}^{$endif}.m_ulDeviceHandle]);
  {
    m_unAppID: TAppId;
    m_ulDeviceHandle: TInputHandle;
    m_ulMappingCreator: CSteamID;
    m_unMajorRevision: UInt32;
    m_unMinorRevision: UInt32;
    m_bUsesSteamInputAPI: LongBool;
    m_bUsesGamepadAPI: LongBool;
    }
end;

{ Callback ID 2804 }
procedure TSteamInput.CallbackSteamInputGamepadSlotChange(
  P: PSteamInputGamepadSlotChange);
begin
  writelnlog('>>> SteamInputGamepadSlotChange : InputHandle = %d',[P{$ifdef fpc}^{$endif}.m_ulDeviceHandle]);

end;

end.

