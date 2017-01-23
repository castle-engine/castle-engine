{*------------------------------------------------------------------------------
  Receiving input from a 3DConnexion device, e.g. a SpacePilot.

  @Author Patrick M. Kolla
  @Version 0.2.0
-------------------------------------------------------------------------------}
// *****************************************************************************
// Copyright: © 2007 Patrick M. Kolla. All rights reserved.
// File:      pk3DConnexion.pas
// License:   LGPL 2.1
// Compiler:  Delphi 2006
// Purpose:   Receiving input from a 3DConnexion device, e.g. a SpacePilot.
// Authors:   Patrick M. Kolla (pk)
// *****************************************************************************
//  3DConnexion, SpacePilot, and probably other terms as well are trademarks
//  of 3DConnextion, A LogiTech company, all rights to these names are with
//  them, and the names are only used to identify the use of this unit, which
//  has been created without any commercial aim.
// *****************************************************************************
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation; either
//  version 2.1 of the License, or (at your option) any later version.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, write to the Free Software
//  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
// *****************************************************************************
// Dependencies:
// TDxInput_TLB - Type library unit for 3DConnexion COM driver
// *****************************************************************************
// Changelog (new entries first):
// ---------------------------------------
// 2007-06-02  pk  RLS  0.2.0
// 2007-06-02  pk   1h  Improved custom emulation settings
// 2007-05-31  pk  RLS  0.1.0
// 2007-05-31  pk  15m  Added header and some comments
// 2007-05-31  pk  30m  Added and tested mouse emulation
// 2007-05-30  pk   2h  Experimenting with COM objects
// *****************************************************************************

unit CastleInternalPk3DConnexion;

{ TODO -oPatrick : React to newly connected devices! }

{$I castleconf.inc}

interface

{$ifdef MSWINDOWS}
uses
   SysUtils,
   Windows,
   Classes,
   ActiveX,
   CastleInternalTDxInput_TLB;
{$endif}

type
   TEmulationAxis = (eaRotateX, eaRotateY, eaRotateZ, eaRotateAngle,
                      eaTranslateX, eaTranslateY, eaTranslateZ, eaTranslateLength);
   TEmulationDetail = (edNone, edMouseX, edMouseY, edMouseWheelV, edMouseWheelH,
                       edKeyboardArrowsLR, edKeyboardArrowsUD, edKeyboardPageUD,
                       edKeyboardCustom);
   TEmulationType = (etNone, etMouse, etKeyboard, etMouseKeyboard, etCustom);
   TEmulationAxisSetting = record
      Detail: TEmulationDetail;
      KeyLeft, KeyRight: byte;
      Threshold, Multiplier: integer;
   end;
   TEmulationSettings = array[TEmulationAxis] of TEmulationAxisSetting;

const
   SEmulationAxis : array[TEmulationAxis] of string =
      ('Rotate (X)', 'Rotate (Y)', 'Rotate (Z)', 'Rotate (Angle)',
       'Translate (X)', 'Translate (Y)', 'Translate (Z)', 'Translate (Length)');
   SEmulationAxisIDs : array[TEmulationAxis] of string =
      ('RotateX', 'RotateY', 'RotateZ', 'RotateAngle',
       'TranslateX', 'TranslateY', 'TranslateZ', 'TranslateLength');
   SEmulationType : array[TEmulationType] of string =
      ('None', 'Mouse', 'Keyboard', 'Mouse & Keyboard', 'Custom');
   SEmulationDetail : array[TEmulationDetail] of string =
      ('None', 'Mouse: X-axis', 'Mouse: Y-axis',
       'Mouse: Wheel (vertical)', 'Mouse: Wheel (horizontal)',
       'Keyboard: Arrows left/right', 'Keyboard: Arrows up/down',
       'Keyboard: Page up/down', 'Keyboard: Custom');
   SVirtualKeys : array[0..$AF] of string =
      ('No key assigned!',
       'Left mouse button (VK_LBUTTON)',
       'Right mouse button (VK_RIGHT_BUTTON)',
       'Cancel (VK_CANCEL)',
       'Middle mouse button (VK_MBUTTON)',
       'X1 mouse button (VK_XBUTTON1)',
       'X2 mouse button (VK_XBUTTON2)',
       ' --- Undefined',
       'Backspace (VK_BACK)',
       'Tabulator (VK_TAB)',
       ' --- Reserved', // 0A
       ' --- Reserved', // 0B
       'Clear (VK_CLEAR)',
       'Return/Enter (VK_RETURN)',
       ' --- Undefined', // 0E
       ' --- Undefined', // OF
       'Shift (VK_SHIFT)',
       'Control (VK_CONTROL)',
       'Alt/Menu (VK_ALT)',
       'Pause (VK_PAUSE)',
       'Caps Lock (VK_CAPITAL)',
       'IME: Kana/Hanguel/Hangul mode (VK_KANA/VK_HANGUEL/VK_HANGUL)',
       ' --- Undefined', // 16
       'IME: Junja mode (VK_JUNJA)',
       'IME: Final mode (VK_FINAL)',
       'IME: Hanja/Kanji mode (VK_HANJA/VK_KANJI)',
       ' --- Undefined', // 1A
       'Escape (VK_ESCAPE)',
       'IME: convert (VK_CONVERT)',
       'IME: nonconvert (VK_NONCONVERT)',
       'IME: accept (VK_ACCEPT)',
       'IME: mode change request (VK_MODECHANGE', // 1F
       'Spacebar (VK_SPACE)',
       'Page up (VK_PRIOR)',
       'Page down (VK_NEXT)',
       'End (VK_END)',
       'Home (VK_HOME)',
       'Arrow left (VK_LEFT)',
       'Arrow up (VK_UP)',
       'Arrow right (VK_RIGHT)',
       'Arrow down (VK_DOWN)',
       'Select (VK_SELECT)',
       'Print (VK_PRINT)',
       'Execute (VK_EXECUTE)',
       'Print Screen (VK_SNAPSHOT)',
       'Insert (VK_INSERT)',
       'Delete (VK_DELETE)',
       'Help (VK_HELP)',
       '0 (number key)',
       '1 (number key)',
       '2 (number key)',
       '3 (number key)',
       '4 (number key)',
       '5 (number key)',
       '6 (number key)',
       '7 (number key)',
       '8 (number key)',
       '9 (number key)',
       ' --- Undefined', // 3A
       ' --- Undefined', // 3B
       ' --- Undefined', // 3C
       ' --- Undefined', // 3D
       ' --- Undefined', // 3E
       ' --- Undefined', // 3F
       ' --- Undefined', // 40
       'A (letter key)',
       'B (letter key)',
       'C (letter key)',
       'D (letter key)',
       'E (letter key)',
       'F (letter key)',
       'G (letter key)',
       'H (letter key)',
       'I (letter key)',
       'J (letter key)',
       'K (letter key)',
       'L (letter key)',
       'M (letter key)',
       'N (letter key)',
       'O (letter key)',
       'P (letter key)',
       'Q (letter key)',
       'R (letter key)',
       'S (letter key)',
       'T (letter key)',
       'U (letter key)',
       'V (letter key)',
       'W (letter key)',
       'X (letter key)',
       'Y (letter key)',
       'Z (letter key)',
       'Left Windows key (VK_LWIN)',
       'Right Windows key (VK_RWIN)',
       'Applications key (VK_APPS)',
       ' --- Reserved', // 5E
       'Sleep (VK_SLEEP)',
       '0 (VK_NUMPAD0)',
       '1 (VK_NUMPAD1)',
       '2 (VK_NUMPAD2)',
       '3 (VK_NUMPAD3)',
       '4 (VK_NUMPAD4)',
       '5 (VK_NUMPAD5)',
       '6 (VK_NUMPAD6)',
       '7 (VK_NUMPAD7)',
       '8 (VK_NUMPAD8)',
       '9 (VK_NUMPAD9)',
       'Multiply (VK_MULTIPLY)',
       'Add (VK_ADD)',
       'Separator (VK_SEPARATOR)',
       'Subtract (VK_SUBTRACT)',
       'Decimal (VK_DECIMAL)',
       'Divide (VK_DIVIDE)',
       'F1 (VK_F1)',
       'F2 (VK_F2)',
       'F3 (VK_F3)',
       'F4 (VK_F4)',
       'F5 (VK_F5)',
       'F6 (VK_F6)',
       'F7 (VK_F7)',
       'F8 (VK_F8)',
       'F9 (VK_F9)',
       'F10 (VK_F10)',
       'F11 (VK_F11)',
       'F12 (VK_F12)',
       'F13 (VK_F13)',
       'F14 (VK_F14)',
       'F15 (VK_F15)',
       'F16 (VK_F16)',
       'F17 (VK_F17)',
       'F18 (VK_F18)',
       'F19 (VK_F19)',
       'F20 (VK_F20)',
       'F21 (VK_F21)',
       'F22 (VK_F22)',
       'F23 (VK_F23)',
       'F24 (VK_F24)',
       ' --- Undefined', // 88
       ' --- Undefined', // 89
       ' --- Undefined', // 8A
       ' --- Undefined', // 8B
       ' --- Undefined', // 8C
       ' --- Undefined', // 8D
       ' --- Undefined', // 8E
       ' --- Undefined', // 8F
       'Num lock (VK_NUMLOCK)',
       'Scroll lock (VK_SCROLL)',
       ' --- OEM specific', // 92
       ' --- OEM specific', // 93
       ' --- OEM specific', // 94
       ' --- OEM specific', // 95
       ' --- OEM specific', // 96
       ' --- Undefined', // 97
       ' --- Undefined', // 98
       ' --- Undefined', // 99
       ' --- Undefined', // 9A
       ' --- Undefined', // 9B
       ' --- Undefined', // 9C
       ' --- Undefined', // 9D
       ' --- Undefined', // 9E
       ' --- Undefined', // 9F
       'Left shift key (VK_LSHIFT)',
       'Right shift key (VK_RSHIFT)',
       'Left control key (VK_LCONTROL)',
       'Right control key (VK_RCONTROL)',
       'Left alt/menu key (VK_LMENU)',
       'Right alt/menu key (VK_RMENU)',
       'Browser back (VK_BROWSER_BACK)', // A6
       'Browser forward (VK_BROWSER_FORWARD)',
       'Browser refresh (VK_BROWSER_REFRESH)',
       'Browser stop (VK_BROWSER_STOP)',
       'Browser search (VK_BROWSER_SEARCH)',
       'Browser favorites (VK_BROWSER_FAVORITES)',
       'Browser home (VK_BROWSER_HOME)',
       'Volume mute (VK_VOLUME_MUTE)',
       'Volume down (VK_VOLUME_DOWN)',
       'Volume up (VK_VOLUME_UP)'
       );


type
   // Rotation on X,Y,Z is -1..1, Angle is 0..N
   //    N depends on speed setting, up to 1600
   TOnRotation = procedure(const X, Y, Z, Angle: Double) of object;
   // Translation on X,Y,Z is -N..N, Length is 0..N
   //    N depends on speed setting, up to 1600
   TOnTranslation = procedure(const X, Y, Z, Length: Double) of object;
   TOnKeyChange = procedure(const KeyIndex: integer; const Pressed: boolean) of object;
   TOnKeySomething = procedure(const KeyIndex: integer) of object;

{ Surround most of the content in Windows-only ifdef.
  Still, the above types, like TEmulationAxis, are defined on all OSes.
  This is useful e.g. to detect name clash errors on any OS. }
{$ifdef MSWINDOWS}

type
   T3DConnexionDevice = class(TObject)
   private
      FDevice: ISimpleDevice; /// Device interface
      FKeyboard: IKeyboard; /// Keyboard interface
      FLastResult: HResult; /// Error code for last COM action
      FLoaded: boolean; /// Whether connection to COM object was successful
      FRevisionNumber: widestring; /// COM driver revision? (not the same as driver version, e.g. 1.1.0 vs. 3.3.6)
      FSensor: ISensor; /// Sensor interface
      FKeyStates: array of boolean; /// Internal memory for last key stati
      FOnRotation: TOnRotation; /// Event for rotations
      FOnTranslation: TOnTranslation; /// Event for translations
      FOnKeyChange: TOnKeyChange; /// Event for key stati
      FOnKeyDown: TOnKeySomething; /// Event for key down
      FOnKeyUp: TOnKeySomething; /// Event for key up
      FKeyboardEmulationThreshold: integer; /// Only fire keys if amplitude larger than this
      FEmulationType: TEmulationType;
      FCustomEmulations: TEmulationSettings;
      FEmulatedKeyPressedStates: array[TEmulationAxis, false..true] of boolean;
      procedure HandleCustomDetail(const Axis: TEmulationAxis; const Value: Double);
      procedure FireKeyChange(const KeyIndex: integer; const Pressed: boolean);
      procedure FireRotation(const X, Y, Z, Angle: Double); overload;
      procedure FireRotation(const Rotation: IAngleAxis); overload;
      procedure FireTranslation(const X, Y, Z, Length: Double); overload;
      procedure FireTranslation(const Translation: IVector3D); overload;
      function GetSensorPeriod: Double;
      function GetCustomEmulations(AIndex: TEmulationAxis): TEmulationAxisSetting;
      procedure SetCustomEmulations(AIndex: TEmulationAxis; const Value: TEmulationAxisSetting);
   protected
   public
      constructor Create(const ApplicationName: widestring);
      destructor Destroy; override;
      procedure GetKeyboardKeyLabels(const List: TStrings);
      procedure GetKeyboardKeyNames(const List: TStrings);
      procedure GetKeyboardKeyInfos(const List: TStrings);
      procedure PollRotation;
      procedure PollTranslation;
      procedure PollKeys;
      procedure Poll;
      procedure GetSensorRotation(var X, Y, Z, Angle: Double);
      procedure GetSensorTranslation(var X, Y, Z, Length: Double);
      procedure SetExampleCustomEmulations;
      procedure SetCustomEmulationAxis(const AIndex: TEmulationAxis; const ADetail: TEmulationDetail; const AKeyLeft, AKeyRight: byte; const AThreshold, AMultiplier: integer);
      procedure CopyToSettings(var Settings: TEmulationSettings);
      procedure SetFromSettings(const Settings: TEmulationSettings);
      property CustomEmulations[AIndex: TEmulationAxis]: TEmulationAxisSetting read GetCustomEmulations write SetCustomEmulations;
   published
      property EmulationType: TEmulationType read FEmulationType write FEmulationType;
      property LastResult: HResult read FLastResult;
      property Loaded: boolean read FLoaded;
      property RevisionNumber: widestring read FRevisionNumber;
      property Sensor: ISensor read FSensor;
      property SensorPeriod: Double read GetSensorPeriod;
      property Keyboard: IKeyboard read FKeyboard;
      property KeyboardEmulationThreshold: integer read FKeyboardEmulationThreshold write FKeyboardEmulationThreshold;
      property OnKeyChange: TOnKeyChange read FOnKeyChange write FOnKeyChange;
      property OnKeyUp: TOnKeySomething read FOnKeyUp write FOnKeyUp;
      property OnKeyDown: TOnKeySomething read FOnKeyDown write FOnKeyDown;
      property OnRotation: TOnRotation read FOnRotation write FOnRotation;
      property OnTranslation: TOnTranslation read FOnTranslation write FOnTranslation;
   end;

procedure LoadSchemeFromFile(var Scheme: TEmulationSettings; const Filename: string);
procedure SaveSchemeToFile(const Scheme: TEmulationSettings; const Filename: string);

implementation

uses
   IniFiles;

const
  {$EXTERNALSYM MOUSEEVENTF_HWHEEL}
  MOUSEEVENTF_HWHEEL           = $1000; { wheel button rolled }
  MOUSEEVENTF_WHEEL            = 128;  // JA


{ T3DConnexionDevice }

{*------------------------------------------------------------------------------
  Constructor; connectes to COM object, loads preferences. Sets Loaded boolean.

  @param ApplicationName   Name of application; used for preferences.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.CopyToSettings(var Settings: TEmulationSettings);
var axis: TEmulationAxis;
begin
   for axis := Low(TEmulationAxis) to High(TEmulationAxis)
    do Settings[axis] := CustomEmulations[axis];
end;

constructor T3DConnexionDevice.Create(const ApplicationName: widestring);
var iHelper: IUnknown;
    info: ITDxInfo;
    iKey: integer;
    axis: TEmulationAxis;
begin
   inherited Create;
   FLoaded := false;
   CoInitialize(nil);
   FLastResult := CoCreateInstance(CLASS_TDxInfo, nil, CLSCTX_INPROC_SERVER, ITDxInfo, iHelper);
   if (FLastResult <> S_OK)
    then Exit;
   FLastResult := iHelper.QueryInterface(ITDxInfo, info);
   if FLastResult = S_OK
    then FRevisionNumber := info.RevisionNumber;
   FLastResult := CoCreateInstance(CLASS_Device, nil, CLSCTX_INPROC_SERVER, ISimpleDevice, iHelper);
   if (FLastResult <> S_OK)
    then Exit;
   FLastResult := iHelper.QueryInterface(ISimpleDevice, FDevice);
   if (FLastResult <> S_OK)
    then Exit;
   FDevice.Connect;
   FDevice.LoadPreferences(ApplicationName);
   FSensor := FDevice.Sensor;
   FKeyboard := FDevice.Keyboard;
   SetLength(FKeyStates, FKeyboard.Keys);
   for iKey := Low(FKeyStates) to High(FKeyStates)
    do FKeyStates[iKey] := false;
   for axis := Low(FEmulatedKeyPressedStates) to High(FEmulatedKeyPressedStates) do begin
      FEmulatedKeyPressedStates[axis, true] := false;
      FEmulatedKeyPressedStates[axis, false] := false;
   end;
   FLoaded := true;
   FEmulationType := etNone;
   FKeyboardEmulationThreshold := 60;
   SetExampleCustomEmulations;
end;

{*------------------------------------------------------------------------------
  Destructor
------------------------------------------------------------------------------*}
destructor T3DConnexionDevice.Destroy;
begin
   CoUninitialize;
   inherited;
end;

{*------------------------------------------------------------------------------
  Fires an event about a changed key status.

  @param KeyIndex   Index of key that was pressed, 0..n-1
  @param Pressed   Whether key is currently down
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.FireKeyChange(const KeyIndex: integer;
  const Pressed: boolean);
var keyFlag: cardinal;
begin
   if Assigned(FOnKeyChange)
    then FOnKeyChange(KeyIndex, Pressed);
   case Pressed of
      true: if Assigned(FOnKeyDown)
       then FOnKeyDown(KeyIndex);
      false: if Assigned(FOnKeyUp)
       then FOnKeyUp(KeyIndex);
   end;
   case FEmulationType of
      etNone: ;
      etMouse, etMouseKeyboard: begin
         case KeyIndex of
            1: if Pressed
             then KeyFlag := MOUSEEVENTF_RIGHTDOWN
              else KeyFlag := MOUSEEVENTF_RIGHTUP;
            else if Pressed
             then KeyFlag := MOUSEEVENTF_LEFTDOWN
              else KeyFlag := MOUSEEVENTF_LEFTUP;
         end;
         Mouse_Event(KeyFlag, 0, 0, 0, 0);
      end;
      etKeyboard: ;
   end;
end;

{*------------------------------------------------------------------------------
  Fires an event that notifies about rotations.

  @param X   X axis (tilt forward/backwards)
  @param Y   Y axis (rotate)
  @param Z   Z axis (tilt sidewards)
  @param Angle   Angle of rotation.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.FireRotation(const X, Y, Z, Angle: Double);
var cX, cZ: cardinal;
begin
   if Assigned(FOnRotation)
    then FOnRotation(X, Y, Z, Angle);
   case FEmulationType of
      etNone: ;
      etMouse, etMouseKeyboard: begin
        cX := cardinal(Round(-X*16));
        cZ := cardinal(Round(-Z*16));
        if cX<>0
         then Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, cX, 0);
        if cZ<>0
         then Mouse_Event(MOUSEEVENTF_HWHEEL, 0, 0, cZ, 0);
      end;
      etKeyboard: ;
      etCustom: begin
         HandleCustomDetail(eaRotateX, X);
         HandleCustomDetail(eaRotateY, Y);
         HandleCustomDetail(eaRotateZ, Z);
         HandleCustomDetail(eaRotateAngle, Angle);
      end;
   end;
end;

{*------------------------------------------------------------------------------
  Fires an event that notifies about rotations.

  @param Rotation  Rotation object.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.FireRotation(const Rotation: IAngleAxis);
begin
   FireRotation(Rotation.X, Rotation.Y, Rotation.Z, Rotation.Angle);
end;

{*------------------------------------------------------------------------------
  Fires an event that notifies about translations.

  @param X   X axis (move left/right)
  @param Y   Y axis (move up/down)
  @param Z   Z axis (move forward/backwards)
  @param Length   Length of the vector consisting of the above.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.FireTranslation(const X, Y, Z, Length: Double);
var cX, cY, cZ: integer;
begin
   if Assigned(FOnTranslation)
    then FOnTranslation(X, Y, Z, Length);
   case FEmulationType of
      etNone: ;
      etMouse: Mouse_Event(MOUSEEVENTF_MOVE, Round(X), Round(Z), 0, 0);
      etKeyboard, etMouseKeyboard: begin
         { DONE -oPatrick : Implement keyboard rotation emulation }
         cX := Round(X); // arrow left/right
         if cX>FKeyboardEmulationThreshold
          then Keybd_Event(VK_RIGHT, 0, 0, 0)
         else if cX<-FKeyboardEmulationThreshold
          then Keybd_Event(VK_LEFT, 0, 0, 0)
         else begin
            Keybd_Event(VK_LEFT, 0, KEYEVENTF_KEYUP, 0);
            Keybd_Event(VK_RIGHT, 0, KEYEVENTF_KEYUP, 0);
         end;
         cZ := Round(Z); // arrow up/down
         if cZ>FKeyboardEmulationThreshold
          then Keybd_Event(VK_DOWN, 0, 0, 0)
         else if cZ<-FKeyboardEmulationThreshold
          then Keybd_Event(VK_UP, 0, 0, 0)
         else begin
            Keybd_Event(VK_UP, 0, KEYEVENTF_KEYUP, 0);
            Keybd_Event(VK_DOWN, 0, KEYEVENTF_KEYUP, 0);
         end;
         cY := Round(Y); // page up/down
         if cY>(3*FKeyboardEmulationThreshold)
          then Keybd_Event(VK_PRIOR, 0, 0, 0)
         else if cY<-(3*FKeyboardEmulationThreshold)
          then Keybd_Event(VK_NEXT, 0, 0, 0)
         else begin
            Keybd_Event(VK_PRIOR, 0, KEYEVENTF_KEYUP, 0);
            Keybd_Event(VK_NEXT, 0, KEYEVENTF_KEYUP, 0);
         end;
      end;
      etCustom: begin
         HandleCustomDetail(eaTranslateX, X);
         HandleCustomDetail(eaTranslateY, Y);
         HandleCustomDetail(eaTranslateZ, Z);
         HandleCustomDetail(eaTranslateLength, Length);
      end;
   end;
end;

{*------------------------------------------------------------------------------
  Fires an event that notifies about translations.

  @param Translation   Translation object.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.FireTranslation(const Translation: IVector3D);
begin
   FireTranslation(Translation.X, Translation.Y, Translation.Z, Translation.Length);
end;

function T3DConnexionDevice.GetCustomEmulations(AIndex: TEmulationAxis): TEmulationAxisSetting;
begin
   Result := FCustomEmulations[AIndex];
end;

{*------------------------------------------------------------------------------
  Returns labels and names of device keys/buttons.

  @param List   List that will receive information.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.GetKeyboardKeyInfos(const List: TStrings);
var iKey: integer;
    s: widestring;
begin
   if not Assigned(FDevice)
    then Exit;
   if not Assigned(FKeyboard)
    then Exit;
   for iKey := 1 to FKeyboard.Keys do begin
      try
         s := FKeyboard.GetKeyLabel(iKey) + '=' + FKeyboard.GetKeyName(iKey);
      except
         on E: Exception do s := E.Message;
      end;
      List.Add(s);
   end;
end;

{*------------------------------------------------------------------------------
  Returns labels of device keys/buttons.

  @param List   List that will receive information.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.GetKeyboardKeyLabels(const List: TStrings);
var iKey: integer;
    s: widestring;
begin
   if not Assigned(FDevice)
    then Exit;
   if not Assigned(FKeyboard)
    then Exit;
   for iKey := 1 to FKeyboard.Keys do begin
      try
         s := FKeyboard.GetKeyLabel(iKey);
      except
         on E: Exception do s := 'FKeyboard.GetKeyLabel: ' + E.Message;
      end;
      List.Add(s);
   end;
end;

{*------------------------------------------------------------------------------
  Returns names of device keys/buttons.

  @param List   List that will receive information.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.GetKeyboardKeyNames(const List: TStrings);
var iKey: integer;
    s: widestring;
begin
   if not Assigned(FDevice)
    then Exit;
   if not Assigned(FKeyboard)
    then Exit;
   for iKey := 1 to FKeyboard.Keys do begin
      try
         s := FKeyboard.GetKeyName(iKey);
      except
         on E: Exception do s := 'FKeyboard.GetKeyLabel: ' + E.Message;
      end;
      List.Add(s);
   end;
end;

{*------------------------------------------------------------------------------
  Returns the sensor period.
  This may be the time difference between two polls?

  @return Period.
------------------------------------------------------------------------------*}
function T3DConnexionDevice.GetSensorPeriod: Double;
begin
   if Assigned(FSensor)
    then Result := FSensor.Period
     else Result := 0;
end;

procedure T3DConnexionDevice.HandleCustomDetail(const Axis: TEmulationAxis;
  const Value: Double);
var AxisSettings: TEmulationAxisSetting;
procedure HandleKeyboardEvent(KeyLeft, KeyRight: byte);
var cValue: integer;
    bPressed: boolean;
begin
   cValue := Round(Value * AxisSettings.Multiplier);
   bPressed := Abs(cValue) > AxisSettings.Threshold;
   // Get rid of old states first
   if (FEmulatedKeyPressedStates[Axis, true] and ((cValue<=0) or (not bPressed))) then begin
      // was: positive amplitude, now too small, zero or negative.
      FEmulatedKeyPressedStates[Axis, true] := false;
      Keybd_Event(KeyLeft, 0, KEYEVENTF_KEYUP, 0);
   end else if (FEmulatedKeyPressedStates[Axis, false] and ((cValue>=0) or (not bPressed))) then begin
      // was: negative amplitude, now too small, zero or positive.
      FEmulatedKeyPressedStates[Axis, false] := false;
      Keybd_Event(KeyRight, 0, KEYEVENTF_KEYUP, 0);
   end;
   if not bPressed
    then Exit;
   // Now enter new states:
   if cValue>0 then begin
      FEmulatedKeyPressedStates[Axis, true] := true;
      Keybd_Event(KeyLeft, 0, 0, 0);
   end else begin
      FEmulatedKeyPressedStates[Axis, false] := true;
      Keybd_Event(KeyRight, 0, 0, 0);
   end;
end;
begin
   AxisSettings := CustomEmulations[Axis];
   case AxisSettings.Detail of
      edNone: ;
      edMouseX: Mouse_Event(MOUSEEVENTF_MOVE, cardinal(Round(Value*AxisSettings.Multiplier)), 0, 0, 0);
      edMouseY: Mouse_Event(MOUSEEVENTF_MOVE, 0, cardinal(Round(Value*AxisSettings.Multiplier)), 0, 0);
      edMouseWheelV: Mouse_Event(MOUSEEVENTF_WHEEL, 0, 0, cardinal(Round(-Value*AxisSettings.Multiplier)), 0);
      edMouseWheelH: Mouse_Event(MOUSEEVENTF_HWHEEL, 0, 0, cardinal(Round(-Value*AxisSettings.Multiplier)), 0);
      edKeyboardArrowsLR: HandleKeyboardEvent(VK_RIGHT, VK_LEFT);
      edKeyboardArrowsUD: HandleKeyboardEvent(VK_DOWN, VK_UP);
      edKeyboardPageUD: HandleKeyboardEvent(VK_PRIOR, VK_NEXT);
      edKeyboardCustom: HandleKeyboardEvent(AxisSettings.KeyLeft, AxisSettings.KeyRight);
   end;
end;

{*------------------------------------------------------------------------------
  Polls information on rotation, translation and key stati.
  Information will be returned in corresponding events.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.Poll;
begin
   PollRotation;
   PollTranslation;
   PollKeys;
end;

{*------------------------------------------------------------------------------
  Polls information on key stati.
  Information will be returned in corresponding event.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.PollKeys;
var iKey: integer;
begin
   for iKey := Low(FKeyStates) to High(FKeyStates) do try
      case FKeyStates[iKey] of
         true: if FKeyboard.IsKeyUp(Succ(iKey)) then begin
            FKeyStates[iKey] := false;
            FireKeyChange(iKey, false);
         end;
         false: if FKeyboard.IsKeyDown(Succ(iKey)) then begin
            FKeyStates[iKey] := true;
            FireKeyChange(iKey, true);
         end;
      end;
   except
   end;
end;

{*------------------------------------------------------------------------------
  Polls information on rotation.
  Information will be returned in corresponding event.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.PollRotation;
var rotation: IAngleAxis;
begin
   rotation := FSensor.Rotation;
   FireRotation(rotation);
end;

{*------------------------------------------------------------------------------
  Polls information on translation.
  Information will be returned in corresponding event.
------------------------------------------------------------------------------*}
procedure T3DConnexionDevice.PollTranslation;
var translation: IVector3D;
begin
   translation := FSensor.Translation;
   FireTranslation(translation);
end;

procedure T3DConnexionDevice.GetSensorRotation(var X, Y, Z, Angle: Double);
var rotation: IAngleAxis;
begin
   rotation := FSensor.Rotation;
   X := rotation.X;
   Y := rotation.Y;
   Z := rotation.Z;
   Angle := rotation.Angle;
end;

procedure T3DConnexionDevice.GetSensorTranslation(var X, Y, Z, Length: Double);
var translation: IVector3D;
begin
   translation := FSensor.Translation;
   X := translation.X;
   Y := translation.Y;
   Z := translation.Z;
   Length := translation.Length;
end;

procedure T3DConnexionDevice.SetCustomEmulationAxis(const AIndex: TEmulationAxis;
  const ADetail: TEmulationDetail; const AKeyLeft, AKeyRight: byte;
  const AThreshold, AMultiplier: integer);
begin
   with FCustomEmulations[AIndex] do begin
      Detail := ADetail;
      KeyLeft := AKeyLeft;
      KeyRight := AKeyRight;
      Threshold := AThreshold;
      Multiplier := AMultiplier;
   end;
end;

procedure T3DConnexionDevice.SetCustomEmulations(AIndex: TEmulationAxis;
  const Value: TEmulationAxisSetting);
begin
   FCustomEmulations[AIndex] := Value;
end;

procedure T3DConnexionDevice.SetExampleCustomEmulations;
begin
   SetCustomEmulationAxis(eaRotateX, edMouseWheelV, 0, 0, 0, 16);
   SetCustomEmulationAxis(eaRotateY, edNone, 0, 0, 0, 1);
   SetCustomEmulationAxis(eaRotateZ, edMouseWheelH, 0, 0, 0, 16);
   SetCustomEmulationAxis(eaRotateAngle, edNone, 0, 0, 0, 1);
   SetCustomEmulationAxis(eaTranslateX, edKeyboardArrowsLR, 0, 0, 60, 1);
   SetCustomEmulationAxis(eaTranslateY, edKeyboardPageUD, 0, 0, 60, 1);
   SetCustomEmulationAxis(eaTranslateZ, edKeyboardArrowsUD, 0, 0, 60, 1);
   SetCustomEmulationAxis(eaTranslateLength, edNone, 0, 0, 0, 1);
end;

procedure T3DConnexionDevice.SetFromSettings(const Settings: TEmulationSettings);
var axis: TEmulationAxis;
begin
   for axis := Low(TEmulationAxis) to High(TEmulationAxis)
    do CustomEmulations[axis] := Settings[axis];
end;

procedure LoadSchemeFromFile(var Scheme: TEmulationSettings; const Filename: string);
var mif: TMemIniFile;
    axis: TEmulationAxis;
    sSection: string;
begin
   mif := TMemIniFile.Create(Filename);
   for axis := Low(TEmulationAxis) to High(TEmulationAxis) do begin
      sSection := SEmulationAxisIDs[axis];
      try
         Scheme[axis].Detail := TEmulationDetail(mif.ReadInteger(sSection, 'Detail', integer(Scheme[axis].Detail)));
      except
         Scheme[axis].Detail := edNone;
      end;
      try
         Scheme[axis].KeyLeft := mif.ReadInteger(sSection, 'KeyLeft', Scheme[axis].KeyLeft);
      except
         Scheme[axis].KeyLeft := 0;
      end;
      try
         Scheme[axis].KeyRight := mif.ReadInteger(sSection, 'KeyRight', Scheme[axis].KeyRight);
      except
         Scheme[axis].KeyRight := 0;
      end;
      try
         Scheme[axis].Threshold := mif.ReadInteger(sSection, 'Threshold', Scheme[axis].Threshold);
      except
         Scheme[axis].Threshold := 60;
      end;
      try
         Scheme[axis].Multiplier := mif.ReadInteger(sSection, 'Multiplier', Scheme[axis].Multiplier);
      except
         Scheme[axis].Multiplier := 1;
      end;
   end;
   mif.Free;
end;

procedure SaveSchemeToFile(const Scheme: TEmulationSettings; const Filename: string);
var mif: TMemIniFile;
    axis: TEmulationAxis;
    sSection: string;
begin
   mif := TMemIniFile.Create(Filename);
   for axis := Low(TEmulationAxis) to High(TEmulationAxis) do begin
      sSection := SEmulationAxisIDs[axis];
      mif.WriteInteger(sSection, 'Detail', integer(Scheme[axis].Detail));
      mif.WriteInteger(sSection, 'KeyLeft', Scheme[axis].KeyLeft);
      mif.WriteInteger(sSection, 'KeyRight', Scheme[axis].KeyRight);
      mif.WriteInteger(sSection, 'Threshold', Scheme[axis].Threshold);
      mif.WriteInteger(sSection, 'Multiplier', Scheme[axis].Multiplier);
   end;
   mif.UpdateFile;
   mif.Free;
end;

{$else} // JA: ifndef MSWINDOWS - dummy implementation

type
   T3DConnexionDevice = class(TObject)
   private
      FLoaded: boolean; /// Whether connection to COM object was successful
   public
      constructor Create(const ApplicationName: widestring);
      destructor Destroy; override;
      procedure GetSensorRotation(var X, Y, Z, Angle: Double);
      procedure GetSensorTranslation(var X, Y, Z, Length: Double);
   published
      property Loaded: boolean read FLoaded;
   end;

implementation

constructor T3DConnexionDevice.Create(const ApplicationName: widestring);
begin
   inherited Create;
   FLoaded := false;
end;

destructor T3DConnexionDevice.Destroy;
begin
   inherited;
end;

procedure T3DConnexionDevice.GetSensorRotation(var X, Y, Z, Angle: Double);
begin
   X := 0; Y := 0; Z := 0; Angle := 0;
end;

procedure T3DConnexionDevice.GetSensorTranslation(var X, Y, Z, Length: Double);
begin
   X := 0; Y := 0; Z := 0; Length := 0;
end;

{$endif} // ifndef MSWINDOWS

end.
