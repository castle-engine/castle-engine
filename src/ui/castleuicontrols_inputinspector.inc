{%MainUnit castleuicontrols.pas}
{
  Copyright 2010-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type

  { How to invoke the inspector, see @link(TCastleContainer.InputInspector). }
  TInputInspector = class(TComponent)
  strict private
    Pressed: TFloatTime;
  private
    { Does this TInputPressRelease should toggle the inspector. }
    function IsEvent(const Event: TInputPressRelease): Boolean;

    { Track should we trigger inspector toggle because of PressFingers/PressTime. }
    function IsPressed(const Container: TCastleContainer; const SecondsPassed: Single): Boolean;
  public
    { Key to activate the inspector (set to keyNone to disable).
      By default this is keyF8 in debug builds, and keyNone in release. }
    Key: TKey;

    { Required modifiers to be pressed together with @link(Key).
      This is [] by default (no modifiers).
      Ignored when @link(Key) is keyNone. }
    KeyModifiers: TModifierKeys;

    { Number of fingers necessary to be pressed on a touch device to activate the inspector
      (set to 0 to disable).
      By default this is 3 in debug builds, 0 in release.
      On multi-touch devices (typical mobile devices) this determines
      the number of fingers you need to press. }
    PressFingers: Cardinal;

    { How long do the fingers mentioned in @link(PressFingers) have to be pressed.
      Ignored when @link(PressFingers) is 0. }
    PressTime: TFloatTime;

    { Describe current TInputInspector state. }
    function ToString: String; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TInputInspector ------------------------------------------------------------ }

function TInputInspector.IsEvent(const Event: TInputPressRelease): Boolean;
begin
  Result := Event.IsKey(Key) and (Event.ModifiersDown = KeyModifiers);
end;

function TInputInspector.IsPressed(const Container: TCastleContainer; const SecondsPassed: Single): Boolean;
var
  PressedNow: Boolean;
begin
  Result := false;
  // PressFingers checked first, as it will be false in most release builds
  PressedNow := (PressFingers <> 0) and
    ApplicationProperties.TouchDevice and
    (Container.TouchesCount >= PressFingers);
  if PressedNow then
  begin
    Pressed := Pressed + SecondsPassed;
    if Pressed >= PressTime then
    begin
      Result := true;
      Pressed := 0;
    end;
  end else
    Pressed := 0;
end;

function TInputInspector.ToString: String;
var
  MK: TModifierKey;
begin
  Result := '';

  if Key <> keyNone then
  begin
    for MK in KeyModifiers do
      Result := Result + KeyToStr(ModifierKeyToKey[MK]) + '+';
    Result := Result + KeyToStr(Key);
  end;

  if PressFingers <> 0 then
  begin
    Result := SAppendPart(Result, ' / ', Format('Press %d fingers', [PressFingers]));
  end;
end;

{$endif read_implementation}
