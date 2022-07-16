{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Implementation of TCastleWindow.MessageOK, MessageYesNo (Cocoa alerts)

  Lots of inspiration from LCL Cocoa widgetset code.
}

var
  AlertEnded: Boolean;
  AlertResultCode: NSModalResponse;

procedure AlertCompletionHandler(ResultCode: NSModalResponse);
  {cdecl; No need to change calling convention, looking at examples}
begin
  AlertEnded := true;
  AlertResultCode := ResultCode;
end;

type
  TAlertCompletionHandler = reference to procedure(ResultCode: NSModalResponse); cdecl; cblock;

function TCastleWindow.CocoaPromptUser(const DialogCaption: String;
  const DialogMessage: String;
  const AlertStyle: NSAlertStyle;
  const ButtonCaptions: array of String;
  const ButtonResults: array of NSInteger): NSInteger;
var
  anAlert: NSAlert;
  informativeText: NSString;
  messageText: NSString;
  I: Integer;
  aButton: NSButton;
begin
  Assert(ref <> nil);
  Assert(High(ButtonCaptions) = High(ButtonResults));
  Assert(High(ButtonCaptions) >= 0);

  Result := -1;
  AnAlert := NSAlert.alloc.init;
  try
    informativeText := NSSTR(DialogMessage);
    messageText := NSSTR(DialogCaption);
    anAlert.setAlertStyle(AlertStyle);
    anAlert.setInformativeText(informativeText);
    anAlert.setMessageText(messageText);

    for I := Low(ButtonCaptions) to High(ButtonCaptions) do
    begin
      aButton := anAlert.addButtonWithTitle(NSSTR(ButtonCaptions[I]));
      aButton.setKeyEquivalentModifierMask(0);

      // call aButton.setKeyEquivalent
      if (ButtonCaptions[I] = 'OK') or
         (ButtonCaptions[I] = 'Yes') then
        aButton.setKeyEquivalent(NSSTR(#13))
      else
      if ButtonCaptions[I] = 'No' then
        aButton.setKeyEquivalent(NSSTR(#27))
      else
        // By default, the first button is the default button. If in our
        // case this should not be the case, remove the default status
        // from the first button.
        aButton.setKeyEquivalent(NSSTR(''));

      aButton.setTag(ButtonResults[I]);
    end;

    AlertResultCode := -1;
    AlertEnded := false;

    { Using beginSheetModalForWindow_completionHandler with callback
      based on https://wiki.freepascal.org/macOS_NSAlert

      This requires casting AlertCompletionHandler to "cblock", see
      https://forum.lazarus.freepascal.org/index.php?topic=57724.0
        The OpaqueCBlocks predated FPC support for cblocks. So they're basically untyped parameters used as placeholders for cblock parameters.
      https://wiki.freepascal.org/FPC_New_Features_3.2.0#Support_for_interfacing_with_C_blocks_functionality
      https://gitlab.com/freepascal.org/fpc/source/-/blob/main/tests/test/tblock1.pp
      https://gitlab.com/freepascal.org/fpc/source/-/blob/main/tests/test/tblock2.pp
    }

    anAlert.beginSheetModalForWindow_completionHandler(ref,
      TAlertCompletionHandler(@AlertCompletionHandler));

    { Using MenuForcefullyDisabled is important here, as we run
      Application.ProcessMessage and in general process all events as usual
      underneath the message box. }
    MenuForcefullyDisabled := true;
    try
      while not AlertEnded do
        if not Application.ProcessMessage(true, true) then
          Break;
    finally
      MenuForcefullyDisabled := false;
    end;

    if AlertEnded then
      Result := AlertResultCode;
  finally
    AnAlert.release;
  end;

  { Make the ref again receive keys, otherwise closing the dialog
    with key makes weird experience -- further key presses will not be send
    to the application window you're looking at. }
  ref.makeKeyAndOrderFront(nil);
end;

const
  CaptionFromMessageType: array [TWindowMessageType] of String = (
    'Information',
    'Warning',
    'Question',
    'Error',
    'Message' // mtOther
  );
  AlertStyleFromMessageType: array [TWindowMessageType] of NSAlertStyle = (
    NSInformationalAlertStyle, // mtInfo
    NSWarningAlertStyle, // mtWarning
    NSInformationalAlertStyle, // mtQuestion
    NSCriticalAlertStyle, // mtError
    NSInformationalAlertStyle // mtOther
  );

procedure TCastleWindow.MessageOK(const S: String; const MessageType: TWindowMessageType);
begin
  CocoaPromptUser(CaptionFromMessageType[MessageType], S,
    AlertStyleFromMessageType[MessageType], ['OK'], [0]);
end;

function TCastleWindow.MessageYesNo(const S: String;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := CocoaPromptUser(CaptionFromMessageType[MessageType], S,
    AlertStyleFromMessageType[MessageType], ['Yes', 'No'], [0, 1]) = 0;
end;
