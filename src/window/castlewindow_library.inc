{
  Copyright 2004-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ CastleWindow backend that depends on external code to initialize
  OpenGL context.
  The external code must notify us about events by calling LibraryXxx methods,
  like TCastleWindowCustom.LibraryKeyDown or TCastleApplication.LibraryUpdate.
  The external code will get notified about our events using LibraryCallbackProc. }

{$ifdef read_interface_uses}
CTypes,
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_interface_types}
  TLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32; szParam: pcchar): cInt32; cdecl;
{$endif read_interface_types}

{$ifdef read_window_interface}
public
  LibraryCallbackProc: TLibraryCallbackProc;

  { External library code must notify our window instance about various events
    by calling these methods. }
  procedure LibraryKeyDown(const Key: TKey; const CharKey: char);
  procedure LibraryKeyUp(const Key: TKey);
  procedure LibraryMotion(const Position: TVector2Single;
    const FingerIndex: TFingerIndex);
  procedure LibraryMouseDown(const Position: TVector2Single;
    const Button: CastleKeysMouse.TMouseButton; const FingerIndex: TFingerIndex);
  procedure LibraryMouseUp(const Position: TVector2Single;
    const Button: CastleKeysMouse.TMouseButton; const FingerIndex: TFingerIndex);
  procedure LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
  procedure LibraryResize(const AWidth, AHeight: Integer);
  procedure LibraryRender;
{$endif read_window_interface}

{$ifdef read_application_interface}
public
  procedure LibraryUpdate;
{$endif read_application_interface}

{$ifdef read_implementation}

const
  { Constants used for callbacks.
    Must be synchronized with constants in
    ../library/castlelib_dynloader.pas and
    ../library/castleengine.h }

  // library callback codes
  ecgelibNeedsDisplay          = 0;
  ecgelibSetMouseCursor        = 1;  // sends mouse cursor code in iParam1
  ecgelibNavigationTypeChanged = 2;  // sends ECgeNavigationType in iParam1 (see castleengine.h)
  ecgelibSetMousePosition      = 3;  // sends x in iParam1 and y in iParam2
  ecgelibWarning               = 4;  // sends message in szParam

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

{ TCastleWindowCustom ---------------------------------------------------------- }

procedure TCastleWindowCustom.CreateBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindowCustom.OpenBackend;
begin
  { We just assume that we already have OpenGL context initialized.
    Nothing to do. }
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowCustom.SwapBuffers;
begin
  { The external code must take care of swapping buffers or doing glFlush. }
end;

procedure TCastleWindowCustom.CloseBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindowCustom.Invalidate;
begin
  if not Closed then Invalidated := true;
  if Assigned(LibraryCallbackProc) then  // tell the parent window to repaint
    LibraryCallbackProc(ecgelibNeedsDisplay, 0, 0, nil);
end;

procedure TCastleWindowCustom.BackendMakeCurrent;
begin
  { Nothing to do, we assume external code makes sure this is always
    the current OpenGL context. }
end;

procedure TCastleWindowCustom.SetCursor(const Value: TMouseCursor);
var
  CursorCode: cInt32;
begin
  if FCursor <> Value then
  begin
    FCursor := Value;

    if Assigned(LibraryCallbackProc) then
    begin
      // send to client
      case Value of
        mcNone, mcForceNone: CursorCode := ecgecursorNone;
        mcWait: CursorCode := ecgecursorWait;
        mcHand: CursorCode := ecgecursorHand;
        mcText: CursorCode := ecgecursorText;
        else CursorCode := ecgecursorDefault;
      end;
      LibraryCallbackProc(ecgelibSetMouseCursor, CursorCode, 0, nil);
    end;
  end;
end;

procedure TCastleWindowCustom.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then
    { TODO: call LibraryCallbackProc to change caption to GetWholeCaption };
end;

procedure TCastleWindowCustom.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  FCustomCursor := Value;
  { TODO: call LibraryCallbackProc with custom cursor shape, if you want this }
end;

procedure TCastleWindowCustom.SetMousePosition(const Value: TVector2Single);
begin
  if (not Closed) and Assigned(LibraryCallbackProc) then
    LibraryCallbackProc(ecgelibSetMousePosition, Floor(Value[0]), Floor(Value[1]), nil);
end;

procedure TCastleWindowCustom.SetFullScreen(const Value: boolean);
begin
  { TODO: call LibraryCallbackProc to switch fullscreen mode }
  FFullScreen := Value;
end;

{ LibraryXxx methods --------------------------------------------------------- }

procedure TCastleWindowCustom.LibraryKeyDown(const Key: TKey; const CharKey: char);
begin
  DoKeyDown(Key, CharKey);
end;

procedure TCastleWindowCustom.LibraryKeyUp(const Key: TKey);
begin
  DoKeyUp(Key);
end;

procedure TCastleWindowCustom.LibraryMotion(
  const Position: TVector2Single; const FingerIndex: TFingerIndex);
begin
  { TODO: using MousePressed as Pressed is only OK without multi-touch. }
  DoMotion(InputMotion(FTouches.FingerIndexPosition[FingerIndex], Position, MousePressed, FingerIndex));
end;

procedure TCastleWindowCustom.LibraryMouseDown(
  const Position: TVector2Single; const Button: CastleKeysMouse.TMouseButton;
  const FingerIndex: TFingerIndex);
begin
  DoMouseDown(Position, Button, FingerIndex);
end;

procedure TCastleWindowCustom.LibraryMouseUp(
  const Position: TVector2Single; const Button: CastleKeysMouse.TMouseButton;
  const FingerIndex: TFingerIndex);
begin
  DoMouseUp(Position, Button, FingerIndex);
end;

procedure TCastleWindowCustom.LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
begin
  DoMouseWheel(Scroll, Vertical);
end;

procedure TCastleWindowCustom.LibraryResize(const AWidth, AHeight: Integer);
begin
  DoResize(AWidth, AHeight, false);
end;

procedure TCastleWindowCustom.LibraryRender;
begin
  DoRender;
end;

{ This method is handled at TCastleApplication, this way we handle
  also Application.OnUpdate and Application.OnTimer. }
procedure TCastleApplication.LibraryUpdate;
begin
  DoApplicationUpdate;
  FOpenWindows.DoUpdate;
  MaybeDoTimer;
end;

{ TCastleWindowCustom menu ----------------------------------------------------
  Does not do anything, this backend cannot manage the menu bar. }

procedure TCastleWindowCustom.BackendMenuInitialize;
begin
end;

procedure TCastleWindowCustom.BackendMenuFinalize;
begin
end;

procedure TCastleWindowCustom.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindowCustom.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindowCustom.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
end;

function TCastleWindowCustom.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowCustom.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

procedure TCastleWindowCustom.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

function TCastleWindowCustom.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

{ TCastleWindowCustom dialogs ---------------------------------------------------------- }

{ Methods below should make native-looking dialog boxes. }

function TCastleWindowCustom.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO } Result := false;
end;

function TCastleWindowCustom.ColorDialog(var Color: TVector3Single): boolean;
begin
  { TODO } Result := false;
end;

procedure TCastleWindowCustom.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindowCustom.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  { TODO } Result := true;
end;

{ TCastleApplication ---------------------------------------------------------- }

procedure TCastleApplication.CreateBackend;
begin
end;

procedure TCastleApplication.DestroyBackend;
begin
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
begin
  Result := true;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  Result := ProcessMessage(false, false);
end;

procedure TCastleApplication.Run;
begin
  { for this CastleWindow backend, Run makes no sense, the external code
    must perform event loop }
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
end;

function TCastleApplication.ScreenWidth: integer;
begin
  if OpenWindowsCount <> 0 then
    Result := OpenWindows[0].Width else
    Result := 200; // dummy fallback
end;

function TCastleApplication.ScreenHeight: integer;
begin
  if OpenWindowsCount <> 0 then
    Result := OpenWindows[0].Height else
    Result := 200; // dummy fallback
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'Library (Using Existing OpenGL Context)';
end;

{ TCastleClipboard ----------------------------------------------------------- }

function TCastleClipboard.GetAsText: string;
begin
  { TODO }
  Result := '';
end;

procedure TCastleClipboard.SetAsText(const Value: string);
begin
  { TODO }
end;

{ TCastleWindow -------------------------------------------------------------- }

procedure TCastleWindow.NavigationInfoChanged;
var
  NavValue: integer;
begin
  // send into to parent app (to update navigation buttons state)
  if Assigned(LibraryCallbackProc) then
  begin
    case NavigationType of
      ntWalk     : NavValue := 0;
      ntFly      : NavValue := 1;
      ntExamine  : NavValue := 2;
      ntTurntable: NavValue := 3;
      ntNone     : NavValue := 4;
    end;
    LibraryCallbackProc(ecgelibNavigationTypeChanged, NavValue, 0, nil);
  end;
end;

{$endif read_implementation}
