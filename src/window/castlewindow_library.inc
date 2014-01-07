{
  Copyright 2004-2013 Michalis Kamburelis.

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
  like TCastleWindowBase.LibraryKeyDown or TCastleApplication.LibraryUpdate.
  The external code will get notified about our events using LibraryCallbackProc. }

{$ifdef read_interface_uses}
CTypes,
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_interface_types}
  TLibraryCallbackProc = function (eCode, iParam1, iParam2: cInt32): cInt32; cdecl;
{$endif read_interface_types}

{$ifdef read_window_interface}
public
  LibraryCallbackProc: TLibraryCallbackProc;

  { External library code must notify our window instance about various events
    by calling these methods. }
  procedure LibraryKeyDown(Key: TKey; CharKey: char);
  procedure LibraryKeyUp(key: TKey);
  procedure LibraryMouseMove(X, Y: Integer);
  procedure LibraryMouseDown(X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
  procedure LibraryMouseUp(X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
  procedure LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
  procedure LibraryResize(AWidth, AHeight: Integer);
  procedure LibraryDraw;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  LastDoTimerTime: TMilisecTime;
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

  // mouse cursor codes
  ecgecursorDefault   = 0;
  ecgecursorWait      = 1;
  ecgecursorHand      = 2;
  ecgecursorText      = 3;
  ecgecursorNone      = 4;

{ TCastleWindowBase ---------------------------------------------------------- }

procedure TCastleWindowBase.CreateBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindowBase.OpenBackend;
begin
  { We just assume that we already have OpenGL context initialized.
    Nothing to do. }
  Application.OpenWindowsAdd(Self);
end;

procedure TCastleWindowBase.SwapBuffers;
begin
  { The external code must take care of swapping buffers or doing glFlush. }
end;

procedure TCastleWindowBase.CloseBackend;
begin
  { Nothing to do. }
end;

procedure TCastleWindowBase.PostRedisplay;
begin
  if Assigned(LibraryCallbackProc) then  // tell the parent window to repaint
    LibraryCallbackProc(ecgelibNeedsDisplay, 0, 0);
end;

procedure TCastleWindowBase.BackendMakeCurrent;
begin
  { Nothing to do, we assume external code makes sure this is always
    the current OpenGL context. }
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  DoDraw;
end;

procedure TCastleWindowBase.SetCursor(const Value: TMouseCursor);
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
        mcNone: CursorCode := ecgecursorNone;
        mcWait: CursorCode := ecgecursorWait;
        mcHand: CursorCode := ecgecursorHand;
        mcText: CursorCode := ecgecursorText;
        else CursorCode := ecgecursorDefault;
      end;
      LibraryCallbackProc(ecgelibSetMouseCursor, CursorCode, 0);
    end;
  end;
end;

procedure TCastleWindowBase.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  if not Closed then
    { TODO: call LibraryCallbackProc to change caption to GetWholeCaption };
end;

procedure TCastleWindowBase.SetCustomCursor(const Value: TRGBAlphaImage);
begin
  FCustomCursor := Value;
  { TODO: call LibraryCallbackProc with custom cursor shape, if you want this }
end;

procedure TCastleWindowBase.SetMousePosition(const NewMouseX, NewMouseY: Integer);
begin
  if not Closed then
    { TODO: call LibraryCallbackProc to move mouse cursor to NewMouseX, NewMouseY };
end;

procedure TCastleWindowBase.SetFullScreen(const Value: boolean);
begin
  { TODO: call LibraryCallbackProc to switch fullscreen mode }
  FFullScreen := Value;
end;

{ LibraryXxx methods --------------------------------------------------------- }

procedure TCastleWindowBase.LibraryKeyDown(Key: TKey; CharKey: char);
begin
  DoKeyDown(Key, CharKey);
end;

procedure TCastleWindowBase.LibraryKeyUp(Key: TKey);
begin
  DoKeyUp(Key);
end;

procedure TCastleWindowBase.LibraryMouseMove(X, Y: Integer);
begin
  DoMouseMove(X, Y);
end;

procedure TCastleWindowBase.LibraryMouseDown(X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
begin
  DoMouseDown(X, Y, Btn);
end;

procedure TCastleWindowBase.LibraryMouseUp(X, Y: Integer; Btn: CastleKeysMouse.TMouseButton);
begin
  DoMouseUp(X, Y, Btn);
end;

procedure TCastleWindowBase.LibraryMouseWheel(const Scroll: Single; const Vertical: boolean);
begin
  DoMouseWheel(Scroll, Vertical);
end;

procedure TCastleWindowBase.LibraryResize(AWidth, AHeight: Integer);
begin
  DoResize(AWidth, AHeight, false);
end;

procedure TCastleWindowBase.LibraryDraw;
begin
  DoDraw;
end;

{ This method is handled at TCastleApplication, this way we handle
  also Application.OnUpdate and Application.OnTimer. }
procedure TCastleApplication.LibraryUpdate;
begin
  DoSelfUpdate;
  FOpenWindows.DoUpdate;
  MaybeDoTimer(LastDoTimerTime);
end;

{ TCastleWindowBase menu ----------------------------------------------------
  Does not do anything, this backend cannot manage the menu bar. }

procedure TCastleWindowBase.BackendMenuInitialize;
begin
end;

procedure TCastleWindowBase.BackendMenuFinalize;
begin
end;

procedure TCastleWindowBase.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindowBase.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
end;

procedure TCastleWindowBase.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
end;

function TCastleWindowBase.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindowBase.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

procedure TCastleWindowBase.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
end;

function TCastleWindowBase.RedirectKeyDownToMenuClick: boolean;
begin
  Result := true;
end;

{ TCastleWindowBase dialogs ---------------------------------------------------------- }

{ Methods below should make native-looking dialog boxes. }

function TCastleWindowBase.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO } Result := false;
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
begin
  { TODO } Result := false;
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindowBase.MessageYesNo(const S: string;
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
  Result := true;
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

procedure TCastleWindow.NavigationInfoChanged(Sender: TObject);
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
    LibraryCallbackProc(ecgelibNavigationTypeChanged, NavValue, 0);
  end;
end;

{$endif read_implementation}
