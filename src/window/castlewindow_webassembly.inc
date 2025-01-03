{%MainUnit castlewindow.pas}
{
  Copyright 2004-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface_uses}
CastleInternalJobWeb,
{$endif}

{$ifdef read_implementation_uses}
{$endif}

{$ifdef read_window_interface}
strict private
  Canvas: IJSHTMLCanvasElement;
{$endif read_window_interface}

{$ifdef read_application_interface}
strict private
  CanvasAnimationHandler: Integer;
  HasPreviousTime: Boolean;
  PreviousTime: TDOMHighResTimeStamp;
  { Callback for requestAnimationFrame
    ( https://developer.mozilla.org/en-US/docs/Web/API/Window/requestAnimationFrame ) }
  procedure AnimationFrame(Time: TDOMHighResTimeStamp);
{$endif read_application_interface}

{$ifdef read_implementation}

{ TCastleWindow ------------------------------------------------------------------ }

procedure TCastleWindow.CreateBackend;
begin
end;

procedure TCastleWindow.BackendMenuInitialize;
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.BackendMenuFinalize;
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateCaption(Entry: TMenuEntryWithCaption);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateEnabled(Entry: TMenuEntryWithCaption);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuUpdateChecked(Entry: TMenuItemChecked);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

function TCastleWindow.MenuUpdateCheckedFast: boolean;
begin
  Result := false;
end;

procedure TCastleWindow.MenuInsert(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.MenuDelete(const Parent: TMenu;
  const ParentPosition: Integer; const Entry: TMenuEntry);
begin
  // Menu not supported by WebAssembly TCastleWindow backend
end;

procedure TCastleWindow.SwapBuffers;
begin
  { No need to do anything, according to MDN we don't even need to do glFlush
    when using RequestAnimationFrame. }
end;

procedure TCastleWindow.OpenBackend;

  procedure DebugLog;
  begin
    // LogGLInformationVerbose := true;
    WritelnLog(Format('Initial WebGL context Width x Height: %d x %d', [
      Integer(GL.DrawingBufferWidth),
      Integer(GL.DrawingBufferHeight)
    ]));
  end;

begin
  Canvas := TJSHTMLCanvasElement.Cast(JSDocument.getElementById('castle-canvas'));

  GL2 := TJSWebGL2RenderingContext.Cast(Canvas.getContext('webgl2'));
  if GL2 <> nil then
  begin
    // WebGL 2.0 is a superset of WebGL 1.0
    GL := TJSWebGLRenderingContext.Cast(GL2);
    WritelnLog('WebGL 2.0 context initialized from WebAssembly');
  end else
  begin
    GL := TJSWebGLRenderingContext.Cast(Canvas.getContext('webgl'));
    if GL = nil then
      raise Exception.Create('Failed to load WebGL context (2.0 or 1.0) from WebAssembly');
    WritelnLog('WebGL 1.0 context initialized from WebAssembly');
  end;

  // Tell TCastleWindow about the real window size
  DoResize(GL.DrawingBufferWidth, GL.DrawingBufferHeight, false);

  DebugLog;

  Application.OpenWindowsAdd(Self);

  // TODO
  // GetInitialCursorPos;
  // UpdateCursor;
  // InitializeDpi;
end;

procedure TCastleWindow.CloseBackend;
begin
  { One doesn't close WebGL context. }
end;

procedure TCastleWindow.SetCaption(const Part: TCaptionPart; const Value: string);
begin
  FCaption[Part] := Value;
  // No typical way to use Window.Caption from WASM
  // if not Closed then Something := GetWholeCaption
end;

procedure TCastleWindow.BackendMakeCurrent;
begin
  // No need for this, we always call WebGL through GL/GL2 which are context
end;

procedure TCastleWindow.SetCursor(const Value: TMouseCursor);
begin
  if FCursor <> Value then
  begin
    FCursor := Value;
    if not Closed then
      { TODO UpdateCursor };
  end;
end;

function TCastleWindow.RedirectKeyDownToMenuClick: boolean;
begin
  Result := { TODO } true;
end;

procedure TCastleWindow.SystemSetMousePosition(const Value: TVector2);
begin
  { TODO }
end;

procedure TCastleWindow.UpdateFullScreenBackend;
begin
  { This is the simplest implementation of this method: }
  SimpleUpdateFullScreenBackend;
  { Some backends may provide a better way of doing this.
    Remove the above line then, and put better implementation here. }
  // TODO: go to fullscreen in WebGL
end;

procedure TCastleWindow.BackendInsideUpdate;
begin
end;

{ TCastleWindow dialogs -------------------------------------------------- }

{ Methods below should make native-looking dialog boxes.
  Alternatively, you can include castlewindow_dialogs_by_messages.inc
  (outside of $ifdef read_implementation, usually at the beginning of
  castlewindow_xxx.inc file), to have simple dialog boxes drawn on
  OpenGL context using our CastleMessages unit. }

function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  { TODO } Result := false;
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): Boolean;
begin
  { TODO } Result := false;
end;

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  { TODO }
end;

function TCastleWindow.MessageYesNo(const S: string;
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
  { Simplest implementation. This will be done in a loop using RequestAnimationFrame. }
  UpdateAndRenderEverything;
  Result := not Terminated;
end;

function TCastleApplication.ProcessAllMessages: boolean;
begin
  { Simplest implementation. }
  Result := ProcessMessage(false, false);
end;

procedure TCastleApplication.AnimationFrame(Time: TDOMHighResTimeStamp);
var
  DeltaTime: Float;
begin
  { TODO: recreate context.
    See https://developer.mozilla.org/en-US/docs/Web/API/WebGLRenderingContext/isContextLost }
	if GL.isContextLost then
    raise Exception.Create('Context has been lost');

  // calculate DeltaTime, update HasPreviousTime and PreviousTime
  if HasPreviousTime then
    DeltaTime := (Time - PreviousTime) / 1000
  else
  begin
    DeltaTime := 1 / 60; // arbitrary initial value
    HasPreviousTime := True;
  end;
  PreviousTime := Time;

  // TODO: Pass DeltaTime to our FPS, passing Time to measure time

  // Call Update, Render on all TCastleWindow
  ProcessAllMessages;

  // schedule next frame
  CanvasAnimationHandler := JSWindow.RequestAnimationFrame(@AnimationFrame);
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then
    Exit;

  // TODO just once for now
  ProcessAllMessages;

  // TODO make loop
  //CanvasAnimationHandler := JSWindow.RequestAnimationFrame(@AnimationFrame);
end;

procedure TCastleApplication.BackendTerminate;
begin
end;

function TCastleApplication.ScreenWidth: integer;
begin
  if GL <> nil then
    Result := GL.DrawingBufferWidth
  else
  begin
    WritelnWarning('Querying Application.ScreenWidth before GL context is initialized returns arbitrary value 64');
    Result := 64;
  end;
end;

function TCastleApplication.ScreenHeight: integer;
begin
  if GL <> nil then
    Result := GL.DrawingBufferHeight
  else
  begin
    WritelnWarning('Querying Application.ScreenHeight before GL context is initialized returns arbitrary value 64');
    Result := 64;
  end;
end;

function TCastleApplication.BackendName: string;
begin
  Result := 'WebAssembly';
end;

{ TWindowContainer ----------------------------------------------------------- }

function TWindowContainer.SettingMousePositionCausesMotion: Boolean;
begin
  { TODO: check, using window_events example, what is the correct value
    (press "5", see if OnMotion is generated). }
  Result := true;
end;

{$endif read_implementation}
