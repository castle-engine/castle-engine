{
  Copyright 2001-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is the common code for CASTLE_WINDOW_XLIB and CASTLE_WINDOW_WINAPI implementations. }

{$ifdef read_window_interface}
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  { Same as ProcessMessage(WaitForMessage, WaitToLimitFPS),
    but additionally returns under WasAnyMessage was there any message
    processed.

    This ProcessMessage2 is useful to implement ProcessAllMessages.
    ActuallyProcessMessage is implemented using ProcessMessage2
    (and simply ignoring WasAnyMessage). }
  function ProcessMessage2(WaitForMessage, WaitToLimitFPS: boolean;
    var WasAnyMessage: boolean): boolean;
{$endif read_application_interface}

{$ifdef read_implementation}

{ common things for winsystem CastleWindow implementations.
  Uwagi :
   - zawsze musimy w jakis sposob sprawdzic czy okienko dla ktorego
     dostalismy message jest aktualnie na liscie Application.OpenWindows.
     Pamietajmy bowiem ze do naszej kolejki komunikatow mogly
     trafic message'y dla okienek ktore juz zamknelismy - typowym
     przykladem jest gdy zamykamy okienko w reakcji na jakies OnPress.
     Przeciez w kolejce komunikatow moze nam wtedy zostac OnRelease
     (wyslany przez WindowManagera kiedy mysmy robili cos innego.
     User szybko zrobil key down, potem up, dopiero potem my zauwazylismy
     ze dostalismy KeyDown, zrobilismy sobie Close (moze nawet
     zwolnilismy z pamieci obiekt TCastleWindow !!) i potem wywolujemy
     ProcessMessage (bo np. sa jeszcze jakies inne okienka otwarte)
     i dostajemy messaga KeyUp dla okienka ktore juz jest Closed
     albo moze nawet ktorego juz nie ma w pamieci !!

     Dotyczy to zarowno WinAPI jak i Xlib.
}

{ TCastleApplication ---------------------------------------------------------- }

function TCastleApplication.ProcessAllMessages: boolean;
var
  WasAnyMessage: boolean;
begin
  repeat
    Result := ProcessMessage2(false, false, WasAnyMessage);
    if (not Result) or (not WasAnyMessage) then break;
  until false;
end;

function TCastleApplication.ProcessMessage(WaitForMessage, WaitToLimitFPS: boolean): boolean;
var
  Dummy: boolean;
begin
  Result := ProcessMessage2(WaitForMessage, WaitToLimitFPS, Dummy);
end;

function TCastleApplication.ProcessMessage2(WaitForMessage, WaitToLimitFPS: boolean;
  var WasAnyMessage: boolean): boolean;

  { CheckMessage sprawdza czy jest jakis message w naszej queue,
      jesli tak - laduje go do msg/event i wywoluje HandleEvent i zwraca true,
      jesli nie - zwraca false.
    WaitMessage czeka az nasza kolejka bedzie niepusta i
      wtedy laduje message do msg/event i wywoluje HandleEvent.
      W rezultacie WaitMessage ZAWSZE przetworzy jakiegos messaga -
      dziala jak CheckMessage ktore uparlo sie zeby zwrocic true.
    NoMessageProcessed - wywolanie tej proc. oznacza ze w tym
      wywolaniu ProcessMessage CheckMessage zwrocilo false
      i z jakichs zewnetrznych powodow nie mozemy wywolac WaitMessage.
      Wobec tego nie przetworzylismy zadnego messaga - moze implementacja wymaga
      by cos zrobic w tej sytuacji ? (np. pod X-ami nalezy wowczas
      zrobic XFlush).

    Pod X-ami robimy XFlush w NoMessageProcessed i jest implicit XFlush
      w WaitMessage wiec zawsze w wywolaniu ProcessMessage zrobimy jakos
      XFlush - co jest wazne.
  }

  {$ifdef CASTLE_WINDOW_WINAPI}  var msg: TMsg;  {$endif}
  {$ifdef CASTLE_WINDOW_XLIB}    var event: TXEvent;  {$endif}

  procedure HandleEvent;
  begin
    WasAnyMessage := true;

    {$ifdef CASTLE_WINDOW_WINAPI}
    if msg.message = WM_QUIT then
      Terminate { set Terminated := true } else
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    {$endif}

    {$ifdef CASTLE_WINDOW_XLIB}
    HandleXEvent(event);
    {$endif}
  end;

  function CheckMessage: boolean;
  {$ifdef CASTLE_WINDOW_WINAPI}
  begin
    Result := true;
    {seek for message to any of windows of our thread}
    if PeekMessageW(msg, 0, 0, 0, PM_REMOVE) then
      HandleEvent else
    (*{seek for messages to our thread with hWnd = 0 (send with PostThreadMessage)
      (NOT NEEDED NOW)}
    if PeekMessageW(msg, HWND(-1), 0, 0, PM_REMOVE) then
      HandleEvent else *)
      Result := false;
  {$endif}

  {$ifdef CASTLE_WINDOW_XLIB}
  begin
    Result := true;
    if XBool(XCheckMaskEvent(XDisplay, AcceptedEventsMask, @event)) <> XBool_False then
      HandleEvent else
    if XBool(XCheckTypedEvent(XDisplay, ClientMessage, @event)) <> XBool_False then
      HandleEvent else
      Result := false;
  {$endif}
  end;

  procedure WaitMessage;
  begin
    {$ifdef CASTLE_WINDOW_WINAPI}
    Check( LongInt(GetMessageW(msg, 0, 0, 0)) <> -1, 'GetMessageW failed with Result -1.');
    {$endif}
    {$ifdef CASTLE_WINDOW_XLIB}
    XNextEvent(XDisplay, @event); { implicit XFlush }
    {$endif}
    HandleEvent;
  end;

  procedure NoMessageProcessed;
  begin
    {$ifdef CASTLE_WINDOW_XLIB}
    XFlush(XDisplay);
    {$endif}
  end;

var
  WasAnyRendering: boolean;
begin
  WasAnyMessage := false;

  if not CheckMessage then
  begin
    { No messages to us, so call update/timer/render on everything. }
    UpdateAndRenderEverything(WasAnyRendering);
    if Terminated then Exit(false);

    { If we do not do anything in any Application Update/Timer
      (this is checked by AllowSuspendForInput),
      and there's no reason to redisplay,
      and we're not during quit process (started by Application.Quit),
      => then we can wait, giving OS/CPU some rest (as opposed to doing
      "busy waiting"). }
    WaitForMessage := WaitForMessage and Application.AllowSuspendForInput and
      not (WasAnyRendering or Terminated);
    if WaitForMessage then
    begin
      MarkSleeping;
      WaitMessage;
    end else
      NoMessageProcessed;
  end;

  if Terminated then Exit(false);

  if (not WasAnyMessage) and
     (not Terminated) and
     (not WaitForMessage) and
     WaitToLimitFPS then
    DoLimitFPS;

  Result := not Terminated;
end;

procedure TCastleApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;
  inherited Initialize; // set Terminated := false
  inherited Run; // calls DoRun, surrounded by HandleException, in a loop
end;

procedure TCastleApplication.QuitWhenNoOpenWindows;
begin
  Terminate; // set Terminated := true
end;

{$endif read_implementation}
