{
  Copyright 2001-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is the common code for GLWINDOW_XLIB and GLWINDOW_WINAPI implementations.

  Note that if you let some exception to be raised out of
  some event (like OnXxx) then this window may be closed
  while recovering from this exception. I.e. GLWindow implementation
  is free to implement part of ProcessMessage like

@longCode(#
  if HasKeyDown then
  try
    OnKeyDown(...);
  except
    Close;
    raise;
  end;
#)

  TODO: this behavior is currently done only by Xlib and WinAPI
  implementation, in glwindow_winsystem.inc.
  Is there really any good reason why we can't remove this behavior?
}



{$ifdef read_window_interface}
private
  { Track if some PostRedisplay was called (and not realized yet by
    DoDraw call). PostRedisplay only sets this, leaving actually
    calling of FlushRedisplay for later.

    Note: Xlib program should always wait for the first Expose event before
    redrawing anything. Similar for WinAPI. }
  RedisplayPosted: boolean;

  { w GLWindow_winsystem implementuje sie OpenBackend ktore
    wywoluje swoja inicjalizacje a potem wywoluje OpenWinSystemDepend
    ktore powinno byc zdefiniowane w GLWindow_xlib i _winapi. }
  procedure OpenWinSystemDepend;

  { FlushRedisplay2 works exactly like FlushRedisplay, but additionally
    it returns czy rzeczywiscie zaszlo odswiezenie (true)
    czy tez nie wykryto takiej potrzeby (nie bylo zaleglego PostRedisplay
    ani komunikatu w rodzaju Expose od WindowManagera do wykonania) }
  function FlushRedisplay2: boolean;
{$endif read_window_interface}

{$ifdef read_application_interface}
private
  LastDoTimerTime: TMilisecTime;
  quitPosted: boolean;

  { This implements the same function as Result := ProcessMessage(AllowSuspend);
    but additionally returns under WasAnyMessage was there any message
    processed.

    If AllowSuspend then always WasAnyMessage = true (but no, you should
    not depend on it as long as possible; however, it's still private
    inside GLWindow_winsystem so if you HAVE to you can depend on it).
    So value WasAnyMessage is really usable to you only when you pass
    AllowSuspend = false.

    This ProcessMessage2 is useful to implement ProcessAllMessages.
    ActuallyProcessMessage is implemented using ProcessMessage2
    (and simply ignoring WasAnyMessage). }
  function ProcessMessage2(AllowSuspend: boolean; var WasAnyMessage: boolean): boolean;
{$endif read_application_interface}

{$ifdef read_implementation}

{ common things for winsystem GLWindow implementations.
  Uwagi :
   - zawsze musimy w jakis sposob sprawdzic czy okienko dla ktorego
     dostalismy message jest aktualnie na liscie Application.OpenWindows.
     Pamietajmy bowiem ze do naszej kolejki komunikatow mogly
     trafic message'y dla okienek ktore juz zamknelismy - typowym
     przykladem jest gdy zamykamy okienko w reakcji na jakies OnKeyDown.
     Przeciez w kolejce komunikatow moze nam wtedy zostac OnKeyUp
     (wyslany przez WindowManagera kiedy mysmy robili cos innego.
     User szybko zrobil key down, potem up, dopiero potem my zauwazylismy
     ze dostalismy KeyDown, zrobilismy sobie Close (moze nawet
     zwolnilismy z pamieci obiekt TCastleWindowBase !!) i potem wywolujemy
     ProcessMessage (bo np. sa jeszcze jakies inne okienka otwarte)
     i dostajemy messaga KeyUp dla okienka ktore juz jest Closed
     albo moze nawet ktorego juz nie ma w pamieci !!

     Dotyczy to zarowno WinAPI jak i Xlib.
}

{ TCastleWindowBase ----------------------------------------------------------------- }

procedure TCastleWindowBase.PostRedisplay;
begin
  RedisplayPosted := true;
end;

procedure TCastleWindowBase.OpenBackend;
begin
  { reset some variables common to all _winsystem implementations }
  RedisplayPosted := false;
  { call specific winsystem impl Open }
  OpenWinSystemDepend;
end;

function TCastleWindowBase.FlushRedisplay2: boolean;
begin
  Result := RedisplayPosted;
  if Result then
  begin
    RedisplayPosted := false;
    DoDraw;
  end;
end;

procedure TCastleWindowBase.FlushRedisplay;
begin
  FlushRedisplay2;
end;

{ TGLApplication ---------------------------------------------------------- }

function TGLApplication.ProcessAllMessages: boolean;
var
  WasAnyMessage: boolean;
begin
  repeat
    Result := ProcessMessage2(false, WasAnyMessage);
    if (not Result) or (not WasAnyMessage) then break;
  until false;
end;

function TGLApplication.ProcessMessage(AllowSuspend: boolean): boolean;
var
  Dummy: boolean;
begin
  Result := ProcessMessage2(AllowSuspend, dummy);
end;

function TGLApplication.ProcessMessage2(AllowSuspend: boolean;
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

  {$ifdef GLWINDOW_WINAPI}  var msg: TMsg;  {$endif}
  {$ifdef GLWINDOW_XLIB}    var event: TXEvent;  {$endif}

  procedure HandleEvent;
  {$ifdef GLWINDOW_XLIB}
  var
    Window: TCastleWindowBase;
    evtype: longint;
  {$endif}
  begin
    WasAnyMessage := true;

    {$ifdef GLWINDOW_WINAPI}
    if msg.message = WM_QUIT then
      quitPosted := true else
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
    {$endif}

    {$ifdef GLWINDOW_XLIB}
    evtype := event.{$ifdef DELPHI} xtype {$else} _type {$endif};
    case evtype of
      MappingNotify: XRefreshKeyboardMapping(@event);
      else
      begin
        { zdarzenia ktore sa skierowane do konkretnego TCastleWindowBase }
        { sprawdzamy czy okienko jest na liscie Application.OpenWindows, przy okazji
          zamieniajac windowXID na obiekt klasy TCastleWindowBase. }
        Window := Application.FindWindowXID(event.xany. {$ifdef DELPHI} xwindow {$else} window {$endif});
        if Window <> nil then
          case evtype of
            Expose: { XExposeEvent }
               if event.xexpose.count = 0 then Window.PostRedisplay;
            (*MapNotify: needRedisplay := true; { XMapEvent } <- niepotrzebne *)
            KeyPress: Window.X_KeyPress(event.xkey);  { XKeyEvent }
            KeyRelease: Window.X_KeyRelease(event.xkey);
            ButtonPress: Window.X_ButtonPress(event.xbutton); { XButtonPressEvent }
            ButtonRelease: Window.X_ButtonRelease(event.xbutton); { XButtonReleaseEvent }
            MotionNotify: Window.X_MotionNotify(event.xmotion); { XPointerMovedEvent }
            ConfigureNotify: Window.X_ConfigureNotify(event.xconfigure); { XConfigureEvent }
            ClientMessage : { XClientMessageEvent - nie ma na to maski }
              if (event.xclient.data.l[0] = Integer(wmDeleteWindow)) then
               Window.DoCloseQuery;
            ReparentNotify : Window.parentwinXID := event.xreparent.parent;
            FocusOut: Window.X_FocusOut(event.xfocus);
          end;
      end;
    end;
    {$endif}
  end;

  function CheckMessage: boolean;
  begin
    {$ifdef GLWINDOW_WINAPI}
    Result := true;
    {seek for message to any of windows of our thread}
    if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
      HandleEvent else
    (*{seek for messages to our thread with hWnd = 0 (send with PostThreadMessage)
      (NOT NEEDED NOW)}
    if PeekMessage(msg, HWND(-1), 0, 0, PM_REMOVE) then
      HandleEvent else *)
      Result := false;
    {$endif}

    {$ifdef GLWINDOW_XLIB}
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
    {$ifdef GLWINDOW_WINAPI}
    Check( LongInt(GetMessage(msg, 0, 0, 0)) <> -1, 'GetMessage failed with Result -1.');
    {$endif}
    {$ifdef GLWINDOW_XLIB}
    XNextEvent(XDisplay, @event); { implicit XFlush }
    {$endif}
    HandleEvent;
  end;

  procedure NoMessageProcessed;
  begin
    {$ifdef GLWINDOW_XLIB}
    XFlush(XDisplay)
    {$endif}
  end;

var
  WasAnyDrawing: boolean;
  I: integer;
  Window: TCastleWindowBase;
begin
  WasAnyMessage := false;
  try
    if OpenWindowsCount <> 0 then
    begin
      if not CheckMessage then
      begin
        { no messages to us - call DoSelfIdle now.
          Wywolujemy OnIdle teraz bo :
           - gbybysmy wywolywali OnIdle w kazdym wywolaniu ProcessMessage
             to doprowadzalisbysmy do tego ze np. ruch mysza powoduje
             pozorne przyspieszenie rysowania - bo gdy user rusza mysza
             nasza petla jest zajeta przetwarzaniem messagy i w zwiazku z tym
             nie wykonuje zadnych OnDraw ktore sa przeciez najbardziej
             opozniajaca czescia programu. W rezultacie wykonywanych jest
             mnostwo razy OnIdle ktore, mimo ze zmieniaja jakas zmienna
             i wywoluja PostRedisplay to jednak czekaja sporo na
             spowodowane przez siebie redisplay (bo ciagle zamiast redisplay'a
             przetwarzamy ruchy myszka). Wiec aby OnIdle byly wywolywane
             mniej wiecej z taka sama czestotliwoscia co OnDraw'y
             musimy umiescic wywolanie OnIdle tutaj - tuz przed wywolywaniem
             samych OnDraw'ow.
           - gdybysmy wywolywali OnIdle dopiero do ponizszych drawach
             jezeli not WasAnyDrawing to z kolei zdarzenia OnDraw
             (ktore np. same sobie juz wywoluja PostRedisplay) moglyby
             hamowac wywolywanie zdarzen OnIdle.
             A tego nie chcemy - patrz komentarze w interfejsie,
             nadrzednym celem jest by OnIdle bylo wykonywane mniej
             wiecej tak czesto co OnDraw.  }
        DoSelfIdle;

        { redraw some windows; we do it only if our event queue is empty.
          Przy okazji robimy DoIdle okien, majac nadzieje ze gromadzac
            blisko siebie callbacki tych samych okien zminimalizujemy koszty
            ciaglego zmieniania aktywnego kontekstu OpenGL'a.
          Ponizszy kod wydaje sie miec paranoje na punkcie zamykania okna - pamietajmy
            ze Closed moze zostac wywolane na oknie w kazdym callbacku, takze w OnIdle
            i OnDraw. A Close usuwa to okno z listy OpenWindows, dlatego kopiujemy sobie
            na poczatku OpenWindows[i] do Window. }
        WasAnyDrawing := false;
        I := 0;
        while I < OpenWindowsCount do
        begin
          Window := OpenWindows[I];
          Window.DoIdle;
          if Window.Closed then Continue {don't Inc(I)};
          { Kolejnosc w OR ponizej MA ZNACZENIE : zawsze chemy wywolac
            FlushRedisplay2, nawet jesli WasAnyDrawing juz jest = true }
          WasAnyDrawing := Window.FlushRedisplay2 or WasAnyDrawing;
          if Window.Closed then Continue {don't Inc(I)};
          Inc(I);
        end;

        { If we do not do anything in any idle/timer
          (this is checked by AllowSuspendForInput),
          and there's no reason to redisplay,
          and we're not during quit process (started by Application.Quit),
          => then there's no need to finish ProcessMessage.

          ProcessMessage may assume in this case it works like
            while ProcessMessage do ;
          so returning with @true means that the next ProcessMessage will have
          the same state (since no callbacks assigned etc., there's nothing
          to change our state). And we'll do "busy waiting", wasting CPU cycles
          on a loop that does nothing.

          In this case it's much better to do blocking call for next input
          event, by GetMessage / XNextMesage etc. This way we explicitly say
          that we want to wait, doing nothing, and our process should be
          suspended by OS until some event is received. Looking at any
          system monitor, you'll see we do not use CPU then. }
        if AllowSuspend and Application.AllowSuspendForInput and
           not (WasAnyDrawing or QuitPosted) then
          WaitMessage else
          NoMessageProcessed;
      end;

    end;

    if QuitPosted then Exit(false);

    MaybeDoTimer(LastDoTimerTime);

    Result := true;
  except
    { call Quit (silencing any further exceptions) + reraise }
    try Quit; except end;
    raise
  end;
end;

procedure TGLApplication.Run;
begin
  if OpenWindowsCount = 0 then Exit;

  QuitPosted := false;
  while ProcessMessage(true) do ;
end;

procedure TGLApplication.QuitWhenNoOpenWindows;
begin
  QuitPosted := true;
end;

{$endif read_implementation}