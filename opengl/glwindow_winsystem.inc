{
  Copyright 2001-2005 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ This is the common code for GLWINDOW_XLIB and GLWINDOW_WINAPI implementations. }

{$ifdef read_tglwindow_interface}
private
  { program w Xlib powinien czekac na pierwsze zdarzenie Expose zanim
    narysuje COKOLWIEK; podobnie Windows, dlatego redisplayPosted jest w
    Init inicjowane na false. }
  redisplayPosted: boolean;

  { w GLWindow_winsystem implementuje sie InitImplDepend ktore
    wywoluje swoja inicjalizacje a potem wywoluje InitWinSystemDepend
    ktore powinno byc zdefiniowane w GLWindow_xlib i _winapi. }
  procedure InitWinSystemDepend;

  { FlushRedisplay2 works exactly like FlushRedisplay, but additionally
    it returns czy rzeczywiscie zaszlo odswiezenie (true)
    czy tez nie wykryto takiej potrzeby (nie bylo zaleglego PostRedisplay
    ani komunikatu w rodzaju Expose od WindowManagera do wykonania) }
  function FlushRedisplay2: boolean;
{$endif read_tglwindow_interface}

{$ifdef read_tglwindowmanager_interface}
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
{$endif read_tglwindowmanager_interface}

{$ifdef read_implementation}

{ common things for winsystem GLWindow implementations.
  Uwagi :
   - zawsze musimy w jakis sposob sprawdzic czy okienko dla ktorego
     dostalismy message jest aktualnie na liscie glwm.Active.
     Pamietajmy bowiem ze do naszej kolejki komunikatow mogly
     trafic message'y dla okienek ktore juz zamknelismy - typowym
     przykladem jest gdy zamykamy okienko w reakcji na jakies OnKeyDown.
     Przeciez w kolejce komunikatow moze nam wtedy zostac OnKeyUp
     (wyslany przez WindowManagera kiedy mysmy robili cos innego.
     User szybko zrobil key down, potem up, dopiero potem my zauwazylismy
     ze dostalismy KeyDown, zrobilismy sobie Close (moze nawet
     zwolnilismy z pamieci obiekt TGLWindow !!) i potem wywolujemy
     ProcessMessage (bo np. sa jeszcze jakies inne okienka otwarte)
     i dostajemy messaga KeyUp dla okienka ktore juz jest Closed
     albo moze nawet ktorego juz nie ma w pamieci !!

     Dotyczy to zarowno WinAPI jak i Xlib.
}

{ TGLWindow ----------------------------------------------------------------- }

procedure TGLWindow.PostRedisplay;
begin
 redisplayPosted := true;
end;

procedure TGLWindow.InitImplDepend;
begin
 { reset some variables common to all _winsystem implementations }
 redisplayPosted := false;
 { call specific winsystem impl Init }
 InitWinSystemDepend;
end;

function TGLWindow.FlushRedisplay2: boolean;
begin
 result := redisplayPosted;
 if result then
 begin
  redisplayPosted := false;
  DoDraw;
 end;
end;

procedure TGLWindow.FlushRedisplay;
begin
 FlushRedisplay2;
end;

{ TGLWindowsManager ---------------------------------------------------------- }

function TGLWindowsManager.ProcessAllMessages: boolean;
var WasAnyMessage: boolean;
begin
 repeat
  result := ProcessMessage2(false, WasAnyMessage);
  if (not result) or (not WasAnyMessage) then break;
 until false;
end;

function TGLWindowsManager.ProcessMessage(AllowSuspend: boolean): boolean;
var dummy: boolean;
begin
 result := ProcessMessage2(AllowSuspend, dummy);
end;

function TGLWindowsManager.ProcessMessage2(AllowSuspend: boolean;
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
  var glwin: TGLWindow;
      evtype: longint;
  {$endif}
  begin
   WasAnyMessage := true;

   {$ifdef GLWINDOW_WINAPI}
   {tests: LogWrite(MessageDescr(msg));}
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
    begin { zdarzenia ktore sa skierowane do konkretnego TGLWindow }
     { sprawdzamy czy okienko jest na liscie glwm.Active, przy okazji
       zamieniajac windowXID na obiekt klasy TGLWindow. }
     glwin := glwm.ActiveWindowXID(event.xany. {$ifdef DELPHI} xwindow {$else} window {$endif});
     if glwin <> nil then
      case evtype of
       Expose: { XExposeEvent }
	  if event.xexpose.count = 0 then glwin.PostRedisplay;
     (*MapNotify: needRedisplay := true; { XMapEvent } <- niepotrzebne *)
       KeyPress: glwin.X_KeyPress(event.xkey);  { XKeyEvent }
       KeyRelease: glwin.X_KeyRelease(event.xkey);
       ButtonPress: glwin.X_ButtonPress(event.xbutton); { XButtonPressEvent }
       ButtonRelease: glwin.X_ButtonRelease(event.xbutton); { XButtonReleaseEvent }
       MotionNotify: glwin.X_MotionNotify(event.xmotion); { XPointerMovedEvent }
       ConfigureNotify: glwin.X_ConfigureNotify(event.xconfigure); { XConfigureEvent }
       ClientMessage : { XClientMessageEvent - nie ma na to maski }
	 if (event.xclient.data.l[0] = Integer(wmDeleteWindow)) then
          glwin.DoCloseQuery;
       ReparentNotify : glwin.parentwinXID := event.xreparent.parent;
       FocusOut: glwin.X_FocusOut(event.xfocus);
      end;
    end;
   end;
   {$endif}
  end;

  function CheckMessage: boolean;
  begin
   {$ifdef GLWINDOW_WINAPI}
   result := true;
   {seek for message to any of windows of our thread}
   if PeekMessage(msg, 0, 0, 0, PM_REMOVE) then
    HandleEvent else
 (*{seek for messages to our thread with hWnd = 0 (send with PostThreadMessage)
    (NOT NEEDED NOW)}
   if PeekMessage(msg, HWND(-1), 0, 0, PM_REMOVE) then
    HandleEvent else *)
    result := false;
   {$endif}

   {$ifdef GLWINDOW_XLIB}
   result := true;
   if XBool(XCheckMaskEvent(XDisplay, AcceptedEventsMask, @event)) <> XBool_False then
    HandleEvent else
   if XBool(XCheckTypedEvent(XDisplay, ClientMessage, @event)) <> XBool_False then
    HandleEvent else
    result := false;
   {$endif}
  end;

  procedure WaitMessage;
  begin
   {$ifdef GLWINDOW_WINAPI}
   Check( LongInt(GetMessage(msg, 0, 0, 0)) <> -1, 'GetMessage failed with result -1.');
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

  function SomeWindowDisallowsSuspend: boolean;
  { czy istnieje jakies okienko ktore cos robi w DoIdle lub DoTimer ?  }
  var i: integer;
  begin
   for i := 0 to ActiveCount-1 do
    if not Active[i].AllowSuspendForInput then
    begin
     result := true; exit
    end;
   result := false;
  end;

var wasAnyDrawing: boolean;
    i: integer;
    glwin: TGLWindow;
begin
 WasAnyMessage := false;
 try
  if ActiveCount <> 0 then
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
         jezeli not wasAnyDrawing to z kolei zdarzenia OnDraw
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
        i OnDraw. A Close usuwa to okno z listy Active[], dlatego kopiujemy sobie
        na poczatku Active[i] do glwin. }
    wasAnyDrawing := false;
    i := 0;
    while i < ActiveCount do
    begin
     glwin := Active[i];
     glwin.DoIdle;
     if glwin.Closed then Continue {don't Inc(i)};
     { Kolejnosc w OR ponizej MA ZNACZENIE : zawsze chemy wywolac
       FlushRedisplay2, nawet jesli wasAnyDrawing juz jest = true }
     wasAnyDrawing := glwin.FlushRedisplay2 or wasAnyDrawing;
     if glwin.Closed then Continue {don't Inc(i)};
     Inc(i);
    end;

    { If we do not do anything in any idle/timer
      (this is checked by SomeWindowDisallowsSuspend,
      and additionally the Glwm callbacks OnIdle and OnTimer are checked
      here explicitly), and there's no reason to redisplay and we're
      not during quit process (started by Glwm.Quit),
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
    if AllowSuspend and not (Assigned(OnIdle) or Assigned(OnTimer) or
       SomeWindowDisallowsSuspend or wasAnyDrawing or QuitPosted) then
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

procedure TGLWindowsManager.Loop;
begin
  if ActiveCount = 0 then Exit;

  QuitPosted := false;
  while ProcessMessage(true) do ;
end;

procedure TGLWindowsManager.QuitWhenNoWindowsActive;
begin
 QuitPosted := true;
end;

{$endif read_implementation}