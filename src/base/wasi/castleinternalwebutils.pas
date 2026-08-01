{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ WebAssembly utilities. }
unit CastleInternalWebUtils;

{$I castleconf.inc}

{$ifndef WASI}
  {$message fatal 'This unit is only for WASI (WebAssembly) target.'}
{$endif}

interface

uses Job.Js, CastleInternalJobWeb;

{ Register callbacks for success/failure of JavaScript promise,
  in a way that can be canceled (by doing @link(UnregisterPromiseCallbacks)).

  This allows to use JavaScript promises safely: when the caller
  is no longer interested in the result (in particular, if the instance
  of the class that has registered callbacks by @code(JsPromise._Then) is freed),
  you can just free this instance, and the callbacks won't be called anymore.

  It would be nice if JavaScript promises had a built-in cancellation mechanism,
  but they don't ( https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise
  -- """Promise itself has no first-class protocol for cancellation...""" ).
  See posts like
  https://stackoverflow.com/questions/30233302/promise-is-it-possible-to-force-cancel-a-promise
  confirming that it sucks and JavaScript just sucks.
  In case of using promises from garbage-collected languages, at least
  you will not get crashes, you just may be notified about a promise
  result in a state that is no longer relevant (e.g. decoding audio
  buffer finished in a view that is no longer interested in playing it).
  In a non-garbage-collected language like Object Pascal, we may get
  crashes, e.g. if TWebAudioSoundBufferBackend would naively use promises,
  and then user frees TCastleSound before OggVorbis decoding finishes
  -> promise success callback could be called on a freed TWebAudioSoundBufferBackend
  instance.

  Calling this multiple times @italic(replaces) previous callbacks for the same promise.
  This is in contrast to JavaScript's Promise._Then, which allows to register
  multiple callbacks for the same promise, that are appended.
}
procedure RegisterPromiseCallbacks(const JsPromise: IJSPromise;
  const OnAccepted, OnRejected: TJSPromiseResolver);

{ After unregistering, the callbacks registered by @link(RegisterPromiseCallbacks)
  won't be called anymore. }
procedure UnregisterPromiseCallbacks(const JsPromise: IJSPromise);

{ If JsPromise assigned, unregister the callbacks and set it to nil. }
procedure UnregisterPromiseAndNil(var JsPromise: IJSPromise);

{ Add event listener to a JavaScript element,
  in a way that can be removed later (by doing @link(RemoveEventListener)).

  This workarounds FPC Job.Js issue, that IJSEventTarget.RemoveEventListener
  method (from CastleInternalJobWeb, generated using Job.Js approach)
  doesn't work, it doesn't unregister the listener (likely because it is actually
  a different thing (wrapper around Pascal call) passed to JS removeEventListener
  than the one that was passed to JS addEventListener?).

  So we never really unregister anything from JavaScript. Instead we register
  in JavaScript our own proxy, that calls the Pascal @code(Callback).
  The @link(RemoveEventListener) just makes this proxy do nothing.
  Consequences:

  @unorderedList(
    @item(After @link(RemoveEventListener), the @code(Callback) is really
      not called anymore. Which is the point of all this: it is safe to free
      the instance that owns the @code(Callback) method afterwards.)

    @item(The proxy registered in JavaScript stays registered, and is never
      freed. It is reused by the next @link(AddEventListener) with the same
      @code(Element) and @code(EventName), so a repeated
      "add, remove, add, remove..." (e.g. reopening TCastleWindow)
      doesn't accumulate JavaScript listeners.)
  )

  Adding the same @code(Callback) for the same @code(Element) and
  @code(EventName) twice does nothing the second time, just like
  JavaScript addEventListener that ignores duplicates.

  @code(Element) instances are compared by the Pascal interface identity,
  so pass the same Pascal interface instance (not merely two interfaces
  wrapping the same JavaScript object) to @link(AddEventListener) and
  @link(RemoveEventListener). This is naturally satisfied when the element
  is kept in a field, like @code(TCastleWindow.Canvas). }
procedure AddEventListener(const Element: IJSEventTarget; const EventName: String;
  const Callback: TEventListener);

{ Remove listener that was added by @link(AddEventListener).
  Does nothing if such listener was not added. }
procedure RemoveEventListener(const Element: IJSEventTarget; const EventName: String;
  const Callback: TEventListener);

{ Like @link(AddEventListener), but for a JavaScript object that you have
  as a Job.Js class instance, not as an interface.
  Use this for the Job.Js globals @code(JSDocument), @code(JSWindow).

  Never pass such class instance to @link(AddEventListener) instead.
  It would make a temporary interface reference,
  increasing then decreasing the reference count,
  and thus freeing the global object. }
procedure AddEventListenerObject(const Element: TJSEventTarget;
  const EventName: String; const Callback: TEventListener);

{ Remove listener that was added by @link(AddEventListenerObject).
  Does nothing if such listener was not added. }
procedure RemoveEventListenerObject(const Element: TJSEventTarget;
  const EventName: String; const Callback: TEventListener);

implementation

uses SysUtils, Generics.Collections;

type
  { Call OnAccepted / OnRejected callbacks when a promise is accepted / rejected. }
  TPromiseProxy = class
    OnAccepted, OnRejected: TJSPromiseResolver;
    Promise: IJSPromise;
    function Accepted(const aValue: Variant): Variant;
    function Rejected(const aValue: Variant): Variant;
  end;

  { Track all registered promises and their callbacks. }
  TPromises = class({$ifdef FPC}specialize{$endif} TObjectDictionary<IJSPromise, TPromiseProxy>)
  end;

var
  Promises: TPromises;

function TPromiseProxy.Accepted(const aValue: Variant): Variant;
begin
  if Assigned(OnAccepted) then
    Result := OnAccepted(aValue)
  else
    Result := aValue;

  // promise is accepted, we won't get any more calls to this TPromiseProxy,
  // so we can remove it from the dictionary to free memory.
  Promises.Remove(Promise);
end;

function TPromiseProxy.Rejected(const aValue: Variant): Variant;
begin
  if Assigned(OnRejected) then
    Result := OnRejected(aValue)
  else
    Result := aValue;

  // promise is rejected, we won't get any more calls to this TPromiseProxy,
  // so we can remove it from the dictionary to free memory.
  Promises.Remove(Promise);
end;

procedure RegisterPromiseCallbacks(const JsPromise: IJSPromise;
  const OnAccepted, OnRejected: TJSPromiseResolver);
var
  Proxy: TPromiseProxy;
begin
  if not Assigned(Promises) then
    Promises := TPromises.Create([doOwnsValues]);
  if Promises.TryGetValue(JsPromise, Proxy) then
  begin
    Proxy.OnAccepted := OnAccepted;
    Proxy.OnRejected := OnRejected;
  end else
  begin
    Proxy := TPromiseProxy.Create;
    Proxy.OnAccepted := OnAccepted;
    Proxy.OnRejected := OnRejected;
    Proxy.Promise := JsPromise;
    Promises.Add(JsPromise, Proxy);
    JsPromise._Then(
      @Proxy.Accepted,
      @Proxy.Rejected
    );
  end;
end;

procedure UnregisterPromiseCallbacks(const JsPromise: IJSPromise);
var
  Proxy: TPromiseProxy;
begin
  if not Assigned(Promises) then
    Exit;
  if Promises.TryGetValue(JsPromise, Proxy) then
  begin
    { If the JsPromise was already registered, we *do not* remove it from
      the dictionary.
      We keep it, so that it can safely call it's TPromiseProxy.Accepted / Rejected
      methods and they will do nothing.

      Note that even if we could direct every promise to a single
      callback on a single instance (that is, if JS promises would send
      something like Sender to allow to distinguish calling promise),
      we would still need to keep the TPromiseProxy instance in the dictionary,
      to avoid calling the "JsPromise._Then(...)" again on it.

      Since we cannot cancel the JavaScript promise itself, and we cannot clear
      it's callbacks, we need to be capable of handling it forever...
      We just set the callbacks to nil. }

    Proxy.OnAccepted := nil;
    Proxy.OnRejected := nil;
  end;
end;

procedure UnregisterPromiseAndNil(var JsPromise: IJSPromise);
begin
  if JsPromise <> nil then
  begin
    UnregisterPromiseCallbacks(JsPromise);
    JsPromise := nil;
  end;
end;

{ Event listeners ------------------------------------------------------------ }

type
  { Call the Pascal Callback when a JavaScript event occurs.

    One instance of this class is registered using JavaScript
    addEventListener by one AddEventListener call here. Since we cannot unregister
    it from JavaScript (see AddEventListener docs), this instance must be valid
    forever: all instances are kept on the ListenerProxies list and never freed. }
  TEventListenerProxy = class
    { JavaScript object for which we are registered.
      Exactly one of these is assigned: ElementIntf (when registered by
      AddEventListener) or ElementObject (when registered by
      AddEventListenerObject). }
    ElementIntf: IJSEventTarget;
    ElementObject: TJSEventTarget;

    { Event name for which we are registered. }
    EventName: String;

    { Pascal callback to call when the event occurs.
      Set to nil if RemoveEventListener was called: then we are still registered
      (as far as JavaScript is concerned),
      but we do nothing, and we can even be reused by the next
      AddEventListener with the same element and EventName. }
    Callback: TEventListener;

    function HandleEvent(Event: IJSEvent): Boolean;
  end;

  { Track all listeners registered by AddEventListener. }
  TEventListenerProxyList = {$ifdef FPC}specialize{$endif} TObjectList<TEventListenerProxy>;

var
  ListenerProxies: TEventListenerProxyList;

function TEventListenerProxy.HandleEvent(Event: IJSEvent): Boolean;
begin
  { When Callback is nil, it means that RemoveEventListener was called.
    In this case we do nothing, and return false (to indicate that the event
    was not handled). }
  Result := Assigned(Callback) and Callback(Event);
end;

{ Find the proxy registered for given element (given as ElementIntf or
  ElementObject, the other one must be @nil) and EventName, with given Callback.
  Callback may be = nil, to find a proxy that is registered in JavaScript
  but currently does nothing (so it can be reused).
  Returns @nil if there is no such proxy. }
function FindListenerProxy(const ElementIntf: IJSEventTarget;
  const ElementObject: TJSEventTarget; const EventName: String;
  const Callback: TEventListener): TEventListenerProxy;
var
  I: Integer;
begin
  if Assigned(ListenerProxies) then
    for I := 0 to ListenerProxies.Count - 1 do
    begin
      Result := ListenerProxies[I];
      if (Result.ElementIntf = ElementIntf) and
         (Result.ElementObject = ElementObject) and
         (Result.EventName = EventName) and
         SameMethods(TMethod(Result.Callback), TMethod(Callback)) then
        Exit;
    end;
  Result := nil;
end;

{ Common implementation of AddEventListener and AddEventListenerObject.
  Pass the element either as ElementIntf or as ElementObject, the other one
  must be @nil. }
procedure AddListenerCore(const ElementIntf: IJSEventTarget;
  const ElementObject: TJSEventTarget; const EventName: String;
  const Callback: TEventListener);
var
  Proxy: TEventListenerProxy;
begin
  if not Assigned(Callback) then
    raise Exception.CreateFmt('AddEventListener(%s): Callback must be assigned', [
      EventName
    ]);

  { Ignore duplicates, just like JavaScript addEventListener. }
  if FindListenerProxy(ElementIntf, ElementObject, EventName, Callback) <> nil then
    Exit;

  { Reuse a proxy that is already registered in JavaScript for this element
    and EventName, but currently does nothing. }
  Proxy := FindListenerProxy(ElementIntf, ElementObject, EventName, nil);
  if Proxy <> nil then
  begin
    Proxy.Callback := Callback;
    Exit;
  end;

  if not Assigned(ListenerProxies) then
    ListenerProxies := TEventListenerProxyList.Create(true);
  Proxy := TEventListenerProxy.Create;
  Proxy.ElementIntf := ElementIntf;
  Proxy.ElementObject := ElementObject;
  Proxy.EventName := EventName;
  Proxy.Callback := Callback;
  ListenerProxies.Add(Proxy);

  if ElementObject <> nil then
    ElementObject.addEventListener(EventName, @Proxy.HandleEvent)
  else
    ElementIntf.addEventListener(EventName, @Proxy.HandleEvent);
end;

{ Common implementation of RemoveEventListener and RemoveEventListenerObject.
  Pass the element either as ElementIntf or as ElementObject, the other one
  must be @nil. }
procedure RemoveListenerCore(const ElementIntf: IJSEventTarget;
  const ElementObject: TJSEventTarget; const EventName: String;
  const Callback: TEventListener);
var
  Proxy: TEventListenerProxy;
begin
  if not Assigned(Callback) then
    raise Exception.CreateFmt('RemoveEventListener(%s): Callback must be assigned', [
      EventName
    ]);

  Proxy := FindListenerProxy(ElementIntf, ElementObject, EventName, Callback);
  { Just like JavaScript removeEventListener, do nothing if this listener
    was not registered. }
  if Proxy <> nil then
    { We cannot unregister the proxy from JavaScript, and thus we cannot free it.
      Just make it do nothing, and allow the next AddEventListener to reuse it. }
    Proxy.Callback := nil;
end;

procedure AddEventListener(const Element: IJSEventTarget; const EventName: String;
  const Callback: TEventListener);
begin
  AddListenerCore(Element, nil, EventName, Callback);
end;

procedure RemoveEventListener(const Element: IJSEventTarget; const EventName: String;
  const Callback: TEventListener);
begin
  RemoveListenerCore(Element, nil, EventName, Callback);
end;

procedure AddEventListenerObject(const Element: TJSEventTarget;
  const EventName: String; const Callback: TEventListener);
begin
  AddListenerCore(nil, Element, EventName, Callback);
end;

procedure RemoveEventListenerObject(const Element: TJSEventTarget;
  const EventName: String; const Callback: TEventListener);
begin
  RemoveListenerCore(nil, Element, EventName, Callback);
end;

finalization
  { Not freeing the Promises, in case some promise will be fullfilled
    after the finalization of this unit.
    Same for ListenerProxies: JavaScript may still call the listeners
    (that do nothing now) after the finalization of this unit. }
  // FreeAndNil(Promises);
  // FreeAndNil(ListenerProxies);
end.