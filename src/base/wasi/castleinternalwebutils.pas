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
  of the class that has registered callbacks by @link(Then) is freed),
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
  multiple callbacks for the same promise, then are appended.
}
procedure RegisterPromiseCallbacks(const JsPromise: IJSPromise;
  const OnAccepted, OnRejected: TJSPromiseResolver);

{ After unregistering, the callbacks registered by @link(RegisterPromiseCallbacks)
  won't be called anymore. }
procedure UnregisterPromiseCallbacks(const JsPromise: IJSPromise);

{ If JsPromise assigned, unregister the callbacks and set it to nil. }
procedure UnregisterPromiseAndNil(var JsPromise: IJSPromise);

implementation

uses Generics.Collections;

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

finalization
  { Not freeing the Promises, in case some promise will be fullfilled
    after the finalization of this unit. }
  // FreeAndNil(Promises);
end.