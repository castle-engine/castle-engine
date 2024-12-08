/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#media-elements
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window,
 InstrumentedProps=(disableRemotePlayback,remote)]
interface HTMLMediaElement : HTMLElement {

  // network state
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString src;
  readonly attribute DOMString currentSrc;

  [CEReactions, SetterThrows]
           attribute DOMString? crossOrigin;
  const unsigned short NETWORK_EMPTY = 0;
  const unsigned short NETWORK_IDLE = 1;
  const unsigned short NETWORK_LOADING = 2;
  const unsigned short NETWORK_NO_SOURCE = 3;
  readonly attribute unsigned short networkState;
  [CEReactions, SetterThrows]
           attribute DOMString preload;
};
