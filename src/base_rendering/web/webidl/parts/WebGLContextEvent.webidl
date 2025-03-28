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

 * The origin of this IDL file is
 * https://www.khronos.org/registry/webgl/specs/latest/1.0/#fire-a-webgl-context-event
 */

[Exposed=(Window,Worker)]
interface WebGLContextEvent : Event {
  constructor(DOMString type, optional WebGLContextEventInit eventInit = {});

  readonly attribute DOMString statusMessage;
};

// EventInit is defined in the DOM4 specification.
dictionary WebGLContextEventInit : EventInit {
  DOMString statusMessage = "";
};
