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
 * http://www.whatwg.org/specs/web-apps/current-work/multipage/the-canvas-element.html#imagedata
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and Opera Software ASA.
 * You are granted a license to use, reproduce and create derivative works of this document.
 */

[Exposed=(Window,Worker),
 Serializable,
 InstrumentedProps=(colorSpace)]
interface ImageData {
 [Throws]
 constructor(unsigned long sw, unsigned long sh);
 [Throws]
 constructor(Uint8ClampedArray data, unsigned long sw,
             optional unsigned long sh);

 [Constant]
 readonly attribute unsigned long width;
 [Constant]
 readonly attribute unsigned long height;
 [Constant, StoreInSlot]
 readonly attribute Uint8ClampedArray data;
};
