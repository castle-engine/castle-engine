/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information on this interface, please see
 * https://html.spec.whatwg.org/#the-offscreencanvas-interface
 */

typedef (OffscreenCanvasRenderingContext2D or ImageBitmapRenderingContext or WebGLRenderingContext or WebGL2RenderingContext or GPUCanvasContext) OffscreenRenderingContext;

dictionary ImageEncodeOptions {
  DOMString type = "image/png";
  unrestricted double quality;
};

enum OffscreenRenderingContextId { "2d", "bitmaprenderer", "webgl", "webgl2", "webgpu" };

[Exposed=(Window,Worker)]
interface OffscreenCanvas : EventTarget {
  [Throws]
  constructor([EnforceRange] unsigned long width, [EnforceRange] unsigned long height);

  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long width;
  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long height;

  [Throws]
  OffscreenRenderingContext? getContext(OffscreenRenderingContextId contextId,
                                        optional any contextOptions = null);

  [Throws]
  ImageBitmap transferToImageBitmap();

  attribute EventHandler oncontextlost;
  attribute EventHandler oncontextrestored;
};
