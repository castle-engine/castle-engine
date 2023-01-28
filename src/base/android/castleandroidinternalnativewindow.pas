(*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
  *)

{ @exclude Internal for the engine. }
unit CastleAndroidInternalNativeWindow;

{$I castleconf.inc}

interface

uses ctypes, CastleAndroidInternalRect;

(*
 * Pixel formats that a window can use.
  *)
const
  WINDOW_FORMAT_RGBA_8888 = 1;
  WINDOW_FORMAT_RGBX_8888 = 2;
  WINDOW_FORMAT_RGB_565 = 4;

type
  PANativeWindow = ^ANativeWindow;
  ANativeWindow = record end;

  PANativeWindow_Buffer = ^ANativeWindow_Buffer;
  ANativeWindow_Buffer = packed record
    // The number of pixels that are show horizontally.
    width : cint32;
    // The number of pixels that are shown vertically.
    height : cint32;
    // The number of *pixels* that a line in the buffer takes in
    // memory.  This may be >= width.
    stride : cint32;
    // The format of the buffer.  One of WINDOW_FORMAT_*
    format : cint32;
    // The actual bits.
    bits : Pointer;
    // Do not touch.
    reserved : array [0..5] of cuint32;
  end;

(**
 * Acquire a reference on the given ANativeWindow object.  This prevents the object
 * from being deleted until the reference is removed.
  *)
procedure ANativeWindow_acquire(window: PANativeWindow); cdecl; external;

(**
 * Remove a reference that was previously acquired with ANativeWindow_acquire().
  *)
procedure ANativeWindow_release(window: PANativeWindow); cdecl; external;

(*
 * Return the current width in pixels of the window surface.  Returns a
 * negative value on error.
  *)
function ANativeWindow_getWidth(window: PANativeWindow): cint32; cdecl; external;

(*
 * Return the current height in pixels of the window surface.  Returns a
 * negative value on error.
  *)
function ANativeWindow_getHeight(window: PANativeWindow): cint32; cdecl; external;

(*
 * Return the current pixel format of the window surface.  Returns a
 * negative value on error.
  *)
function ANativeWindow_getFormat(window: PANativeWindow): cint32; cdecl; external;

(*
 * Change the format and size of the window buffers.
 *
 * The width and height control the number of pixels in the buffers, not the
 * dimensions of the window on screen.  If these are different than the
 * window's physical size, then it buffer will be scaled to match that size
 * when compositing it to the screen.
 *
 * For all of these parameters, if 0 is supplied then the window's base
 * value will come back in force.
  *)
function ANativeWindow_setBuffersGeometry(window: PANativeWindow; width, height, format: cint32): cint32; cdecl; external;

(**
 * Lock the window's next drawing surface for writing.
  *)
function ANativeWindow_lock(window: PANativeWindow; outBuffer: PANativeWindow_Buffer; inOutDirtyBounds: PARect): cint32; cdecl; external;

(**
 * Unlock the window's drawing surface after previously locking it,
 * posting the new buffer to the display.
  *)
function ANativeWindow_unlockAndPost(window: PANativeWindow): cint32; cdecl; external;

implementation

end.

