{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ This is a Pascal wrapper for Xxf86vm library.
  This library allows changing screen resolution under X11.

  Note that libXxf86vm is usually (at least on my Debian testing)
  distributed only as static library (".a" instead of ".so").
  So @code($linklib Xxf86vm) usually links you to static library.
  (this is of course no reason to worry, I just thought I may say it here...)

  Original C header is in @code(X11/include/extensions/xf86vmode.h) .
  Translation done by Kambi by hand and using some regexps.
  Primitive types translation:

  @preformatted(
    short = SmallInt;
    unsigned short = Word;
    int = long = LongInt;
    unsigned int = unsigned long = LongWord;
    float = Single;
    double = Double;
    Bool = LongBool;
  )

  @preformatted(
    ------------------------------------------------------------------------

    $Xorg: xf86vmode.h,v 1.3 2000/08/18 04:05:46 coskrey Exp $
    $XFree86: xc/include/extensions/xf86vmode.h,v 3.29 2001/05/06 00:47:35 mvojkovi Exp $

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
    IN NO EVENT SHALL Kaleb S. KEITHLEY BE LIABLE FOR ANY CLAIM, DAMAGES
    OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
    ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
    OTHER DEALINGS IN THE SOFTWARE.

    THIS IS NOT AN X CONSORTIUM STANDARD OR AN X PROJECT TEAM SPECIFICATION
  )

  @exclude (This is only a C header translation --- no nice PasDoc docs.)
}

unit CastleXF86VMode;

{$I castleconf.inc}

{$ifdef VER1_0_10}
  {$FATAL This unit cannot be safely
    used with FPC 1.0.10 due to some bugs in compiler (that were not present
    in FPC 1.0.6) -- VideoReset in CastleWindow after --fullscreen-custom causes
    AccessViolation. }
{$endif}

interface

uses Xlib, X;

{$linklib Xxf86vm}
{$define vmdecl := cdecl; external}

const
  { }
  X_XF86VidModeQueryVersion      = 0;
  X_XF86VidModeGetModeLine       = 1;
  X_XF86VidModeModModeLine       = 2;
  X_XF86VidModeSwitchMode        = 3;
  X_XF86VidModeGetMonitor        = 4;
  X_XF86VidModeLockModeSwitch    = 5;
  X_XF86VidModeGetAllModeLines   = 6;
  X_XF86VidModeAddModeLine       = 7;
  X_XF86VidModeDeleteModeLine    = 8;
  X_XF86VidModeValidateModeLine  = 9;
  X_XF86VidModeSwitchToMode      = 10;
  X_XF86VidModeGetViewPort       = 11;
  X_XF86VidModeSetViewPort       = 12;
{* new for version 2.x of this extension *}
  X_XF86VidModeGetDotClocks      = 13;
  X_XF86VidModeSetClientVersion  = 14;
  X_XF86VidModeSetGamma          = 15;
  X_XF86VidModeGetGamma          = 16;
  X_XF86VidModeGetGammaRamp      = 17;
  X_XF86VidModeSetGammaRamp      = 18;
  X_XF86VidModeGetGammaRampSize  = 19;

  CLKFLAG_PROGRAMABLE    = 1;

{$ifdef XF86VIDMODE_EVENTS}
  XF86VidModeNotify      = 0;
  XF86VidModeNumberEvents        = XF86VidModeNotify + 1;
  XF86VidModeNotifyMask          = 00000001;

  XF86VidModeNonEvent    = 0;
  XF86VidModeModeChange  = 1;
{$else}
  XF86VidModeNumberEvents        = 0;
{$endif}

  XF86VidModeBadClock            = 0;
  XF86VidModeBadHTimings         = 1;
  XF86VidModeBadVTimings         = 2;
  XF86VidModeModeUnsuitable      = 3;
  XF86VidModeExtensionDisabled   = 4;
  XF86VidModeClientNotLocal      = 5;
  XF86VidModeZoomLocked          = 6;
  XF86VidModeNumberErrors        = XF86VidModeZoomLocked + 1;

type
  TXF86VidModeModeLine = record
    hdisplay: SmallInt;
    hsyncstart: SmallInt;
    hsyncend: SmallInt;
    htotal: SmallInt;
    hskew: SmallInt;
    vdisplay: SmallInt;
    vsyncstart: SmallInt;
    vsyncend: SmallInt;
    vtotal: SmallInt;
    flags: LongWord;
    privsize: LongInt;
    { private is a C++ reserved word }
    c_private: PLongInt;
  end;
  PXF86VidModeModeLine = ^TXF86VidModeModeLine;

  TXF86VidModeModeInfo = record
    dotclock: LongWord;
    hdisplay: SmallInt;
    hsyncstart: SmallInt;
    hsyncend: SmallInt;
    htotal: SmallInt;
    hskew: SmallInt;
    vdisplay: SmallInt;
    vsyncstart: SmallInt;
    vsyncend: SmallInt;
    vtotal: SmallInt;
    flags: LongWord;
    privsize: LongInt;
    c_private: PLongInt;
  end;
    PXF86VidModeModeInfo =  ^TXF86VidModeModeInfo;
   { PPXF86VidModeModeInfo can be also treated as PArray_PXF86VidModeModeInfo }
   PPXF86VidModeModeInfo =  ^PXF86VidModeModeInfo;
  PPPXF86VidModeModeInfo = ^PPXF86VidModeModeInfo;

  TArray_PXF86VidModeModeInfo = array[0..High(Word)] of PXF86VidModeModeInfo;
  PArray_PXF86VidModeModeInfo = ^TArray_PXF86VidModeModeInfo;

  TXF86VidModeSyncRange = record
    hi: Single;
    lo: Single;
  end;
  PXF86VidModeSyncRange = ^TXF86VidModeSyncRange;

  TXF86VidModeMonitor = record
    vendor: PChar;
    model: PChar;
    EMPTY: Single;
    nhsync: byte;
    hsync: PXF86VidModeSyncRange;
    nvsync: byte;
    vsync: PXF86VidModeSyncRange;
  end;
  PXF86VidModeMonitor = ^TXF86VidModeMonitor;

  TXF86VidModeNotifyEvent = record
    c_type: LongInt;                    {< of event }
    serial: LongInt;            {< # of last request processed by server }
    send_event: LongBool;               {< true if this came from a SendEvent req }
    display: PDisplay;          {< Display the event was read from }
    root: TWindow;              {< root window of event screen }
    state: LongInt;                     {< What happened }
    kind: LongInt;                      {< What happened }
    forced: LongBool;           {< extents of new region }
    time: TTime;                        {< event timestamp }
  end;

  TXF86VidModeGamma = record
    red: Single;                        {< Red Gamma value }
    green: Single;              {< Green Gamma value }
    blue: Single;                       {< Blue Gamma value }
  end;
  PXF86VidModeGamma = ^TXF86VidModeGamma;

{ Macros: }

{ }
function XF86VidModeSelectNextMode(dpy: PDisplay; screen: LongInt): LongBool;
function XF86VidModeSelectPrevMode(dpy: PDisplay; screen: LongInt): LongBool;

function XF86VidModeQueryVersion(
  dpy: PDisplay;
  majorVersion: PLongInt;
  minorVersion: PLongInt
): LongBool; vmdecl;

function XF86VidModeQueryExtension(
  dpy: PDisplay;
  event_base: PLongInt;
  error_base: PLongInt
): LongBool; vmdecl;

function XF86VidModeSetClientVersion(
  dpy: PDisplay
): LongBool; vmdecl;

function XF86VidModeGetModeLine(
  dpy: PDisplay;
  screen: LongInt;
  dotclock: PLongInt;
  modeline: PXF86VidModeModeLine
): LongBool; vmdecl;

function XF86VidModeGetAllModeLines(
  dpy: PDisplay;
  screen: LongInt;
  modecount: PLongInt;
  modelinesPtr: PPPXF86VidModeModeInfo
): LongBool; vmdecl;

function XF86VidModeAddModeLine(
  dpy: PDisplay;
  screen: LongInt;
  new_modeline: PXF86VidModeModeInfo;
  after_modeline: PXF86VidModeModeInfo
): LongBool; vmdecl;

function XF86VidModeDeleteModeLine(
  dpy: PDisplay;
  screen: LongInt;
  modeline: PXF86VidModeModeInfo
): LongBool; vmdecl;

function XF86VidModeModModeLine(
  dpy: PDisplay;
  screen: LongInt;
  modeline: PXF86VidModeModeLine
): LongBool; vmdecl;

function XF86VidModeValidateModeLine(
  dpy: PDisplay;
  screen: LongInt;
  modeline: PXF86VidModeModeInfo
): TStatus; vmdecl;

function XF86VidModeSwitchMode(
  dpy: PDisplay;
  screen: LongInt;
  zoom: LongInt
): LongBool; vmdecl;

function XF86VidModeSwitchToMode(
  dpy: PDisplay;
  screen: LongInt;
  modeline: PXF86VidModeModeInfo
): LongBool; vmdecl;

function XF86VidModeLockModeSwitch(
  dpy: PDisplay;
  screen: LongInt;
  lock: LongInt
): LongBool; vmdecl;

function XF86VidModeGetMonitor(
  dpy: PDisplay;
  screen: LongInt;
  monitor: PXF86VidModeMonitor
): LongBool; vmdecl;

function XF86VidModeGetViewPort(
  dpy: PDisplay;
  screen: LongInt;
  x_return: PLongInt;
  y_return: PLongInt
): LongBool; vmdecl;

function XF86VidModeSetViewPort(
  dpy: PDisplay;
  screen: LongInt;
  x: LongInt;
  y: LongInt
): LongBool; vmdecl;

function XF86VidModeGetDotClocks(
  dpy: PDisplay;
  screen: LongInt;
  flags_return: PLongInt;
  number_of_clocks_return: PLongInt;
  max_dot_clock_return: PLongInt;
  clocks_return: PPLongInt
): LongBool; vmdecl;

function XF86VidModeGetGamma(
  dpy: PDisplay;
  screen: LongInt;
  Gamma: PXF86VidModeGamma
): LongBool; vmdecl;

function XF86VidModeSetGamma(
  dpy: PDisplay;
  screen: LongInt;
  Gamma: PXF86VidModeGamma
): LongBool; vmdecl;

function XF86VidModeSetGammaRamp(
  dpy: PDisplay;
  screen: LongInt;
  size: LongInt;
  red_array: PWord;
  green_array: PWord;
  blue_array: PWord
): LongBool; vmdecl;

function XF86VidModeGetGammaRamp(
  dpy: PDisplay;
  screen: LongInt;
  size: LongInt;
  red_array: PWord;
  green_array: PWord;
  blue_array: PWord
): LongBool; vmdecl;

function XF86VidModeGetGammaRampSize(
  dpy: PDisplay;
  screen: LongInt;
  size: PLongInt
): LongBool; vmdecl;

implementation

function XF86VidModeSelectNextMode(dpy: PDisplay; screen: LongInt): LongBool;
begin
 Result := XF86VidModeSwitchMode(dpy, screen, 1);
end;

function XF86VidModeSelectPrevMode(dpy: PDisplay; screen: LongInt): LongBool;
begin
 Result := XF86VidModeSwitchMode(dpy, screen, -1);
end;

end.
