{
  Copyright 2001-2004 Michalis Kamburelis.

  This file is part of "Kambi's OpenGL Pascal units".

  "Kambi's OpenGL Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's OpenGL Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's OpenGL Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Translation of glut.h header (vesrion 3.7 beta) to Pascal.)

  Compatible with Delphi, Kylix, FreePascal on Win32 and
  FreePascal on Linux.

  Stosowalem dyrektywy $IF ale przestalem - teraz uzywam tylko
  $IFDEF, wiec jest kompatybilnosc z FPC (i przy okazji ze
  starszymi Delphi).

  Dlaczego procedury / funkcje gluta sa ladowane w initialization
  modulu zamiast robic je przez external ? Patrz openGLh,
  komentarz przy glXMakeCurrent.

  Wieksze wlasne dodatki / konieczne zmiany oznaczylem przez 'Kambi'
  (wlasciwie to jest tylko jeden moj dodatek uzyteczny z zewnatrz :
  procedura glutPascalInit).

  oryginal : Copyright (c) Mark J. Kilgard, 1994, 1995, 1996, 1998.

  Dolaczenie tego modulu do programu powoduje ze user musi miec
  zainstalowanego gluta (pod UNIXem - libglut.so, pod win32 - glut32.dll).
}

unit KambiGlut;

(*
  Type translation :
    unsigned int = LongWord
    int = integer
    const char * = PChar ("const PChar" is nonsense and it's NOT the meaning of "const char *")
    void * = pointer

  C constant macros :  #define NAME VALUE
  regexp : ^#define \([^[:blank:]]*\)[[:blank:]]*\([^[:blank:]]*\)$
  replaceWith : \1=\2;

  C comments :  /* .. */
  regexp : /\*\([^*]*\)\*/
  replaceWith : {\1}

  C function declarations :
  regexp : extern \([^[:blank:]]*\) APIENTRY \([^;]*\);
  replaceWith : function \2:\1;

  C #if directives :
  regexp : ^#if (\(.*\)$
  replaceWith : {$IF \1}
  (note - now I don't use $IF compiler directives because
   they don't exist in FPC).
*)

{$Include KambiConf.inc}

{ Choose calling convention for glut functions. }
{$ifdef UNIX}  {$define GLUT_CDECL}   {$endif}
{$ifdef WIN32} {$define GLUT_STDCALL} {$endif}

interface

uses
  {$ifdef WIN32} Windows, {$else} {$endif} OpenGLh, SysUtils;

const
  GlutDLL =
    {$ifdef UNIX}
      {$ifdef DARWIN} 'libglut.dylib' { TODO--confirm this works under Darwin }
      {$else} 'libglut.so'
      {$endif}
    {$endif}
    {$ifdef WIN32} 'glut32.dll' {$endif};

{
  GLUT API revision history:

  GLUT_API_VERSION is updated to reflect incompatible GLUT
  API changes (interface changes, semantic changes, deletions,
  or additions).

  GLUT_API_VERSION=1  First public release of GLUT.  11/29/94

  GLUT_API_VERSION=2  Added support for OpenGL/GLX multisampling,
  extension.  Supports new input devices like tablet, dial and button
  box, and Spaceball.  Easy to query OpenGL extensions.

  GLUT_API_VERSION=3  glutMenuStatus added.

  GLUT_API_VERSION=4  glutInitDisplayString, glutWarpPointer,
  glutBitmapLength, glutStrokeLength, glutWindowStatusFunc, dynamic
  video resize subAPI, glutPostWindowRedisplay, glutKeyboardUpFunc,
  glutSpecialUpFunc, glutIgnoreKeyRepeat, glutSetKeyRepeat,
  glutJoystickFunc, glutForceJoystickFunc (NOT FINALIZED!).
}

{ Kambi: in glut.h GLUT_API_VERSION i GLUT_XLIB_IMPLEMENTATION were
  only preprocessor symbols. For us (for the $IF directive) they must
  be Pascal constans.

  Note - I don't use the $IF directive now because FPC doesn't implement
  it. So I don't use _GLUT_API_VERSION and _GLUT_XLIB_IMPLEMENTATION. }

const _GLUT_API_VERSION = 3;

{
  GLUT implementation revision history:

  GLUT_XLIB_IMPLEMENTATION is updated to reflect both GLUT
  API revisions and implementation revisions (ie, bug fixes).

  GLUT_XLIB_IMPLEMENTATION=1  mjk's first public release of
  GLUT Xlib-based implementation.  11/29/94

  GLUT_XLIB_IMPLEMENTATION=2  mjk's second public release of
  GLUT Xlib-based implementation providing GLUT version 2
  interfaces.

  GLUT_XLIB_IMPLEMENTATION=3  mjk's GLUT 2.2 images. 4/17/95

  GLUT_XLIB_IMPLEMENTATION=4  mjk's GLUT 2.3 images. 6/?/95

  GLUT_XLIB_IMPLEMENTATION=5  mjk's GLUT 3.0 images. 10/?/95

  GLUT_XLIB_IMPLEMENTATION=7  mjk's GLUT 3.1+ with glutWarpPoitner.  7/24/96

  GLUT_XLIB_IMPLEMENTATION=8  mjk's GLUT 3.1+ with glutWarpPoitner
  and video resize.  1/3/97

  GLUT_XLIB_IMPLEMENTATION=9 mjk's GLUT 3.4 release with early GLUT 4 routines.

  GLUT_XLIB_IMPLEMENTATION=11 Mesa 2.5's GLUT 3.6 release.

  GLUT_XLIB_IMPLEMENTATION=12 mjk's GLUT 3.6 release with early GLUT 4 routines + signal handling.

  GLUT_XLIB_IMPLEMENTATION=13 mjk's GLUT 3.7 release with GameGLUT support.
}

  _GLUT_XLIB_IMPLEMENTATION = 13;

  { Display mode bit masks. }
  GLUT_RGB=0;
  GLUT_RGBA=GLUT_RGB;
  GLUT_INDEX=1;
  GLUT_SINGLE=0;
  GLUT_DOUBLE=2;
  GLUT_ACCUM=4;
  GLUT_ALPHA=8;
  GLUT_DEPTH=16;
  GLUT_STENCIL=32;
  GLUT_MULTISAMPLE=128;
  GLUT_STEREO=256;
  GLUT_LUMINANCE=512;

  { Mouse buttons. }
  GLUT_LEFT_BUTTON=0;
  GLUT_MIDDLE_BUTTON=1;
  GLUT_RIGHT_BUTTON=2;

  { Mouse button  state. }
  GLUT_DOWN=0;
  GLUT_UP=1;

  { function keys }
  GLUT_KEY_F1=1;
  GLUT_KEY_F2=2;
  GLUT_KEY_F3=3;
  GLUT_KEY_F4=4;
  GLUT_KEY_F5=5;
  GLUT_KEY_F6=6;
  GLUT_KEY_F7=7;
  GLUT_KEY_F8=8;
  GLUT_KEY_F9=9;
  GLUT_KEY_F10=10;
  GLUT_KEY_F11=11;
  GLUT_KEY_F12=12;
  { directional keys }
  GLUT_KEY_LEFT=100;
  GLUT_KEY_UP=101;
  GLUT_KEY_RIGHT=102;
  GLUT_KEY_DOWN=103;
  GLUT_KEY_PAGE_UP=104;
  GLUT_KEY_PAGE_DOWN=105;
  GLUT_KEY_HOME=106;
  GLUT_KEY_END=107;
  GLUT_KEY_INSERT=108;

  { Entry/exit  state. }
  GLUT_LEFT=0;
  GLUT_ENTERED=1;

  { Menu usage  state. }
  GLUT_MENU_NOT_IN_USE=0;
  GLUT_MENU_IN_USE=1;

  { Visibility  state. }
  GLUT_NOT_VISIBLE=0;
  GLUT_VISIBLE=1;

  { Window status  state. }
  GLUT_HIDDEN=0;
  GLUT_FULLY_RETAINED=1;
  GLUT_PARTIALLY_RETAINED=2;
  GLUT_FULLY_COVERED=3;

  { Color index component selection values. }
  GLUT_RED=0;
  GLUT_GREEN=1;
  GLUT_BLUE=2;

{$ifdef WIN32}
  { Stroke font constants (use these in GLUT program). }
  GLUT_STROKE_ROMAN=pointer(0);
  GLUT_STROKE_MONO_ROMAN=pointer(1);

  { Bitmap font constants (use these in GLUT program). }
  GLUT_BITMAP_9_BY_15=pointer(2);
  GLUT_BITMAP_8_BY_13=pointer(3);
  GLUT_BITMAP_TIMES_ROMAN_10=pointer(4);
  GLUT_BITMAP_TIMES_ROMAN_24=pointer(5);
  GLUT_BITMAP_HELVETICA_10=pointer(6);
  GLUT_BITMAP_HELVETICA_12=pointer(7);
  GLUT_BITMAP_HELVETICA_18=pointer(8);
{$else}

 {w oryginalnym glut.h funkcje ponizej to byly makra.
  Kazde z tych makr zwraca adres odpowiedniej funkcji - w oryginale funkcje
  te musialy byc wiec zadeklarowane tutaj, w interface. Ale my te ukryte
  funkcje deklarujemy jako rzeczywiscie ukryte w implementacji a makra
  rozwiazujemy w initialization modulu do ponizszych zmiennych. }
var
  { Stroke font constants (use these in GLUT program). }
  GLUT_STROKE_ROMAN :pointer = nil;
  GLUT_STROKE_MONO_ROMAN :pointer = nil;

  { Bitmap font constants (use these in GLUT program). }
  GLUT_BITMAP_9_BY_15 :pointer = nil;
  GLUT_BITMAP_8_BY_13 :pointer = nil;
  GLUT_BITMAP_TIMES_ROMAN_10 :pointer = nil;
  GLUT_BITMAP_TIMES_ROMAN_24 :pointer = nil;
  GLUT_BITMAP_HELVETICA_10 :pointer = nil;
  GLUT_BITMAP_HELVETICA_12 :pointer = nil;
  GLUT_BITMAP_HELVETICA_18 :pointer = nil;
{$endif}

const
  { glutGet parameters. }
  GLUT_WINDOW_X=100;
  GLUT_WINDOW_Y=101;
  GLUT_WINDOW_WIDTH=102;
  GLUT_WINDOW_HEIGHT=103;
  GLUT_WINDOW_BUFFER_SIZE=104;
  GLUT_WINDOW_STENCIL_SIZE=105;
  GLUT_WINDOW_DEPTH_SIZE=106;
  GLUT_WINDOW_RED_SIZE=107;
  GLUT_WINDOW_GREEN_SIZE=108;
  GLUT_WINDOW_BLUE_SIZE=109;
  GLUT_WINDOW_ALPHA_SIZE=110;
  GLUT_WINDOW_ACCUM_RED_SIZE=111;
  GLUT_WINDOW_ACCUM_GREEN_SIZE=112;
  GLUT_WINDOW_ACCUM_BLUE_SIZE=113;
  GLUT_WINDOW_ACCUM_ALPHA_SIZE=114;
  GLUT_WINDOW_DOUBLEBUFFER=115;
  GLUT_WINDOW_RGBA=116;
  GLUT_WINDOW_PARENT=117;
  GLUT_WINDOW_NUM_CHILDREN=118;
  GLUT_WINDOW_COLORMAP_SIZE=119;
  GLUT_WINDOW_NUM_SAMPLES=120;
  GLUT_WINDOW_STEREO=121;
  GLUT_WINDOW_CURSOR=122;
  GLUT_SCREEN_WIDTH=200;
  GLUT_SCREEN_HEIGHT=201;
  GLUT_SCREEN_WIDTH_MM=202;
  GLUT_SCREEN_HEIGHT_MM=203;
  GLUT_MENU_NUM_ITEMS=300;
  GLUT_DISPLAY_MODE_POSSIBLE=400;
  GLUT_INIT_WINDOW_X=500;
  GLUT_INIT_WINDOW_Y=501;
  GLUT_INIT_WINDOW_WIDTH=502;
  GLUT_INIT_WINDOW_HEIGHT=503;
  GLUT_INIT_DISPLAY_MODE=504;
  GLUT_ELAPSED_TIME=700;
  GLUT_WINDOW_FORMAT_ID=123;

  { glutDeviceGet parameters. }
  GLUT_HAS_KEYBOARD=600;
  GLUT_HAS_MOUSE=601;
  GLUT_HAS_SPACEBALL=602;
  GLUT_HAS_DIAL_AND_BUTTON_BOX=603;
  GLUT_HAS_TABLET=604;
  GLUT_NUM_MOUSE_BUTTONS=605;
  GLUT_NUM_SPACEBALL_BUTTONS=606;
  GLUT_NUM_BUTTON_BOX_BUTTONS=607;
  GLUT_NUM_DIALS=608;
  GLUT_NUM_TABLET_BUTTONS=609;
  GLUT_DEVICE_IGNORE_KEY_REPEAT=610;
  GLUT_DEVICE_KEY_REPEAT=611;
  GLUT_HAS_JOYSTICK=612;
  GLUT_OWNS_JOYSTICK=613;
  GLUT_JOYSTICK_BUTTONS=614;
  GLUT_JOYSTICK_AXES=615;
  GLUT_JOYSTICK_POLL_RATE=616;

  { glutLayerGet parameters. }
  GLUT_OVERLAY_POSSIBLE=800;
  GLUT_LAYER_IN_USE=801;
  GLUT_HAS_OVERLAY=802;
  GLUT_TRANSPARENT_INDEX=803;
  GLUT_NORMAL_DAMAGED=804;
  GLUT_OVERLAY_DAMAGED=805;

  { glutVideoResizeGet parameters. }
  GLUT_VIDEO_RESIZE_POSSIBLE=900;
  GLUT_VIDEO_RESIZE_IN_USE=901;
  GLUT_VIDEO_RESIZE_X_DELTA=902;
  GLUT_VIDEO_RESIZE_Y_DELTA=903;
  GLUT_VIDEO_RESIZE_WIDTH_DELTA=904;
  GLUT_VIDEO_RESIZE_HEIGHT_DELTA=905;
  GLUT_VIDEO_RESIZE_X=906;
  GLUT_VIDEO_RESIZE_Y=907;
  GLUT_VIDEO_RESIZE_WIDTH=908;
  GLUT_VIDEO_RESIZE_HEIGHT=909;

  { glutUseLayer parameters. }
  GLUT_NORMAL=0;
  GLUT_OVERLAY=1;

  { glutGetModifiers return mask. }
  GLUT_ACTIVE_SHIFT=1;
  GLUT_ACTIVE_CTRL=2;
  GLUT_ACTIVE_ALT=4;

  { glutSetCursor parameters. }
  { Basic arrows. }
  GLUT_CURSOR_RIGHT_ARROW=0;
  GLUT_CURSOR_LEFT_ARROW=1;
  { Symbolic cursor shapes. }
  GLUT_CURSOR_INFO=2;
  GLUT_CURSOR_DESTROY=3;
  GLUT_CURSOR_HELP=4;
  GLUT_CURSOR_CYCLE=5;
  GLUT_CURSOR_SPRAY=6;
  GLUT_CURSOR_WAIT=7;
  GLUT_CURSOR_TEXT=8;
  GLUT_CURSOR_CROSSHAIR=9;
  { Directional cursors. }
  GLUT_CURSOR_UP_DOWN=10;
  GLUT_CURSOR_LEFT_RIGHT=11;
  { Sizing cursors. }
  GLUT_CURSOR_TOP_SIDE=12;
  GLUT_CURSOR_BOTTOM_SIDE=13;
  GLUT_CURSOR_LEFT_SIDE=14;
  GLUT_CURSOR_RIGHT_SIDE=15;
  GLUT_CURSOR_TOP_LEFT_CORNER=16;
  GLUT_CURSOR_TOP_RIGHT_CORNER=17;
  GLUT_CURSOR_BOTTOM_RIGHT_CORNER=18;
  GLUT_CURSOR_BOTTOM_LEFT_CORNER=19;
  { Inherit from parent window. }
  GLUT_CURSOR_INHERIT=100;
  { Blank cursor. }
  GLUT_CURSOR_NONE=101;
  { Fullscreen crosshair (if available). }
  GLUT_CURSOR_FULL_CROSSHAIR=102;

var
  { GLUT initialization sub-API. }
  glutInit: procedure(argcp:Pinteger; argv:{(const array of PChar)}Pointer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutInitDisplayMode: procedure(mode:LongWord); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutInitDisplayString: procedure(s:PChar); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutInitWindowPosition: procedure(x, y:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutInitWindowSize: procedure(width, height:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutMainLoop: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT window sub-API. }
  glutCreateWindow: function(title:PChar) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutCreateSubWindow: function(win, x, y, width, height:integer) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutDestroyWindow: procedure(win:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPostRedisplay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPostWindowRedisplay: procedure(win:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSwapBuffers: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutGetWindow: function :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetWindow: procedure(win:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetWindowTitle: procedure(title:PChar); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetIconTitle: procedure(title:PChar); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPositionWindow: procedure(x, y:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutReshapeWindow: procedure(width, height:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPopWindow: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPushWindow: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutIconifyWindow: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutShowWindow: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutHideWindow: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutFullScreen: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetCursor: procedure(cursor:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWarpPointer: procedure(x, y:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT overlay sub-API. }
  glutEstablishOverlay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutRemoveOverlay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutUseLayer: procedure(layer:TGLenum); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPostOverlayRedisplay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPostWindowOverlayRedisplay: procedure(win:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutShowOverlay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutHideOverlay: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

type TProc=procedure; cdecl;
     TProcInt=procedure(x:integer); cdecl;
     TProcIntInt=procedure(w,h:integer); cdecl;
     TProcIntIntInt=procedure(a,b,c:integer); cdecl;
     TProcCharIntInt=procedure(key:char; x,y:integer); cdecl;
     TProcIntIntIntInt=procedure(button, state, x,y:integer); cdecl;
     TProcUintIntIntInt=procedure(buttonMask:LongWord; x,y,z:integer); cdecl;

var
  { GLUT menu sub-API. }
  glutCreateMenu: function(proc:TProcInt) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutDestroyMenu: procedure(menu:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutGetMenu: function :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetMenu: procedure(menu:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutAddMenuEntry: procedure(alabel:PChar; value:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutAddSubMenu: procedure(alabel:Pchar; submenu:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutChangeToMenuEntry: procedure(item:integer; alabel:PChar; value:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutChangeToSubMenu: procedure(item:integer; alabel:PChar; submenu:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutRemoveMenuItem: procedure(item:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutAttachMenu: procedure(button:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutDetachMenu: procedure(button:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT window callback sub-API. }
  glutDisplayFunc: procedure(proc:TProc); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutReshapeFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutKeyboardFunc: procedure(proc:TProcCharIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutMouseFunc: procedure(proc:TProcIntIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutMotionFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutPassiveMotionFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutEntryFunc: procedure(proc:TProcInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutVisibilityFunc: procedure(proc:TProcInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutIdleFunc: procedure(proc:TProc); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutTimerFunc: procedure(millis:LongWord; proc:TProcInt; value:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutMenuStateFunc: procedure(proc:TProcInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSpecialFunc: procedure(proc:TProcIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSpaceballMotionFunc: procedure(proc:TProcIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSpaceballRotateFunc: procedure(proc:TProcIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSpaceballButtonFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutButtonBoxFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutDialsFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutTabletMotionFunc: procedure(proc:TProcIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutTabletButtonFunc: procedure(proc:TProcIntIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutMenuStatusFunc: procedure(proc:TProcIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutOverlayDisplayFunc: procedure(proc:TProc); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWindowStatusFunc: procedure(proc:TProcInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  (*Kambi note : not implemented yet
  glutKeyboardUpFunc: procedure(proc:TProcCharIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSpecialUpFunc: procedure(proc:TProcIntIntInt); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutJoystickFunc: procedure(proc:TProcUintIntIntInt; pollInterval:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  *)

  { GLUT color index sub-API. }
  glutSetColor: procedure(x:integer; red, green, blue:TGLfloat); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutGetColor: function(ndx, component:integer) :TGLfloat; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutCopyColormap: procedure(win:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT state retrieval sub-API. }
  glutGet: function(aType:TGLenum) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutDeviceGet: function(aType:TGLenum) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  { GLUT extension support sub-API }
  glutExtensionSupported: function(aName:PChar) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutGetModifiers: function :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutLayerGet: function(aType:TGLenum) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT font sub-API }
  glutBitmapCharacter: procedure(font:pointer; character:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutBitmapWidth: function(font:pointer; character:integer) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutStrokeCharacter: procedure(font:pointer; character:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutStrokeWidth: function(font:pointer; character:integer) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutBitmapLength: function(font:pointer; s:PChar) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutStrokeLength: function(font:pointer; s:PChar) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT pre-built models sub-API }
  glutWireSphere: procedure(radius:TGLdouble; slices, stacks:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidSphere: procedure(radius:TGLdouble; slices, stacks:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireCone: procedure(base, height:TGLdouble; slices, stacks:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidCone: procedure(base, height:TGLdouble; slices, stacks:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireCube: procedure(size:TGLdouble); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidCube: procedure(size:TGLdouble); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireTorus: procedure(innerRadius,outerRadius:TGLdouble; sides,rings:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidTorus: procedure(innerRadius,outerRadius:TGLdouble; sides,rings:TGLint); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireDodecahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidDodecahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireTeapot: procedure(size:TGLdouble); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidTeapot: procedure(size:TGLdouble); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireOctahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidOctahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireTetrahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidTetrahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutWireIcosahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSolidIcosahedron: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT video resize sub-API. }
  glutVideoResizeGet: function(param:TGLenum) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetupVideoResizing: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutStopVideoResizing: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutVideoResize: procedure(x, y, width, height:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutVideoPan: procedure(x, y, width, height:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT debugging sub-API. }
  glutReportErrors: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}

  { GLUT device control sub-API. }
  { glutSetKeyRepeat modes. }
const
  GLUT_KEY_REPEAT_OFF=0;
  GLUT_KEY_REPEAT_ON=1;
  GLUT_KEY_REPEAT_DEFAULT=2;

  { Joystick button masks. }
  GLUT_JOYSTICK_BUTTON_A=1;
  GLUT_JOYSTICK_BUTTON_B=2;
  GLUT_JOYSTICK_BUTTON_C=4;
  GLUT_JOYSTICK_BUTTON_D=8;

  (*not implemented
  glutIgnoreKeyRepeat: procedure(ignore:integer);  {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutSetKeyRepeat: procedure(repeatMode:integer); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutForceJoystickFunc: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  *)

const
  { GLUT game mode sub-API. }
  { glutGameModeGet. }
  GLUT_GAME_MODE_ACTIVE=0;
  GLUT_GAME_MODE_POSSIBLE=1;
  GLUT_GAME_MODE_WIDTH=2;
  GLUT_GAME_MODE_HEIGHT=3;
  GLUT_GAME_MODE_PIXEL_DEPTH=4;
  GLUT_GAME_MODE_REFRESH_RATE=5;
  GLUT_GAME_MODE_DISPLAY_CHANGED=6;

  (*not implemented yet
  glutGameModeString: procedure(s:Pchar); {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutEnterGameMode: function :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutLeaveGameMode: procedure; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  glutGameModeGet: function(mode:TGLenum) :integer; {$ifdef GLUT_CDECL} cdecl; {$endif} {$ifdef GLUT_STDCALL} stdcall; {$endif}
  *)

  {Kambi : my glutPascalInit calls glutInit(argc, argv) constructing
    argc and argv from ParamCount and ParamStr. Advantage : now you can
    call Pascal Glut programs with arguments like --geometry and other,
    and Glut will handle these. Disadvantage : there is no standard Pascal
    way to delete some parameters from ParamStr and so you will not
    know which command-line options were handled by Glut. }
  procedure glutPascalInit;

implementation

uses KambiUtils;

{procedury pomocnicze dla Gluta, zrobione przeze mnie ----------------------------}

procedure glutPascalInit;
{glutInit obok inicjacji calego gluta robi bardzo fajna rzecz :
   przetwarza argumenty naszego programu,
   te ktore rozpoznaje - realizuje i wyrzuca je z naszych argumentow.
 Sztuczka z wyrzucaniem argumetow polega na modyfikacji argc i argv ktore
   typowo sa przekazanymi parametrami jakie otrzymala main().
 Pomysl uczynienia argumentow programu zmiennymi lokalnymi w procedurze
   main() wcale nie jest dobry w ANSI C gdy trzeba gdzies indziej przetwarzac
   parametry.
 Ale ParamCount i ParamStr Pascala tez maja wade : to sa funkcje,
   wiec nie mozna samemu modyfikowac wlasnych parametrow. Patrz ParCount i
   ParStr w KambiUtils ktore umozliwiaja takie zachowanie, ale tu ich nie uzywam
   (nie chce czynic glut.pas zaleznym od KambiUtils.pas).

 Jak by nie bylo, tworzymy tutaj specjalnie dla glutInit argc i argv na podstawie
   ParamCount/Str. Niestety, nie mozemy pozniej usunac przetworzonych przez
   gluta argumentow z listy ParamStr - trudno. Proste programy w glut+openGL'u
   i tak nie beda pewnie zwracac uwagi na swoje parametry.
 Przykladowy parametr rozumiany przez gluta pod X-ami to -geometry WxH+X+Y
}
type ArrayPChar=array[0..High(Word)]of PChar;
     PArrayPChar=^ArrayPChar;
var argc, i:integer;
    argv, argvcopy:PArrayPChar;
begin
 argc:=ParamCount+1;
 argv:=GetMem((argc+1)*SizeOf(PChar));
 argvcopy:=GetMem(argc*SizeOf(PChar));
 for i:=0 to ParamCount do
 begin
  argv[i]:=StrNew(PChar(ParamStr(i))); { KOPIUJEMY ParamStr do nowego PChara- tak najbezpieczniej }
  argvcopy[i]:=argv[i];
 end;
 argv[argc]:=nil; { ostatni element tablicy argv[] powinien byc ustawiony na nil }

 glutInit(@argc,argv);

 { cleanup :
   Dalismy glutowi na pozarcie skonstruowane argc i argv.
   Teraz chcemy zwolnic pamiec zaalokowana przez StrNew - ale glut byc moze
   zmodyfikowal nasza zawartosc tablicy argv usuwajac niektore wskazniki !
   I dlatego mysmy byli sprytni i przechowalismy je w argvcopy. }
 for i:=0 to ParamCount do StrDispose(argvcopy[i]);
 FreeMem(argv);
 FreeMem(argvcopy);
end;

{makra ------------------------------------------------------------------------}

 {pod nie-Windowsem glut wymaga troszke dodatkowej robotki.

  1.
    stale fontow pod glutem (GLUT_BITMAP_xx i GLUT_STROKE_xx) sa
    troszke kuriozalne pod UNIXem : sa to makra ktore zwracaja adres
    odpowiednich innych funkcji glutDLL (pod win32 GLUT_BITMAP_xx i GLUT_STROKE_xx
    to sa stale). W rezultacie musimy recznie inicjowac i zamykac gluta
    (dlopen/dlclose) aby uzyskac zmienna libglut:pointer przy uzyciu ktorej
    uzyskujemy adresy odpowiednich procedur przez dlsym(libglut,'xxx').

    Metoda zrobienia tego bez dlopen/close/sym przez konstrukcje w rodzaju np.
      function glutStrokeRoman:pointer; external glutdll;
      function GLUT_STROKE_ROMAN :pointer; begin result:=@glutStrokeRoman end;
    ...zawodzi. Bledne byloby tez przekonanie ze @proc gdzie proc jest
    eksportowana zwraca adres zmiennej zawierajacej adres funkcji eksportowanej :
      type PPointer=^pointer;
      function GLUT_STROKE_ROMAN :pointer;          begin result:=PPointer(@glutStrokeRoman)^ end;
    nie dziala rowniez. Nie dziala tez z
      type PPPointer=^PPointer;
      function GLUT_STROKE_ROMAN :pointer;          begin result:=PPPointer(@glutStrokeRoman)^^ end;
    i nic nie pomaga proba wywolywania najpierw glutStrokeRoman (aby
    na pewno zaladowac najpierw z libglut poprawny adres) - otoz okazuje sie
    ze nie mozemy wywolac sami glutStrokeRoman, byc moze korzysta ona
    z jakiejs statycznej zmiennej w libglut ktora nie jest zainicjowana jesli
    sami wywolamy glutStrokeRoman ? Przeciez faktycznie nikt mi nie pozwalal
    WYWOLAC glutStrokeRoman, ja mialem tylko przekazac jej adres.

    Jak by nie bylo, nikt mi nie gwarantowal ze jakakolwiek powyzsza metoda
    zadziala. Ale metoda ktorej uzylem tutaj musiala zadzialac i rzeczywiscie
    dziala - wiec wszystko ok.
}

{init/final ----------------------------------------------------------------}

var libglut :TDynLib;

initialization
 libglut:=TDynLib.Load(GlutDLL);

 { GLUT initialization sub-API. }
 @glutInit := libglut.Symbol('glutInit');
 @glutInitDisplayMode := libglut.Symbol('glutInitDisplayMode');
 @glutInitDisplayString := libglut.Symbol('glutInitDisplayString');
 @glutInitWindowPosition := libglut.Symbol('glutInitWindowPosition');
 @glutInitWindowSize := libglut.Symbol('glutInitWindowSize');
 @glutMainLoop := libglut.Symbol('glutMainLoop');

 { GLUT window sub-API. }
 @glutCreateWindow := libglut.Symbol('glutCreateWindow');
 @glutCreateSubWindow := libglut.Symbol('glutCreateSubWindow');
 @glutDestroyWindow := libglut.Symbol('glutDestroyWindow');
 @glutPostRedisplay := libglut.Symbol('glutPostRedisplay');
 @glutPostWindowRedisplay := libglut.Symbol('glutPostWindowRedisplay');
 @glutSwapBuffers := libglut.Symbol('glutSwapBuffers');
 @glutGetWindow := libglut.Symbol('glutGetWindow');
 @glutSetWindow := libglut.Symbol('glutSetWindow');
 @glutSetWindowTitle := libglut.Symbol('glutSetWindowTitle');
 @glutSetIconTitle := libglut.Symbol('glutSetIconTitle');
 @glutPositionWindow := libglut.Symbol('glutPositionWindow');
 @glutReshapeWindow := libglut.Symbol('glutReshapeWindow');
 @glutPopWindow := libglut.Symbol('glutPopWindow');
 @glutPushWindow := libglut.Symbol('glutPushWindow');
 @glutIconifyWindow := libglut.Symbol('glutIconifyWindow');
 @glutShowWindow := libglut.Symbol('glutShowWindow');
 @glutHideWindow := libglut.Symbol('glutHideWindow');
 @glutFullScreen := libglut.Symbol('glutFullScreen');
 @glutSetCursor := libglut.Symbol('glutSetCursor');
 @glutWarpPointer := libglut.Symbol('glutWarpPointer');

 { GLUT overlay sub-API. }
 @glutEstablishOverlay := libglut.Symbol('glutEstablishOverlay');
 @glutRemoveOverlay := libglut.Symbol('glutRemoveOverlay');
 @glutUseLayer := libglut.Symbol('glutUseLayer');
 @glutPostOverlayRedisplay := libglut.Symbol('glutPostOverlayRedisplay');
 @glutPostWindowOverlayRedisplay := libglut.Symbol('glutPostWindowOverlayRedisplay');
 @glutShowOverlay := libglut.Symbol('glutShowOverlay');
 @glutHideOverlay := libglut.Symbol('glutHideOverlay');

 { GLUT menu sub-API. }
 @glutCreateMenu := libglut.Symbol('glutCreateMenu');
 @glutDestroyMenu := libglut.Symbol('glutDestroyMenu');
 @glutGetMenu := libglut.Symbol('glutGetMenu');
 @glutSetMenu := libglut.Symbol('glutSetMenu');
 @glutAddMenuEntry := libglut.Symbol('glutAddMenuEntry');
 @glutAddSubMenu := libglut.Symbol('glutAddSubMenu');
 @glutChangeToMenuEntry := libglut.Symbol('glutChangeToMenuEntry');
 @glutChangeToSubMenu := libglut.Symbol('glutChangeToSubMenu');
 @glutRemoveMenuItem := libglut.Symbol('glutRemoveMenuItem');
 @glutAttachMenu := libglut.Symbol('glutAttachMenu');
 @glutDetachMenu := libglut.Symbol('glutDetachMenu');

 { GLUT window callback sub-API. }
 @glutDisplayFunc := libglut.Symbol('glutDisplayFunc');
 @glutReshapeFunc := libglut.Symbol('glutReshapeFunc');
 @glutKeyboardFunc := libglut.Symbol('glutKeyboardFunc');
 @glutMouseFunc := libglut.Symbol('glutMouseFunc');
 @glutMotionFunc := libglut.Symbol('glutMotionFunc');
 @glutPassiveMotionFunc := libglut.Symbol('glutPassiveMotionFunc');
 @glutEntryFunc := libglut.Symbol('glutEntryFunc');
 @glutVisibilityFunc := libglut.Symbol('glutVisibilityFunc');
 @glutIdleFunc := libglut.Symbol('glutIdleFunc');
 @glutTimerFunc := libglut.Symbol('glutTimerFunc');
 @glutMenuStateFunc := libglut.Symbol('glutMenuStateFunc');
 @glutSpecialFunc := libglut.Symbol('glutSpecialFunc');
 @glutSpaceballMotionFunc := libglut.Symbol('glutSpaceballMotionFunc');
 @glutSpaceballRotateFunc := libglut.Symbol('glutSpaceballRotateFunc');
 @glutSpaceballButtonFunc := libglut.Symbol('glutSpaceballButtonFunc');
 @glutButtonBoxFunc := libglut.Symbol('glutButtonBoxFunc');
 @glutDialsFunc := libglut.Symbol('glutDialsFunc');
 @glutTabletMotionFunc := libglut.Symbol('glutTabletMotionFunc');
 @glutTabletButtonFunc := libglut.Symbol('glutTabletButtonFunc');
 @glutMenuStatusFunc := libglut.Symbol('glutMenuStatusFunc');
 @glutOverlayDisplayFunc := libglut.Symbol('glutOverlayDisplayFunc');
 @glutWindowStatusFunc := libglut.Symbol('glutWindowStatusFunc');

 (*not implemented yet
 @glutKeyboardUpFunc := libglut.Symbol('glutKeyboardUpFunc');
 @glutSpecialUpFunc := libglut.Symbol('glutSpecialUpFunc');
 @glutJoystickFunc := libglut.Symbol('glutJoystickFunc');
 *)

 { GLUT color index sub-API. }
 @glutSetColor := libglut.Symbol('glutSetColor');
 @glutGetColor := libglut.Symbol('glutGetColor');
 @glutCopyColormap := libglut.Symbol('glutCopyColormap');

 { GLUT state retrieval sub-API. }
 @glutGet := libglut.Symbol('glutGet');
 @glutDeviceGet := libglut.Symbol('glutDeviceGet');
 { GLUT extension support sub-API }
 @glutExtensionSupported := libglut.Symbol('glutExtensionSupported');
 @glutGetModifiers := libglut.Symbol('glutGetModifiers');
 @glutLayerGet := libglut.Symbol('glutLayerGet');

 { GLUT font sub-API }
 @glutBitmapCharacter := libglut.Symbol('glutBitmapCharacter');
 @glutBitmapWidth := libglut.Symbol('glutBitmapWidth');
 @glutStrokeCharacter := libglut.Symbol('glutStrokeCharacter');
 @glutStrokeWidth := libglut.Symbol('glutStrokeWidth');
 @glutBitmapLength := libglut.Symbol('glutBitmapLength');
 @glutStrokeLength := libglut.Symbol('glutStrokeLength');

 { GLUT pre-built models sub-API }
 @glutWireSphere := libglut.Symbol('glutWireSphere');
 @glutSolidSphere := libglut.Symbol('glutSolidSphere');
 @glutWireCone := libglut.Symbol('glutWireCone');
 @glutSolidCone := libglut.Symbol('glutSolidCone');
 @glutWireCube := libglut.Symbol('glutWireCube');
 @glutSolidCube := libglut.Symbol('glutSolidCube');
 @glutWireTorus := libglut.Symbol('glutWireTorus');
 @glutSolidTorus := libglut.Symbol('glutSolidTorus');
 @glutWireDodecahedron := libglut.Symbol('glutWireDodecahedron');
 @glutSolidDodecahedron := libglut.Symbol('glutSolidDodecahedron');
 @glutWireTeapot := libglut.Symbol('glutWireTeapot');
 @glutSolidTeapot := libglut.Symbol('glutSolidTeapot');
 @glutWireOctahedron := libglut.Symbol('glutWireOctahedron');
 @glutSolidOctahedron := libglut.Symbol('glutSolidOctahedron');
 @glutWireTetrahedron := libglut.Symbol('glutWireTetrahedron');
 @glutSolidTetrahedron := libglut.Symbol('glutSolidTetrahedron');
 @glutWireIcosahedron := libglut.Symbol('glutWireIcosahedron');
 @glutSolidIcosahedron := libglut.Symbol('glutSolidIcosahedron');

 { GLUT video resize sub-API. }
 @glutVideoResizeGet := libglut.Symbol('glutVideoResizeGet');
 @glutSetupVideoResizing := libglut.Symbol('glutSetupVideoResizing');
 @glutStopVideoResizing := libglut.Symbol('glutStopVideoResizing');
 @glutVideoResize := libglut.Symbol('glutVideoResize');
 @glutVideoPan := libglut.Symbol('glutVideoPan');

 { GLUT debugging sub-API. }
 @glutReportErrors := libglut.Symbol('glutReportErrors');

 { GLUT device control sub-API. }
 {not implemented
 @glutIgnoreKeyRepeat := libglut.Symbol('glutIgnoreKeyRepeat');
 @glutSetKeyRepeat := libglut.Symbol('glutSetKeyRepeat');
 @glutForceJoystickFunc := libglut.Symbol('glutForceJoystickFunc');
 }

 { GLUT game mode sub-API. }
 {not implemented yet
 @glutGameModeString := libglut.Symbol('glutGameModeString');
 @glutEnterGameMode := libglut.Symbol('glutEnterGameMode');
 @glutLeaveGameMode := libglut.Symbol('glutLeaveGameMode');
 @glutGameModeGet := libglut.Symbol('glutGameModeGet');
 }

 {$ifndef WIN32}
 GLUT_STROKE_ROMAN:=libglut.Symbol('glutStrokeRoman');
 GLUT_STROKE_MONO_ROMAN:=libglut.Symbol('glutStrokeMonoRoman');

 GLUT_BITMAP_9_BY_15:=libglut.Symbol('glutBitmap9By15');
 GLUT_BITMAP_8_BY_13:=libglut.Symbol('glutBitmap8By13');
 GLUT_BITMAP_TIMES_ROMAN_10:=libglut.Symbol('glutBitmapTimesRoman10');
 GLUT_BITMAP_TIMES_ROMAN_24:=libglut.Symbol('glutBitmapTimesRoman24');
 GLUT_BITMAP_HELVETICA_10:=libglut.Symbol('glutBitmapHelvetica10');
 GLUT_BITMAP_HELVETICA_12:=libglut.Symbol('glutBitmapHelvetica12');
 GLUT_BITMAP_HELVETICA_18:=libglut.Symbol('glutBitmapHelvetica18');
 {$endif}

finalization
 FreeAndNil(libglut);
end.
