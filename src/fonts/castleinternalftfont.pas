{ Copied to Castle Game Engine from FPC RTL (FPC RTL uses the same license
  as Castle Game Engine, so no problem).
  Adjusted to use CastleInternalFreeType and CastleInternalFreeTypeH.
}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    This file is adapted from the FPC RTL source code, as such
    the license and copyright information of FPC RTL applies here.
    That said, the license of FPC RTL happens to be *exactly*
    the same as used by the "Castle Game Engine": LGPL (version 2.1)
    with "static linking exception" (with exactly the same wording
    of the "static linking exception").
    See the file COPYING.txt, included in this distribution, for details about
    the copyright of "Castle Game Engine".
    See http://www.freepascal.org/faq.var#general-license about the copyright
    of FPC RTL.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ @exclude Not ready for PasDoc. }
unit CastleInternalFtFont;

{$I castleconf.inc}

interface

uses SysUtils, Classes, {$ifdef FPC}FPCanvas, fpimgcmn,{$endif}
  CastleInternalFreeType, CastleInternalFreeTypeH, CastleUtils;

var
  FontMgr : TFontManager;

{ @raises EFreeTypeLibraryNotFound }
procedure InitEngine;
procedure DoneEngine;


implementation

procedure InitEngine;

begin
  if not assigned (FontMgr) then
    FontMgr := TFontManager.create;
end;

procedure DoneEngine;
begin
  if assigned (FontMgr) then
    FontMgr.Free;
end;

initialization
finalization
  DoneEngine;
end.
