{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
unit freetypehdyn;

interface

uses sysutils, dynlibs;

{$DEFINE DYNAMIC}

{$i libfreetype.inc}


initialization
  //InitializeFreetype(FreeTypeDLL); - do not load DLL in initialization, it is loaded when needed in ftfont.InitEngine

finalization
  ReleaseFreetype;
end.
