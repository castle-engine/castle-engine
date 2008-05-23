{
  Copyright 2001-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Implements GLVersion, GLUVersion and related stuff handy for checking
  OpenGL version information, detect Mesa/other vendors etc.

  As you see, this unit doesn't use GL bindings itself.
  That's so that it may be used with various OpenGL bindings,
  like my old OpenGLh or FPC's GL, GLU, GLExt.
  So you must manually initialize GLVersion from some othe unit.
  As far as my engine is concerned, this will happen automatically
  by LoadAllExtensions call. Which is done by GLWindow on Init,
  or TKamOpenGLControl on GL context initialization.
}
unit GLVersionUnit;

{$include openglmac.inc}

interface

type
  { This is used to store OpenGL libraries (core OpenGL or GLU)
    version information.

    As obtained from glGetString(GL_VERSION)
    or gluGetString(GLU_VERSION), also by glGetString(GL_VENDOR).

    This is usually created by KambiGLUtils.LoadAllExtensions. }
  TGenericGLVersion = class
  public
    constructor Create(const VersionString: string);

    { Required (i.e. every OpenGL implemenetation has them)
      major and minor numbers.
      @groupBegin }
    Major: Integer;
    Minor: Integer;
    { @groupEnd }

    { Release is the optional release number (check ReleaseExists first).
      @groupBegin }
    ReleaseExists: boolean;
    Release: Integer;
    { @groupEnd }

    { VendorVersion is whatever vendor-specific information was placed
      inside VersionString, after the
      major_number.minor_number.release_number. It never has any whitespace
      at the beginning (we trim it when initializing). }
    VendorVersion: string;

    function AtLeast(AMajor, AMinor: Integer): boolean;
  end;

  TGLVersion = class(TGenericGLVersion)
  private
    FVendor: string;
    FIsVendorATI: boolean;
    FIsFglrx: boolean;
  public
    constructor Create(const VersionString, AVendor: string);

    { Using VendorSpecific information (extracted by base TGenericGLVersion)
      we can detect whether we the OpenGL implementation is Mesa (check IsMesa)
      and Mesa version.
      @groupBegin }
    IsMesa: boolean;
    MesaMajor: Integer;
    MesaMinor: Integer;
    MesaRelease: Integer;
    { @groupEnd }

    property Vendor: string read FVendor;

    { Is the Vendor ATI ? In other words, is it an ATI GPU with ATI drivers. }
    property IsVendorATI: boolean read FIsVendorATI;

    { Is the Vendor ATI and we're on Linux? }
    property IsFglrx: boolean read FIsFglrx;
  end;

var
  { Core OpenGL version information.
    This is usually created by KambiGLUtils.LoadAllExtensions. }
  GLVersion: TGLVersion;

  { GLU version information.
    This is usually created by KambiGLUtils.LoadAllExtensions. }
  GLUVersion: TGenericGLVersion;

implementation

uses SysUtils, KambiStringUtils, KambiUtils;

{ TGenericGLVersion ---------------------------------------------------------- }

type
  EInvalidGLVersionString = class(Exception);

procedure ParseWhiteSpaces(const S: string; var I: Integer);
begin
  while SCharIs(S, I, WhiteSpaces) do Inc(I);
end;

constructor TGenericGLVersion.Create(const VersionString: string);
const
  Digits = ['0'..'9'];
var
  NumberBegin, I: Integer;
begin
  inherited Create;

  try
    I := 1;

    { Note: we allow some whitespace that is not allowed by OpenGL/GLU
      spec. That's because we try hard to work correctly even with
      broken GL_VERSION / GLU_VERSION strings. }

    { Whitespace }
    ParseWhiteSpaces(VersionString, I);

    { Major number }
    if not SCharIs(VersionString, I, Digits) then
      raise EInvalidGLVersionString.Create('Major version number not found');
    NumberBegin := I;
    while SCharIs(VersionString, I, Digits) do Inc(I);
    Major := StrToInt(CopyPos(VersionString, NumberBegin, I - 1));

    { Whitespace }
    ParseWhiteSpaces(VersionString, I);

    { Dot }
    if not SCharIs(VersionString, I, '.') then
      raise EInvalidGLVersionString.Create(
        'The dot "." separator major and minor version number not found');
    Inc(I);

    { Whitespace }
    ParseWhiteSpaces(VersionString, I);

    { Minor number }
    if not SCharIs(VersionString, I, Digits) then
      raise EInvalidGLVersionString.Create('Minor version number not found');
    NumberBegin := I;
    while SCharIs(VersionString, I, Digits) do Inc(I);
    Minor := StrToInt(CopyPos(VersionString, NumberBegin, I - 1));

    ReleaseExists := SCharIs(VersionString, I, '.');

    if ReleaseExists then
    begin
      { Dot }
      Inc(I);

      { Release number }
      if not SCharIs(VersionString, I, Digits) then
      raise EInvalidGLVersionString.Create(
        'Release version number not found, ' +
        'although there was a dot after minor number');
      NumberBegin := I;
      while SCharIs(VersionString, I, Digits) do Inc(I);
      Release := StrToInt(CopyPos(VersionString, NumberBegin, I - 1));
    end;

    { Whitespace }
    ParseWhiteSpaces(VersionString, I);

    VendorVersion := SEnding(VersionString, I);
  except
    { In case of any error here: silence it.
      So actually EInvalidGLVersionString is not useful.
      We want our program to work even with broken GL_VERSION or GLU_VERSION
      strings.

      Class constructor always starts with Major and Minor initialized
      to 0, ReleaseExists initialized to false, and VendorVersion to ''.
      If we have here an exception, only part of them may be initialized. }
  end;
end;

function TGenericGLVersion.AtLeast(AMajor, AMinor: Integer): boolean;
begin
  Result := (AMajor < Major) or
    ( (AMajor = Major) and (AMinor <= Minor) );
end;

{ TGLVersion ----------------------------------------------------------------- }

constructor TGLVersion.Create(const VersionString, AVendor: string);
const
  Digits = ['0'..'9'];
var
  NumberBegin, I: Integer;
  VendorName: string;
begin
  inherited Create(VersionString);

  try
    I := 1;
    while SCharIs(VendorVersion, I, AllChars - WhiteSpaces) do Inc(I);

    VendorName := CopyPos(VendorVersion, 1, I - 1);
    IsMesa := SameText(VendorName, 'Mesa');

    if IsMesa then
    begin
      { Whitespace }
      ParseWhiteSpaces(VendorVersion, I);

      { Mesa major number }
      if not SCharIs(VendorVersion, I, Digits) then
        raise EInvalidGLVersionString.Create('Mesa major version number not found');
      NumberBegin := I;
      while SCharIs(VendorVersion, I, Digits) do Inc(I);
      MesaMajor := StrToInt(CopyPos(VendorVersion, NumberBegin, I - 1));

      { Whitespace }
      ParseWhiteSpaces(VendorVersion, I);

      { Dot }
      if not SCharIs(VendorVersion, I, '.') then
        raise EInvalidGLVersionString.Create(
          'The dot "." separator between Mesa major and minor version number not found');
      Inc(I);

      { Whitespace }
      ParseWhiteSpaces(VendorVersion, I);

      { Mesa minor number }
      if not SCharIs(VendorVersion, I, Digits) then
        raise EInvalidGLVersionString.Create('Mesa minor version number not found');
      NumberBegin := I;
      while SCharIs(VendorVersion, I, Digits) do Inc(I);
      MesaMinor := StrToInt(CopyPos(VendorVersion, NumberBegin, I - 1));

      { Whitespace }
      ParseWhiteSpaces(VendorVersion, I);

      { Dot }
      if SCharIs(VendorVersion, I, '.') then
      begin
        Inc(I);

        { Whitespace }
        ParseWhiteSpaces(VendorVersion, I);

        { Mesa release number }
        if not SCharIs(VendorVersion, I, Digits) then
          raise EInvalidGLVersionString.Create('Mesa release version number not found');
        NumberBegin := I;
        while SCharIs(VendorVersion, I, Digits) do Inc(I);
        MesaRelease := StrToInt(CopyPos(VendorVersion, NumberBegin, I - 1));
      end else
      begin
        { Some older Mesa versions (like 5.1) really don't have release
          number inside a version string. Seems like they don't have
          release number at all. So the missing dot "."
          separator between Mesa minor and release version number should
          be ignored. }
        MesaRelease := 0;
      end;
    end;
  except
    { Just like in TGenericGLVersion: in case of trouble (broken GL_VERSION
      string) ignore the problem. }
  end;

  FVendor := AVendor;

  { Although "ATI Technologies Inc." is usually found,
    according to http://delphi3d.net/hardware/listreports.php
    also just "ATI" is possible. }

  FIsVendorATI := (Vendor = 'ATI Technologies Inc.') or (Vendor = 'ATI');

  FIsFglrx := {$ifdef LINUX} IsVendorATI {$else} false {$endif};
end;

finalization
  FreeAndNil(GLVersion);
  FreeAndNil(GLUVersion);
end.
