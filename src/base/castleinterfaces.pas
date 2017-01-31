{
  Copyright 2007-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$interfaces com}

{ In FPC > 2.4.2, IInterface methods signature changed.
  See
  http://wiki.freepascal.org/User_Changes_Trunk#IInterface.QueryInterface.2C_._AddRef_and_._Release_definitions_have_been_changed }
{$ifdef VER2_0} {$define OLD_IINTERFACE_METHODS} {$endif}
{$ifdef VER2_2} {$define OLD_IINTERFACE_METHODS} {$endif}
{$ifdef VER2_4} {$define OLD_IINTERFACE_METHODS} {$endif}

{ IInterface methods are stdcall always with older FPC, or only on Windows
  with newer FPC.
  When IINTERFACE_STDCALL is not defined, it means to use cdecl. }
{$ifdef OLD_IINTERFACE_METHODS} {$define IINTERFACE_STDCALL} {$endif}
{$ifdef WINDOWS}                {$define IINTERFACE_STDCALL} {$endif}

{ Utilities for interfaces. }
unit CastleInterfaces;

{$I castleconf.inc}

interface

uses Classes;

type
  { A class that can use interfaces and is not reference counted.

    For COM-style interfaces, it's needed to descend from this to provide
    dumb _AddRef and _Release implementations (that do nothing) and trivial
    QueryInterface implementation.

    See e.g. thread
    [http://lists.freepascal.org/lists/fpc-devel/2007-November/012060.html]. }
  TNonRefCountedInterfacedObject = class(IInterface)
  protected
    function _AddRef: Integer; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
    function _Release: Integer; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
    function QueryInterface({$ifdef OLD_IINTERFACE_METHODS} const {$else} constref {$endif} IID: TGUID; out Obj): Hresult; virtual; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
  end;

  { A TPersistent descendant that can use interfaces and
    is not reference counted. Analogous to TNonRefCountedInterfacedObject. }
  TNonRefCountedInterfacedPersistent = class(TPersistent, IInterface)
  protected
    function _AddRef: Integer; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
    function _Release: Integer; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
    function QueryInterface({$ifdef OLD_IINTERFACE_METHODS} const {$else} constref {$endif} IID: TGUID; out Obj): Hresult; virtual; {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
  end;

implementation

{ TNonRefCountedInterfacedObject --------------------------------------------- }

function TNonRefCountedInterfacedObject._AddRef: Integer;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  Result := -1;
end;

function TNonRefCountedInterfacedObject._Release: Integer;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  Result := -1;
end;

function TNonRefCountedInterfacedObject.QueryInterface(
  {$ifdef OLD_IINTERFACE_METHODS} const {$else} constref {$endif} IID: TGUID;
  out Obj): Hresult;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

{ TNonRefCountedInterfacedPersistent ----------------------------------------- }

function TNonRefCountedInterfacedPersistent._AddRef: Integer;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  Result := -1;
end;

function TNonRefCountedInterfacedPersistent._Release: Integer;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  Result := -1;
end;

function TNonRefCountedInterfacedPersistent.QueryInterface(
  {$ifdef OLD_IINTERFACE_METHODS} const {$else} constref {$endif} IID: TGUID;
  out Obj): Hresult;
  {$ifdef IINTERFACE_STDCALL} stdcall {$else} cdecl {$endif};
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

end.
