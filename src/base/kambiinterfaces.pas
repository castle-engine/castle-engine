{
  Copyright 2007-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$interfaces com}


{ Utilities for interfaces. }
unit KambiInterfaces;

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
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): Hresult; virtual; stdcall;
  end;

  { A TPersistent descendant that can use interfaces and
    is not reference counted. Analogous to TNonRefCountedInterfacedObject. }
  TNonRefCountedInterfacedPersistent = class(TPersistent, IInterface)
  protected
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): Hresult; virtual; stdcall;
  end;

implementation

{ TNonRefCountedInterfacedObject --------------------------------------------- }

function TNonRefCountedInterfacedObject._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TNonRefCountedInterfacedObject._Release: Integer; stdcall;
begin
  Result := -1;
end;

function TNonRefCountedInterfacedObject.QueryInterface(
  const IID: TGUID; out Obj): Hresult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

{ TNonRefCountedInterfacedPersistent ----------------------------------------- }

function TNonRefCountedInterfacedPersistent._AddRef: Integer; stdcall;
begin
  Result := -1;
end;

function TNonRefCountedInterfacedPersistent._Release: Integer; stdcall;
begin
  Result := -1;
end;

function TNonRefCountedInterfacedPersistent.QueryInterface(
  const IID: TGUID; out Obj): Hresult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK else
    Result := E_NOINTERFACE;
end;

end.
