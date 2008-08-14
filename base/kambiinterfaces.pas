{
  Copyright 2007 Michalis Kamburelis.

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
