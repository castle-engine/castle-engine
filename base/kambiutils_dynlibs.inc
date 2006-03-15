{
  Copyright 2003,2004 Michalis Kamburelis.

  This file is part of "Kambi's base Pascal units".

  "Kambi's base Pascal units" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi's base Pascal units" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi's base Pascal units"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ TDynLib class to help loading functions from dynamic libraries }

{$ifdef read_interface}

{ I wrote my own class to handle dynamic libraries because:
  - I wanted to have Load/Symbol functions that automatically DO error
    checking (and raise necessary exceptions)
  - I wanted to have a field SymbolErrorBehaviour - this lets me to tell
    ONCE, for all subsequent Symbol calls, what error checking I want.
    Default is to check errors and raise exceptions.
    There is also a very usefull value reWarnAndContinue:
    it allows you to run program ONCE and see all symbols that are missing
    from DLL.

  Typical usage:

  var ALLibrary:TDynLib = nil;
  initialization
   ALLibrary:=TDynLib.Load('libopenal.so');
   ... some calls to ALLibrary.Symbol() ...
  finalization
   FreeAndNil(ALLibrary);
  end.

  It is important that ALLibrary is initialized to nil (actually, writing
  " = nil" is not necessary for a global variable) and that in finalization
  you use Free(AndNil). This allows you to exit gracefully if library does not
  exist on the system and Load() will raise an exception: ALLibrary will
  stay then as nil and FreeAndNil(ALLibrary) will be a valid NOP.
  Using FreeAndNil(ALLibrary) instead of ALLibrary.Free is just a good
  practice.
}

type
  TDynLibHandle = {$ifdef FPC} TLibHandle {$else} HModule {$endif};

{$ifndef FPC}
const
  { Invalid TDynLibHandle value (meaning : LoadLibrary failed) }
  InvalidDynLibHandle: TDynLibHandle = 0;
{$else}
  { Workaround for FPC bug [http://www.freepascal.org/bugs/showrec.php3?ID=4583] }
{$define InvalidDynLibHandle := DynLibs.NilHandle}
{$endif}

type
  EDynLibError =class(Exception);
  TDynLibSymbolErrorBehaviour = (seRaise, seReturnNil, seWarnAndReturnNil);
  TDynLib = class
  private
    { klasa TDynLib zawsze operuje na FHandle ktore jest valid. }
    FHandle:TDynLibHandle;
    FName:string;
  public
    { nazwa biblioteki, czyli po prostu nazwa odpowiedniego pliku.
      Dokladnie co to znaczy - patrz semantyka SysUtils.LoadLibrary
      (DynLibs for FPC) na danej platformie. }
    property Name:string read FName;
    { Zeby uzyc konstruktora musisz posiadac juz Handle ktore jest valid
      (konstruktor rzuci wyjatek jesli uzyjesz AHandle invalid).
      Zazwyczaj wiec nie bedziesz uzywal konstruktora zeby stworzyc
      ten obiekt ale uzyjesz Load. }
    constructor Create(const AName:string; AHandle:TDynLibHandle);
    destructor Destroy; override;
    { Zainicjuje Handle dll'a o nazwie name. Jesli nie ma takiego dll'a
      w systemie to:
	if CheckResult to rzuci wyjatek EDynLibError else
	                  zwroci nil;
      Jesli nie bylo bledu - zwroci obiekt TDynLib.
      (a wiec if CheckResult to Load nigdy nie zwroci nil).

      Note that the default situation prevents from unintentionally ignoring
      an error and I consider it an advantage. }
    class function Load(const AName:string; CheckResult:boolean):TDynLib; overload;
    class function Load(const AName:string {CheckResult=true}):TDynLib; overload;

  public
    SymbolErrorBehaviour:TDynLibSymbolErrorBehaviour; { = seRaise }
    { Symbol zwraca adres symbolu o danej nazwie w bibliotece dynamicznej.
      Jezeli nie istnieje taki symbol to w zaleznosci od
      SymbolErrorBehaviour :
	seRaise (default) -> raise EDynLibError
	seReturnNil -> return nil (and continue, ignoring error)
	seWarnAndReturnNil -> write warning (using WarningWrite)
	  and return nil (and continue, ignoring error)
	  (useful for debugging : you can easily open the library and after
	  one run of the program you can see what symbols (that you requested)
	  were missing from the library. This is useful when you have a library
	  but you are not sure whether it is compatible and contains all the
	  symbols that you want).
      Of course remember that Symbol() can only check if symbol _exists_ in
      library. If it is function and it has different parameter list,
      different calling convention, etc. -> you will often end up with some nasty
      errors, like EAccessViolation. Often these errors are (practically)
      unrecoverable.
    }
    function Symbol(ProcName: PChar):Pointer;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TDynLib ----------------------------------------------------------------- }

constructor TDynLib.Create(const AName:string; AHandle:TDynLibHandle);
begin
 inherited Create;
 FName:=AName;
 FHandle:=AHandle;
 Check(AHandle <> InvalidDynLibHandle,
   'TDynLib can not be created with invalid DynLibHandle');
 SymbolErrorBehaviour:=seRaise;
end;

destructor TDynLib.Destroy;
begin
 { Should we check here for errors after FreeLibrary ?
   Well, this is finalization code so this is one place where strict error
   checking (and raising exceptions on them) may be not so good idea.
   For now I will not do it. }
 {$ifdef FPC} UnloadLibrary {$else} FreeLibrary {$endif} (FHandle);

 inherited;
end;

class function TDynLib.Load(const AName:string; CheckResult:boolean):TDynLib;

  function LoadLibraryGlobally(AName:PChar):TDynLibHandle;
  { TODO: under UNIX (Linux, more specifically, since I don't use this code
    with any other UNIX yet) I must load with RTLD_GLOBAL, else GLU crashes
    (it seems GLU requires that someone else loads GL symbols for it ?
    I really don't know. TO BE FIXED.) }
  begin
   result:=
     {$ifdef UNIX} TDynLibHandle( dlopen(AName, RTLD_LAZY or RTLD_GLOBAL) );
     {$else} LoadLibrary(AName);
     {$endif}
  end;

var Handle:TDynLibHandle;
begin
 Handle:=LoadLibraryGlobally(PChar(AName));
 if Handle = InvalidDynLibHandle then
 begin
  if CheckResult then
   raise EDynLibError.Create('Can''t load library "' +AName+ '"'
     {$ifdef UNIX}
       {$ifdef USE_LIBC} + ': ' + dlerror {$else} {TODO} {$endif}
     {$endif}) else
   result:=nil;
 end else
  result:=Self.Create(AName, Handle);
end;

class function TDynLib.Load(const AName:string {CheckResult=true}):TDynLib;
begin
 result:=Load(AName, true);
end;

function TDynLib.Symbol(ProcName: PChar):Pointer;

  function ErrStr:string;
  begin result:='Symbol "'+ProcName+'" not found in library "'+Name+'"' end;

begin
 result:= {$ifdef FPC} GetProcedureAddress {$else} GetProcAddress {$endif}
   (FHandle, ProcName);
 if result=nil then
  case SymbolErrorBehaviour of
   seRaise: raise EDynLibError.Create(ErrStr);
   seReturnNil: ;
   seWarnAndReturnNil: WarningWrite(ErrStr);
   else raise EInternalError.Create('SymbolErrorBehaviour=?');
  end;
end;

{$endif read_implementation}