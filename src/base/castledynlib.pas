{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Dynamic libraries loading (TDynLib). }
unit CastleDynLib;

{$I castleconf.inc}

interface

uses
  {$ifdef MSWINDOWS} Windows, {$endif}
  {$ifdef UNIX} Unix, Dl, {$endif}
  SysUtils
  {$ifdef FPC} , DynLibs {$endif};

type
  TDynLibHandle = {$ifdef FPC} TLibHandle {$else} HModule {$endif};

{$ifndef FPC}
const
  { Invalid TDynLibHandle value (meaning : LoadLibrary failed) }
  InvalidDynLibHandle: TDynLibHandle = 0;
{$else}
  { Workaround for FPC bug [http://bugs.freepascal.org/view.php?id=6489] }
{$define InvalidDynLibHandle := DynLibs.NilHandle}
{$endif}

type
  { }
  EDynLibError =class(Exception);

  TDynLibSymbolErrorBehaviour = (seRaise, seReturnNil, seWarnAndReturnNil);

  (*Load functions from dynamic libraries.

    I wrote my own class to handle dynamic libraries because:

    @unorderedList(
      @item(I wanted to have @link(Load) and @link(Symbol) functions that
        @italic(by default do error checking) (and raise necessary exceptions).)

      @item(I wanted to have a field SymbolErrorBehaviour --- this lets me to
        specify, @italic(once for all subsequent Symbol calls),
        what error checking I want.
        Default is to check errors and raise exceptions.
        There is also a very usefull value reWarnAndContinue:
        it allows you to run program @italic(once) and see all symbols that
        are missing from dynamic library.)

      @item(Also, the interface of this is OS-independent and works for
        both FPC and Delphi, so you can avoid ugly $ifdefs in your code.)
    )

    Typical usage:

    @longCode(#
      var
        ALLibrary: TDynLib = nil;
      initialization
        ALLibrary := TDynLib.Load('libopenal.so');
        { ... some calls to ALLibrary.Symbol() ... }
      finalization
        FreeAndNil(ALLibrary);
      end.
    #)

    It is important that ALLibrary is initialized to nil (actually, writing
    " = nil" is not necessary for a global variable) and that in finalization
    you use Free(AndNil). This allows you to exit gracefully if library does not
    exist on the system and @link(Load) will raise an exception: ALLibrary will
    stay then as nil and FreeAndNil(ALLibrary) will be a valid NOP.
    Using FreeAndNil(ALLibrary) instead of ALLibrary.Free is just a good
    practice.
  *)
  TDynLib = class
  private
    { In this class, we always have a valid FHandle. }
    FHandle: TDynLibHandle;
    FName: string;
    FSymbolErrorBehaviour: TDynLibSymbolErrorBehaviour;
  public
    { Standard constructor, requires a valid TDynLibHandle already.
      Usually you will prefer to use @link(Load) method instead of
      directly calling this constructor.

      @raises(ECheckFailed if you supply invalid handle.) }
    constructor Create(const AName: string; AHandle: TDynLibHandle);
    destructor Destroy; override;

    { Name of the library to link to. In practice, file name of the *.so
      or *.dylib or *.dll file.

      A precise strategy where this library is searched
      is specific to a platform, see the semantics of SysUtils.LoadLibrary
      (DynLibs for FPC) call on given OS. }
    property Name: string read FName;

    { Link to a dynamic library specified by Name. Returns created
      TDynLib instance.

      If the library is not found and RaiseExceptionOnError is @false,
      @nil will be returned.
      If RaiseExceptionOnError is @true then EDynLibError will be raised
      in case library is not found.
      So if RaiseExceptionOnError is @true, @nil is
      never returned.

      Note that the default situation prevents from unintentionally ignoring
      an error and @italic(that's good).

      @raises(EDynLibError If library not found and RaiseExceptionOnError
        is @true.) }
    class function Load(const AName: string; RaiseExceptionOnError: boolean = true): TDynLib;

    { What happens when @link(Symbol) fails. }
    property SymbolErrorBehaviour: TDynLibSymbolErrorBehaviour
      read FSymbolErrorBehaviour write FSymbolErrorBehaviour
      default seRaise;

    { Return address of given symbol (function name etc.) from loaded dynamic
      library. If the symbol doesn't exist, then SymbolErrorBehaviour
      says what happens:

      @unorderedList(
        @item(seRaise (default), then EDynLibError will be raised.)
	@item(seReturnNil, then return @nil (and continue, ignoring error).)
	@item(seWarnAndReturnNil, then write warning (using WarningWrite)
	  and return @nil (and continue, ignoring error).

          This is useful for debugging : you can easily open the library and after
	  one run of the program you can see what symbols (that you requested)
	  were missing from the library. This is useful when you have a library
	  but you are not sure whether it is compatible and contains all the
	  symbols that you want.)
      )

      @raises(EDynLibError If SymbolName doesn't exist and
        SymbolErrorBehaviour is seRaise.)

    }
    function Symbol(SymbolName: PChar): Pointer;
  end;

implementation

uses CastleUtils;

constructor TDynLib.Create(const AName: string; AHandle: TDynLibHandle);
begin
 inherited Create;
 FName := AName;
 FHandle := AHandle;
 Check(AHandle <> InvalidDynLibHandle,
   'TDynLib can not be created with invalid DynLibHandle');
 SymbolErrorBehaviour := seRaise;
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

class function TDynLib.Load(const AName: string; RaiseExceptionOnError: boolean): TDynLib;

  function LoadLibraryGlobally(AName: PChar): TDynLibHandle;
  { TODO: under UNIX (Linux, more specifically, since I don't use this code
    with any other UNIX yet) I must load with RTLD_GLOBAL, else GLU crashes
    (it seems GLU requires that someone else loads GL symbols for it ?
    I really don't know. TO BE FIXED.) }
  begin
   result:=
     {$ifdef UNIX} TDynLibHandle( dlopen(AName,
       { RTLD_GLOBAL cannot be used if you want to successfully open
         libopenal.so on Android (tested necessity on Nexus 5).
         It seems that RTLD_NOW or RTLD_LAZY don't matter.
         Found thanks to mentions in
         http://grokbase.com/t/gg/android-ndk/133mh6mk8b/unable-to-dlopen-libtest-so-cannot-load-library-link-image-1995-failed-to-link-libtest-so }
       {$ifdef ANDROID} RTLD_NOW {$else} RTLD_LAZY or RTLD_GLOBAL {$endif}) );
     {$else} LoadLibrary(AName);
     {$endif}
  end;

var Handle: TDynLibHandle;
begin
 Handle := LoadLibraryGlobally(PChar(AName));
 if Handle = InvalidDynLibHandle then
 begin
  if RaiseExceptionOnError then
   raise EDynLibError.Create('Can''t load library "' +AName+ '"'
     {$ifdef UNIX} + ': ' + dlerror {$endif}) else
   result := nil;
 end else
  result := Self.Create(AName, Handle);
end;

function TDynLib.Symbol(SymbolName: PChar): Pointer;

  function ErrStr: string;
  begin result := 'Symbol "'+SymbolName+'" not found in library "'+Name+'"' end;

begin
 result:= {$ifdef FPC} GetProcedureAddress {$else} GetProcAddress {$endif}
   (FHandle, SymbolName);
 if result = nil then
  case SymbolErrorBehaviour of
   seRaise: raise EDynLibError.Create(ErrStr);
   seReturnNil: ;
   seWarnAndReturnNil: WarningWrite(ErrStr);
   else raise EInternalError.Create('SymbolErrorBehaviour=?');
  end;
end;

end.
