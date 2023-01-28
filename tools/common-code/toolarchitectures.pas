{
  Copyright 2014-2023 Michalis Kamburelis and FPC team.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.
  Parts of this file are based on FPC packages/fpmkunit/src/fpmkunit.pp unit,
  which conveniently uses *exactly* the same license as Castle Game Engine.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Architectures (OS and CPU) definitions.
  Parts of this are based on FPMkUnit code, this way we use the same names
  as FPMkUnit, so we're consistent with FPC and fpmake command-line options. }
unit ToolArchitectures;

interface

type
  { Target of the compilation, which may include many OS/CPU combinations. }
  TTarget = (
    { Target a specific chosen OS/CPU. }
    targetCustom,
    { Target all relevant iOS combinations of OS/CPU. }
    targetIOS,
    { Target all relevant Android combinations of OS/CPU. }
    targetAndroid,
    { Build an application for Nintendo Switch. }
    targetNintendoSwitch
  );

  { Processor architectures supported by FPC. Copied from FPMkUnit. }
  TCpu=(cpuNone,
    i386,m68k,powerpc,sparc,x86_64,arm,powerpc64,avr,armeb,
    mips,mipsel,jvm,i8086,aarch64,sparc64
  );
  TCPUS = Set of TCPU;

  { Operating systems supported by FPC. Copied from FPMkUnit. }
  TOS=(osNone,
    linux,go32v2,win32,os2,freebsd,beos,netbsd,
    amiga,atari, solaris, qnx, netware, openbsd,wdosx,
    palmos,macosclassic,darwin,emx,watcom,morphos,netwlibc,
    win64,wince,gba,nds,embedded,symbian,haiku,iphonesim,
    aix,java,android,nativent,msdos,wii,aros,dragonfly,
    win16,ios
  );
  TOSes = Set of TOS;

Const
  // Aliases
  Amd64   = X86_64;
  PPC = PowerPC;
  PPC64 = PowerPC64;
  DOS = Go32v2;
  MacOSX = Darwin;

  AllOSes = [Low(TOS)..High(TOS)];
  AllCPUs = [Low(TCPU)..High(TCPU)];
  AllUnixOSes  = [Linux,FreeBSD,NetBSD,OpenBSD,Darwin,QNX,BeOS,Solaris,Haiku,iphonesim,aix,Android,dragonfly];
  AllBSDOSes      = [FreeBSD,NetBSD,OpenBSD,Darwin,iphonesim,dragonfly];
  AllWindowsOSes  = [Win32,Win64,WinCE];
  AllAmigaLikeOSes = [Amiga,MorphOS,AROS];
  AllLimit83fsOses = [go32v2,os2,emx,watcom,msdos,win16,atari];

  AllSmartLinkLibraryOSes = [Linux,msdos,win16,palmos]; // OSes that use .a library files for smart-linking
  AllImportLibraryOSes = AllWindowsOSes + [os2,emx,netwlibc,netware,watcom,go32v2,macosclassic,nativent,msdos,win16];

  { This table is kept OS,Cpu because it is easier to maintain (PFV) }
  OSCPUSupported : array[TOS,TCpu] of boolean = (
    { os          none   i386    m68k  ppc    sparc  x86_64 arm    ppc64  avr    armeb  mips   mipsel jvm    i8086 aarch64 sparc64}
    { none }    ( false, false, false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { linux }   ( false, true,  true,  true,  true,  true,  true,  true,  false, true , true , true , false, false, true , true ),
    { go32v2 }  ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { win32 }   ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { os2 }     ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { freebsd } ( false, true,  true,  false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { beos }    ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { netbsd }  ( false, true,  true,  true,  true,  true,  true,  false, false, false, false, false, false, false, false, false),
    { amiga }   ( false, false, true,  true,  false, false, false, false, false, false, false, false, false, false, false, false),
    { atari }   ( false, false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false),
    { solaris } ( false, true,  false, false, true,  true,  false, false, false, false, false, false, false, false, false, false),
    { qnx }     ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { netware } ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { openbsd } ( false, true,  true,  false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { wdosx }   ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { palmos }  ( false, false, true,  false, false, false, true,  false, false, false, false, false, false, false, false, false),
{ macosclassic }( false, false, true,  true,  false, false, false, false, false, false, false, false, false, false, false, false),
    { darwin }  ( false, true,  false, true,  false, true,  false, true,  false, false, false, false, false, false, true , false),
    { emx }     ( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { watcom }  ( false, true,  false, false, false ,false, false, false, false, false, false, false, false, false, false, false),
    { morphos } ( false, false, false, true,  false ,false, false, false, false, false, false, false, false, false, false, false),
    { netwlibc }( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { win64   } ( false, false, false, false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { wince    }( false, true,  false, false, false, false, true,  false, false, false, false, false, false, false, false, false),
    { gba    }  ( false, false, false, false, false, false, true,  false, false, false, false, false, false, false, false, false),
    { nds    }  ( false, false, false, false, false, false, true,  false, false, false, false, false, false, false, false, false),
    { embedded }( false, true,  true,  true,  true,  true,  true,  true,  true,  true , false, true , false, true , false, false),
    { symbian } ( false, true,  false, false, false, false, true,  false, false, false, false, false, false, false, false, false),
    { haiku }   ( false, true,  false, false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { iphonesim}( false, true,  false, false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { aix    }  ( false, false, false, true,  false, false, false, true,  false, false, false, false, false, false, false, false),
    { java }    ( false, false, false, false, false, false, false, false, false, false, false, false, true , false, false, false),
    { android } ( false, true,  false, false, false, true,  true,  false, false, false, false, true,  true , false, true,  false),
    { nativent }( false, true,  false, false, false, false, false, false, false, false, false, false, false, false, false, false),
    { msdos }   ( false, false, false, false, false, false, false, false, false, false, false, false, false, true , false, false),
    { wii }     ( false, false, false, true , false, false, false, false, false, false, false, false, false, false, false, false),
    { aros }    ( false, true,  false, false, false, true,  true,  false, false, false, false, false, false, false, false, false),
    { dragonfly}( false, false, false, false, false, true,  false, false, false, false, false, false, false, false, false, false),
    { win16 }   ( false, false, false, false, false, false, false, false, false, false, false, false, false, true , false, false),
    { ios }     ( false, false, false, false, false, false,  true, false, false, false, false, false, false, false, true , false)
  );

function TargetToString(const Target: TTarget): String;
function CPUToString(const CPU: TCPU): String;
function OSToString(const OS: TOS): String;
function TargetCompleteToString(const Target: TTarget; const OS: TOS; const CPU: TCPU): String;

function StringToTarget(const S: String): TTarget;
function StringToCPU(const S: String): TCPU;
function StringToOS(const S: String): TOS;

const
  DefaultCPU: TCPU =
    {$ifdef CPUi386} i386 {$endif}
    {$ifdef CPUm68k} m68k {$endif}
    {$ifdef CPUpowerpc32} powerpc {$endif}
    {$ifdef CPUsparc} sparc {$endif}
    {$ifdef CPUx86_64} x86_64 {$endif}
    {$ifdef CPUarm} arm {$endif}
    {$ifdef CPUaarch64} aarch64 {$endif}
    {$ifdef CPUpowerpc64} powerpc64 {$endif}
    {$ifdef CPUavr} avr {$endif}
    {$ifdef CPUarmeb} armeb {$endif}
    {$ifdef CPUmips} mips {$endif}
    {$ifdef CPUmipsel} mipsel {$endif}
    {$ifdef CPUjvm} jvm {$endif}
    {$ifdef CPUi8086} i8086 {$endif}
    {$ifdef CPUsparc64} sparc64 {$endif}
  ;

  DefaultOS: TOS =
    {$ifdef linux} linux {$endif}
    {$ifdef go32v2} go32v2 {$endif}
    {$ifdef win32} win32 {$endif}
    {$ifdef os2} os2 {$endif}
    {$ifdef freebsd} freebsd {$endif}
    {$ifdef beos} beos {$endif}
    {$ifdef netbsd} netbsd {$endif}
    {$ifdef amiga} amiga {$endif}
    {$ifdef atari} atari {$endif}
    {$ifdef solaris} solaris {$endif}
    {$ifdef qnx} qnx {$endif}
    {$ifdef netware} netware {$endif}
    {$ifdef openbsd} openbsd {$endif}
    {$ifdef wdosx} wdosx {$endif}
    {$ifdef palmos} palmos {$endif}
    {$ifdef macosclassic} macosclassic {$endif} // TODO: what is symbol of this? It used to be macos?
    {$ifdef darwin} darwin {$endif}
    {$ifdef emx} emx {$endif}
    {$ifdef watcom} watcom {$endif}
    {$ifdef morphos} morphos {$endif}
    {$ifdef netwlibc} netwlibc {$endif}
    {$ifdef win64} win64 {$endif}
    {$ifdef wince} wince {$endif}
    {$ifdef gba} gba {$endif}
    {$ifdef nds} nds {$endif}
    {$ifdef embedded} embedded {$endif}
    {$ifdef symbian} symbian {$endif}
    {$ifdef haiku} haiku {$endif}
    {$ifdef iphonesim} iphonesim {$endif}
    {$ifdef aix} aix {$endif}
    {$ifdef java} java {$endif}
    {$ifdef android} android {$endif}
    {$ifdef nativent} nativent {$endif}
    {$ifdef msdos} msdos {$endif}
    {$ifdef wii} wii {$endif}
  ;

function TargetOptionHelp: string;
function OSOptionHelp: string;
function CPUOptionHelp: string;

function ExeExtensionOS(const OS: TOS): string;

function LibraryExtensionOS(const OS: TOS; const Static: boolean = false): string;

implementation

uses TypInfo, SysUtils,
  CastleUtils, CastleParameters;

resourcestring
  SErrInvalidTarget     = 'Invalid target name "%s"';
  SErrInvalidCPU        = 'Invalid CPU name "%s"';
  SErrInvalidOS         = 'Invalid OS name "%s"';

function CPUToString(const CPU: TCPU): String;
begin
  Result := LowerCase(GetEnumName(TypeInfo(TCPU), Ord(CPU)));
end;

function OSToString(const OS: TOS): String;
begin
  Result := LowerCase(GetEnumName(TypeInfo(TOS), Ord(OS)));
end;

function StringToCPU(const S : String) : TCPU;
var
  I : Integer;
begin
  I := GetEnumValue(TypeInfo(TCPU), S);
  if I = -1 then
    raise Exception.CreateFmt(SErrInvalidCPU, [S]);
  Result := TCPU(I);
end;

function StringToOS(const S : String) : TOS;
var
  I : Integer;
begin
  I := GetEnumValue(TypeInfo(TOS), S);
  if I = -1 then
    raise Exception.CreateFmt(SErrInvalidOS, [S]);
  Result := TOS(I);
end;

const
  TargetNames: array [TTarget] of string = ('custom', 'ios', 'android', 'nintendo-switch');

function TargetToString(const Target : TTarget): string;
begin
  Result := TargetNames[Target];
end;

function StringToTarget(const S : String): TTarget;
var
  SLower: string;
begin
  SLower := LowerCase(S);
  for Result := Low(Result) to High(Result) do
    if SLower = TargetNames[Result] then
      Exit;
  raise Exception.CreateFmt(SErrInvalidTarget,[S]);
end;

function TargetCompleteToString(const Target: TTarget;
  const OS: TOS; const CPU: TCPU): String;
begin
  if Target = targetCustom then
    Result := Format('OS / CPU "%s / %s"', [OSToString(OS), CPUToString(CPU)])
  else
    Result := Format('target "%s"', [TargetToString(Target)]);
end;

function TargetOptionHelp: string;
begin
  Result := OptionDescription('--target=<target>',
    'The target system for which we build/package.' +NL+
    'Available <target> values: ' +NL+
    NL +
    '- "custom" (default): Build for a single OS and CPU combination, determined by the --os and --cpu options. These options, in turn, by default indicate the current (host) OS/CPU.' +NL+
    NL +
    '- "ios": Build for all the platforms necessary for iOS applications. This includes both 32-bit and 64-bit iOS devices and iPhoneSimulator.' +NL+
    NL +
    '- "android": Build for all the platforms necessary for Android applications. This includes both 32-bit and 64-bit Android devices.' +NL+
    NL +
    '- "nintendo-switch": Build an application for Nintendo Switch.' +NL+
    '');
end;

function CPUOptionHelp: string;
var
  CPU: TCPU;
  Description, Extra: String;
begin
  Description := 'Set the target processor for which we build/package.' +NL+
    'This is ignored if you used --target=<target>, with <target> being something else than "custom".' +NL+
    'Available <cpu> values: ' +NL;
  for CPU in TCPU do
    if CPU <> cpuNone then
    begin
      case CPU of
        aarch64: Extra := ' (64-bit ARM)';
        else Extra := '';
      end;
      Description += '  ' + CPUToString(CPU) + Extra + NL;
    end;
  Result := OptionDescription('--cpu=<cpu>', Description);
end;

function OSOptionHelp: string;
var
  OS: TOS;
  Description, Extra: String;
begin
  Description := 'Set the target operating system for which we build/package.' +NL+
    'This is ignored if you used --target=<target>, with <target> being something else than "custom".' +NL+
    'Available <os> values: ' +NL;
  for OS in TOS do
    if OS <> osNone then
    begin
      case OS of
        macosclassic: Extra := ' (classic MacOS, that ended with MacOS 9)';
        darwin: Extra := ' (modern macOS 10.x, caled also Mac OS X)';
        else Extra := '';
      end;
      Description += '  ' + OSToString(OS) + Extra + NL;
    end;
  Result := OptionDescription('--os=<os>', Description);
end;

function ExeExtensionOS(const OS: TOS): string;
begin
  if OS in AllWindowsOSes then
    Result :=  '.exe' else
    Result := '';
end;

function LibraryExtensionOS(const OS: TOS; const Static: boolean): string;
begin
  if Static then
    // Correct for Unix, not sure about Windows. This is used only for iOS now.
    Result :=  '.a' else
  if OS in AllWindowsOSes then
    Result :=  '.dll' else
  if OS in [Darwin,iphonesim] then
    Result := '.dylib' else
    Result := '.so';
end;

end.
