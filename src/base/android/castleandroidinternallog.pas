{ Android logging facility.
  @exclude Internal for the engine. }
unit CastleAndroidInternalLog;

{$I castleconf.inc}

interface

{ Based on Android NDK platforms/android-4/arch-arm/usr/include/android/log.h .
  See also Lazarus' trunk/lcl/interfaces/customdrawn/android/log.pas . }

type
  TAndroidLogPriority = (
    alUnknown,
    alDefault,
    alVerbose,
    alDebug,
    alInfo,
    alWarn,
    alError,
    alFatal,
    alSilent
  );

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string);
procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string; const Args: array of const);

implementation

uses CTypes, SysUtils, CastleUtils;

const
  AndroidLogLib = 'liblog.so';

function __android_log_write(prio: CInt; tag, text: PChar): CInt; cdecl;
  external AndroidLogLib;

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string);
begin
  __android_log_write(Ord(Priority), PChar(ApplicationName), PChar(S));
end;

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string; const Args: array of const);
begin
  AndroidLog(Priority, Format(S, Args));
end;

end.
