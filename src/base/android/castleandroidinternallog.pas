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

{ Like AndroidLog, but works better for long strings (> 4076 characters),
  otherwise the default AndroidLog seems to cut them off. }
procedure AndroidLogRobust(const Priority: TAndroidLogPriority; const S: string);

implementation

uses CTypes, SysUtils, CastleUtils;

const
  AndroidLogLib = 'liblog.so';

function __android_log_write(prio: CInt; tag, text: PChar): CInt; cdecl;
  external AndroidLogLib;

var
  LogTag: string;

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string);
const
  MaxAndroidTagLength = 23;
begin
  if LogTag = '' then
    LogTag :=
      {$ifdef CASTLE_ANDROID_ARGV_LOGGING}
      'eye_of_beholder'; // hardcode for this test
      {$else}
      Copy(ApplicationName, 1, MaxAndroidTagLength);
      {$endif}
  __android_log_write(Ord(Priority), PChar(LogTag), PChar(S));
end;

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string; const Args: array of const);
begin
  AndroidLog(Priority, Format(S, Args));
end;

procedure AndroidLogRobust(const Priority: TAndroidLogPriority; const S: string);
var
  I: Integer;
const
  { The limit is actually 4076,
    see http://jhshi.me/2014/06/30/stop-android-logcat-from-truncating-log-line/index.html .
    It's safer to cut off earlier. }
  MaxChunkLength = 4000;
begin
  I := 1;
  while I <= Length(S) do
  begin
    AndroidLog(Priority, Copy(S, I, MaxChunkLength));
    I += MaxChunkLength;
  end;
end;

end.
