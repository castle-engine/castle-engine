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

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: AnsiString); overload;
procedure AndroidLog(const Priority: TAndroidLogPriority; const S: string; const Args: array of const); overload;

{ Like AndroidLog, but works better for long strings (> 4076 characters),
  otherwise the default AndroidLog seems to cut them off. }
procedure AndroidLogRobust(const Priority: TAndroidLogPriority; const S: string);

implementation

uses {$ifdef FPC} CTypes, {$else} Androidapi.Log, {$endif}
  SysUtils, CastleUtils;

// Define __android_log_write for FPC.
// Delphi already defines it for Androidapi.Log.
{$ifdef FPC}
const
  AndroidLogLib = 'liblog.so';
function __android_log_write(prio: CInt; tag, text: PChar): CInt; cdecl;
  external AndroidLogLib;
{$endif}

var
  LogTag: AnsiString;

procedure AndroidLog(const Priority: TAndroidLogPriority; const S: AnsiString);
const
  MaxAndroidTagLength = 23;
  {$ifndef FPC}
  { Map our TAndroidLogPriority to Delphi's android_LogPriority.
    This could be actually also done by typecast, Delphi's
    android_LogPriority is passed directly to C API, just like our
    TAndroidLogPriority, so their ordinal values have to match exactly the C API.
    But better be safe. }
  PriorityToDelphi: array[TAndroidLogPriority] of android_LogPriority = (
    ANDROID_LOG_UNKNOWN,
    ANDROID_LOG_DEFAULT,
    ANDROID_LOG_VERBOSE,
    ANDROID_LOG_DEBUG,
    ANDROID_LOG_INFO,
    ANDROID_LOG_WARN,
    ANDROID_LOG_ERROR,
    ANDROID_LOG_FATAL,
    ANDROID_LOG_SILENT
 );
 {$endif}
begin
  if LogTag = '' then
    LogTag :=
      {$ifdef CASTLE_ANDROID_ARGV_LOGGING}
      'eye_of_beholder'; // hardcode for this test, this is not used in production ever
      {$else}
      Copy(ApplicationName, 1, MaxAndroidTagLength);
      {$endif}

  __android_log_write(
    {$ifdef FPC} Ord(Priority) {$else} PriorityToDelphi[Priority] {$endif},
    PAnsiChar(LogTag),
    PAnsiChar(S)
  );
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
    I := I + MaxChunkLength;
  end;
end;

end.
