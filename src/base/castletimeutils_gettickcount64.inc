{ Windows -------------------------------------------------------------------- }

{$ifdef MSWINDOWS}
{ GetTickCount64 for Windows, from fpc/3.0.0/src/rtl/win/sysutils.pp }

{$IFNDEF WINCE}
type
  TGetTickCount64 = function : QWord; stdcall;

var
  WinGetTickCount64: TGetTickCount64 = Nil;
{$ENDIF}

function CastleGetTickCount64: QWord;
{$IFNDEF WINCE}
var
  lib: THandle;
{$ENDIF}
begin
{$IFNDEF WINCE}
  { on Vista and newer there is a GetTickCount64 implementation }
  if Win32MajorVersion >= 6 then begin
    if not Assigned(WinGetTickCount64) then begin
      lib := LoadLibrary('kernel32.dll');
      WinGetTickCount64 := TGetTickCount64(
                             GetProcAddress(lib, 'GetTickCount64'));
    end;
    Result := WinGetTickCount64();
  end else
{$ENDIF}
    Result := Windows.GetTickCount;
end;
{$endif MSWINDOWS}

{ Nintendo Switch ------------------------------------------------------------ }

{$ifdef CASTLE_NINTENDO_SWITCH}
function CgeNxGetTickCount64(): QWord; cdecl; external;

function CastleGetTickCount64: TMilisecTime;
begin
  Result := CgeNxGetTickCount64;
end;
{$endif}

{ Unix (except Nintendo Switch) ---------------------------------------------- }

{$if defined(UNIX) and not defined(CASTLE_NINTENDO_SWITCH)}

{ GetTickCount64 for Unix.
  Not based on, but in fact very similar idea as the one in
  FPC fpc/3.0.0/src/rtl/unix/sysutils.pp }

var
  {$warnings off} // knowingly using deprecated stuff
  LastGetTickCount64: TMilisecTime;
  {$warnings on}

{$I norqcheckbegin.inc}
function CastleGetTickCount64: TMilisecTime;
var
  timeval: TTimeVal;
begin
  FpGettimeofday(@timeval, nil);

  { By doing tv_sec * 1000, we reject 3 most significant digits from tv_sec.
    That's Ok, since these digits change least often.
    And this way we get the 3 least significant digits to fill
    with tv_usec div 1000 (which must be < 1000, because tv_usec must be < 1 million). }

  Result := QWord(timeval.tv_sec) * 1000 + (timeval.tv_usec div 1000);

  { We cannot trust some Android systems to return increasing values here
    (Android device "Moto X Play", "XT1562", OS version 5.1.1).
    Maybe they synchronize the time from the Internet, and do not take care
    to keep it monotonic (unlike https://lwn.net/Articles/23313/ says?) }

  if Result < LastGetTickCount64 then
  begin
    WritelnLog('Time', 'Detected gettimeofday() going backwards on Unix, workarounding. This is known to happen on some Android devices');
    Result := LastGetTickCount64;
  end else
    LastGetTickCount64 := Result;
end;
{$I norqcheckend.inc}
{$endif UNIX}
