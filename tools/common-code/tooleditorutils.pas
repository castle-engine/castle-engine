{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Utils shared by castle-editor and castle-editor-portable. }
unit ToolEditorUtils;

interface

{ Show last file modification time as nice string. }
function FileDateTimeStr(const FileName: String): String;

{ Show last URL modification time as nice string.
  Returns empty string if URL is not a file. }
function UrlDateTimeStr(const Url: String): String;

implementation

uses DateUtils, SysUtils, StrUtils,
  CastleTimeUtils, CastleUtils, CastleUriUtils;

function FileDateTimeStr(const FileName: String): String;

  function RoundUp(const Val: Double): Int64;
  begin
    Result := Trunc(Val);
    if Frac(Val) > 0 then
      Inc(Result);
  end;

var
  FileDateTime: TDateTime;
  Secs, Mins: Int64;
begin
  if FileAge(FileName, FileDateTime) then
  begin
    Secs := RoundUp(SecondSpan(Now, FileDateTime));
    if Secs < 60 then
      FileDateTimeStr := Format('%d second%s ago', [
        Secs,
        Iff(Secs > 0, 's', '')
      ])
    else
    begin
      Mins := RoundUp(MinuteSpan(Now, FileDateTime));
      if Mins < 60 then
        FileDateTimeStr := Format('%d minute%s ago', [
          Mins,
          Iff(Mins > 0, 's', '')
        ])
      else
        FileDateTimeStr := DateTimeToAtStr(FileDateTime);
    end;
  end else
    Result := 'Unknown';
end;

function UrlDateTimeStr(const Url: String): String;
var
  FileName: String;
begin
  FileName := UriToFilenameSafe(Url);
  if FileName = '' then
    Result := ''
  else
    Result := FileDateTimeStr(FileName);
end;

end.