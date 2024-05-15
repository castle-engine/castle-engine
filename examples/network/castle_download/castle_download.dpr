{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Download an URL, write result on standard output (stdout).
  This is a simple test of TCastleDownload that allows you to grab any URL
  to a Stream.

  Writes diagnostic output to the log (see
  https://castle-engine.io/manual_log.php for information where is the log;
  note that we set LogEnableStandardOutput = false,
  which means that on Linux it is usually inside ~/.config/castle_download/ ).

  Try from command-line like
    castle_download https://castle-engine.io/ > output.html
    castle_download https://castle-engine.io/modern_pascal_introduction.html > output.html
    castle_download https://castle-engine.io/latest.zip > output.zip
}

uses
  {$ifdef UNIX} CThreads, {$endif} // necessary to have asynchronous downloading on Unix
  SysUtils, Classes, StrUtils, Math,
  {$ifdef FPC} OpenSSLSockets, {$endif} // support HTTPS
  CastleDownload, CastleParameters, CastleClassUtils, CastleLog, CastleUtils,
  CastleApplicationProperties, CastleStringUtils;

const
  MaxDots = 80;
var
  WrittenDots: Integer;
  WrittenTotalBytes: Boolean;

{ Makes a trivial "progress bar" using dots written on ErrOutput. }
procedure ReportProgress(const DownloadedBytes, TotalBytes: Int64);
const
  // In case size is unknown, how many bytes we need to write next dot.
  DotPerBytes = 1024 * 1024;
var
  NewWrittenDots: Integer;
begin
  if TotalBytes > 0 then
  begin
    if not WrittenTotalBytes then
    begin
      Writeln(ErrOutput, 'Need to download: ' + SizeToStr(TotalBytes));
      WrittenTotalBytes := true;
    end;

    NewWrittenDots := Max(WrittenDots, Round(MaxDots * DownloadedBytes / TotalBytes));
  end else
  begin
    // some requests never report TotalBytes, write some progress anyway
    NewWrittenDots := Max(WrittenDots, DownloadedBytes div DotPerBytes);

    if (WrittenDots = 0) and (NewWrittenDots > 0) then
    begin
      Writeln(ErrOutput, Format('Total download size unknown (not reported by HTTP server), we will report after each %d bytes (%s)', [
        DotPerBytes,
        SizeToStr(DotPerBytes)
      ]));
    end;
  end;

  Write(ErrOutput, DupeString('.', NewWrittenDots - WrittenDots));
  WrittenDots := NewWrittenDots;
end;

{ Main program code. }
var
  Download: TCastleDownload;
begin
  LogEnableStandardOutput := false; // do not put log in stdout, it would be mixed with downloaded output
  InitializeLog;
  Parameters.CheckHigh(1);

  Download := TCastleDownload.Create(nil);
  try
    Download.Url := Parameters[1];
    Download.HttpHeader('User-Agent', 'castle_download/1.0');
    Download.Start;

    while Download.Status = dsDownloading do
    begin
      ReportProgress(Download.DownloadedBytes, Download.TotalBytes);

      { In a normal GUI application, e.g. using TCastleWindow or TCastleControl,
        the update is done automatically.
        There's no need to call "ApplicationProperties._Update" explicitly,
        and no point in calling "Sleep" if you want to display some smooth animation
        while the download is ongoing.
        But it makes sense in this demo, where we only wait for the download to finish. }
      ApplicationProperties._Update;
      Sleep(100);
    end;

    case Download.Status of
      dsSuccess:
        ReadGrowingStream(Download.Contents, StdOutStream, false);
      dsError:
        Writeln(ErrOutput, Download.ErrorMessage);
      else
        raise EInternalError.Create('No other status is possible when download finished');
    end;
  finally FreeAndNil(Download) end;

  if WrittenDots <> 0 then
    Writeln(ErrOutput); // otherwise ErrOutput ends without newline and looks weird

  (* Note: Blocking version of this would be even simpler:

  EnableBlockingDownloads := true;
  Stream := Download(Parameters[1]);
  try
    ReadGrowingStream(Stream, StdOutStream, false);
  finally FreeAndNil(Stream) end;
  *)
end.
