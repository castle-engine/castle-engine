{
  Copyright 2013-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert any file to data URI scheme.
  See README.md for details. }

uses SysUtils, Classes, Base64, CastleParameters, CastleDownload,
  CastleUriUtils, CastleInternalDataUri, CastleClassUtils;
var
  MimeType: string;
  Stream: TStream;
begin
  EnableBlockingDownloads := true;
  Parameters.CheckHigh(1);
  Stream := Download(Parameters[1], [], MimeType);
  try
    { Note that MimeType may be empty if not recognized.
      That's Ok, data: URI spec allows it. }
    Writeln(StreamToDataUri(Stream, MimeType));
  finally
    FreeAndNil(Stream);
  end;
end.
