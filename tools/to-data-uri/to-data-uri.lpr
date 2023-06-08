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

{ Grab the data from command-line parameter,
  and output data URI encoding it. Useful to encode images, sounds,
  3D models, whatever as data URI.
  See http://en.wikipedia.org/wiki/Data_URI_scheme for description of data URIs.

  Command-line parameter is used with our Download routine, so:
  - it may be a filename
  - it may be an URL: a file URL, http URL (will be automatically downloaded)
    or even another data URI.

  Mime type (necessary to output nice data URI) is also detected by our
  Download routine. For http, it may be returned by http server.
  For file, it's guessed based on file extension.
  See documentation of CastleDownload.Download function for details. }

uses SysUtils, Classes, Base64, CastleParameters, CastleDownload,
  CastleURIUtils, CastleInternalDataUri, CastleClassUtils;
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
