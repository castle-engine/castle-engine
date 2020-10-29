{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Delphi XML reading routines.
  This unit is compatible with a subset of FPC XMLRead unit. }
unit XMLRead;

{$I castleconf.inc}

interface

uses SysUtils, Classes, XMLDoc,
  DOM;

type
  EXMLReadError = class(Exception);

procedure ReadXMLFile(out Doc: TXMLDocument; const Stream: TStream);

implementation


procedure ReadXMLFile(out Doc: TXMLDocument; const Stream: TStream);
begin
  Doc := TXMLDocument.Create;
  Doc.InternalDocument := XMLDoc.TXMLDocument.Create(nil);
  try
    Doc.InternalDocument.LoadFromStream(Stream);
  except
    on E: Exception do
      raise EXMLReadError.Create('Error when reading XML: ' + E.Message);
  end;
end;

end.
