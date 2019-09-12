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
  This unit is compatible with a subset of FPC XMLWrite unit. }
unit XMLWrite;

{$I castleconf.inc}

interface

uses DOM, Classes;

procedure WriteXMLFile(const Doc: TXMLDocument; const Stream: TStream);

implementation

procedure WriteXMLFile(const Doc: TXMLDocument; const Stream: TStream);
begin
  Doc.InternalDocument.SaveToStream(Stream);
end;

end.

