{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  TX3DEvent = class;

  { Writer of VRML/X3D to stream. }
  TX3DWriter = class abstract
  strict private
    Indent: String;
    DoDiscardNextIndent: Boolean;
    FEncoding: TX3DEncoding;
    FStream: TStream;
  public
    { Which VRML/X3D version are we writing. Read-only after creation. }
    Version: TX3DVersion;

    constructor Create(AStream: TStream;
      const AVersion: TX3DVersion; const AEncoding: TX3DEncoding);
    destructor Destroy; override;

    property Encoding: TX3DEncoding read FEncoding;

    procedure IncIndent;
    procedure DecIndent;

    { Comfortable routines that simply write given String to a stream.
      @groupBegin }
    procedure Write(const S: String);
    procedure Writeln(const S: String); overload;
    procedure Writeln; overload;
    procedure WriteIndent(const S: String);
    procedure WritelnIndent(const S: String);
    { @groupEnd }

    { Causes next WriteIndent or WritelnIndent too not write the Indent.
      Useful in some cases to improve readability of generated VRML file. }
    procedure DiscardNextIndent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DWriter ----------------------------------------------------------------- }

const
  { IndentIncrement is String or Char. It's used by SaveToStream }
  IndentIncrement = CharTab;

constructor TX3DWriter.Create(AStream: TStream; const AVersion: TX3DVersion;
  const AEncoding: TX3DEncoding);
begin
  inherited Create;
  Version := AVersion;
  FStream := AStream;
  FEncoding := AEncoding;
end;

destructor TX3DWriter.Destroy;
begin
  inherited;
end;

procedure TX3DWriter.IncIndent;
var
  L: Integer;
begin
  L := Length(Indent) + 1;
  SetLength(Indent, L);
  Indent[L] := IndentIncrement;
end;

procedure TX3DWriter.DecIndent;
begin
  SetLength(Indent, Length(Indent) - 1);
end;

procedure TX3DWriter.Write(const S: String);
begin
  WriteStr(FStream, S);
end;

procedure TX3DWriter.Writeln(const S: String);
begin
  WriteStr(FStream, S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.Writeln;
begin
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.WriteIndent(const S: String);
begin
  if DoDiscardNextIndent then
    DoDiscardNextIndent := false else
    WriteStr(FStream, Indent);
  WriteStr(FStream, S);
end;

procedure TX3DWriter.WritelnIndent(const S: String);
begin
  WriteIndent(S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.DiscardNextIndent;
begin
  DoDiscardNextIndent := true;
end;

{$endif read_implementation}
