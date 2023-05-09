{%MainUnit x3dfields.pas}
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
    Indent: string;
    DoDiscardNextIndent: boolean;
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

    { Comfortable routines that simply write given string to a stream.
      @groupBegin }
    procedure Write(const S: string);
    procedure Writeln(const S: string); overload;
    procedure Writeln; overload;
    procedure WriteIndent(const S: string);
    procedure WritelnIndent(const S: string);
    { @groupEnd }

    { Causes next WriteIndent or WritelnIndent too not write the Indent.
      Useful in some cases to improve readability of generated VRML file. }
    procedure DiscardNextIndent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DWriter ----------------------------------------------------------------- }

const
  { IndentIncrement is string or char. It's used by SaveToStream }
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

procedure TX3DWriter.Write(const S: string);
begin
  WriteStr(FStream, S);
end;

procedure TX3DWriter.Writeln(const S: string);
begin
  WriteStr(FStream, S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.Writeln;
begin
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.WriteIndent(const S: string);
begin
  if DoDiscardNextIndent then
    DoDiscardNextIndent := false else
    WriteStr(FStream, Indent);
  WriteStr(FStream, S);
end;

procedure TX3DWriter.WritelnIndent(const S: string);
begin
  WriteIndent(S);
  WriteStr(FStream, NL);
end;

procedure TX3DWriter.DiscardNextIndent;
begin
  DoDiscardNextIndent := true;
end;

{$endif read_implementation}
