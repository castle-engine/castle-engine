{%MainUnit castlerendererinternalshader.pas}
{
  Copyright 2010-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Maintain a hash of the shader code. }

{$ifdef read_interface}

//type
  { Hash of the shader code.
    Different hash -> different shader code, must link a new shader program.
    Same hash -> can reuse the shader program with the same hash. }
  TShaderCodeHash = record
  strict private
    Sum, XorValue: UInt32;
  public
    { Adds the string contents (as raw bytes, whatever the String encoding is)
      to the hash.
      Commented out, as unused now. }
    //procedure AddString(const S: String; const Multiplier: UInt32);
    procedure AddInteger(const I: Integer);
    procedure AddFloat(const F: Single; const UniquePrimeNumber: Cardinal);

    { Add pointer (reference) to the hash.

      Multiplier allows to distinguish the same pointer used in different roles.
      E.g. pass "light number + 1" when adding something specific to
      a given light source. Must be non-zero. }
    procedure AddPointer(Ptr: Pointer; const Multiplier: UInt32 = 1);

    { Add enabled Effect nodes to the hash.

      Multiplier allows to distinguish the same effects used at different
      "slots", when the generated shader code depends on the slot.
      E.g. effects on a light source should pass "light number + 1",
      effects on a texture should pass "texture unit + 1".
      This way the same Effect node used on a different light number
      (or texture unit) results in a different hash.
      Must be non-zero. }
    procedure AddEffects(Nodes: TX3DNodeList; const Multiplier: UInt32 = 1); overload;
    procedure AddEffects(Nodes: TMFNode; const Multiplier: UInt32 = 1); overload;

    function ToString: String;
    procedure Clear;

    class operator {$ifdef FPC}={$else}Equal{$endif} (const A, B: TShaderCodeHash): boolean;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TShaderCodeHash ------------------------------------------------------------ }

{$include norqcheckbegin.inc}

(*
procedure TShaderCodeHash.AddString(const S: String; const Multiplier: UInt32);
var
  PS: PUInt32;
  Last: UInt32;
  I, ByteCount: Integer;
begin
  { Note: Count bytes, not characters. They differ when String is 16-bit
    (UnicodeString, with Delphi or FPC Unicode RTL).
    We just hash the raw bytes of the string, whatever its encoding is. }
  ByteCount := Length(S) * SizeOf(Char);

  PS := PUInt32(Pointer(S));
  for I := 1 to ByteCount div 4 do
  begin
    Sum := Sum + PS^ * Multiplier;
    XorValue := XorValue xor PS^;
    Inc(PS);
  end;

  if ByteCount mod 4 <> 0 then
  begin
    Last := 0;
    Move(PS^, Last, ByteCount mod 4);
    Sum := Sum + Last * Multiplier;
    XorValue := XorValue xor Last;
  end;
end;
*)

procedure TShaderCodeHash.AddPointer(Ptr: Pointer; const Multiplier: UInt32);
var
  Value: UInt32;
begin
  Assert(Multiplier <> 0);
  { This will cut the pointer on non-32bit processors.
    But that's not a problem --- we just want it for hash,
    taking the least significant 32 bits from pointer is OK for this. }
  Value := UInt32(PtrUInt(Ptr)) * Multiplier;
  Sum := Sum + Value;
  XorValue := XorValue xor Value;
end;

procedure TShaderCodeHash.AddInteger(const I: Integer);
begin
  Sum := Sum + I;
end;

procedure TShaderCodeHash.AddFloat(const F: Single; const UniquePrimeNumber: Cardinal);
begin
  Sum := Sum + (Round(F * 1000) + 1) * UniquePrimeNumber;
end;

{$include norqcheckend.inc}

procedure TShaderCodeHash.AddEffects(Nodes: TX3DNodeList; const Multiplier: UInt32);
var
  I: Integer;
begin
  { We add to hash actual Effect node references (pointers), this way ensuring
    that to share the same shader, effect nodes must be the same.
    Merely equal GLSL source code is not enough (because effects with equal
    source code may still have different uniform values, and sharing them
    would not be handled correctly here --- we set uniform values on change,
    not every time before rendering shape). }
  for I := 0 to Nodes.Count - 1 do
    if (Nodes[I] is TEffectNode) and
       TEffectNode(Nodes[I]).FdEnabled.Value then
      AddPointer(Nodes[I], Multiplier);
end;

procedure TShaderCodeHash.AddEffects(Nodes: TMFNode; const Multiplier: UInt32);
var
  I: Integer;
begin
  for I := 0 to Nodes.Count - 1 do
    if (Nodes[I] is TEffectNode) and
       TEffectNode(Nodes[I]).FdEnabled.Value then
      AddPointer(Nodes[I], Multiplier);
end;

function TShaderCodeHash.ToString: String;
begin
  Result := IntToStr(Sum) + '/' + IntToStr(XorValue);
end;

procedure TShaderCodeHash.Clear;
begin
  Sum := 0;
  XorValue := 0;
end;

class operator TShaderCodeHash.{$ifdef  FPC}={$else}Equal{$endif}(const A, B: TShaderCodeHash): boolean;
begin
  Result := (A.Sum = B.Sum) and (A.XorValue = B.XorValue);
end;

{$endif read_implementation}
