{ -*- buffer-read-only: t -*-

  Copyright 2015-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated X3D node class helpers.
  Do not edit manually, instead regenerate using
  castle_game_engine/src/x3d/doc/generate_x3d_nodes_to_pascal.lpr . }

{$ifdef read_interface}

  private
    function GetDescription: string;
    procedure SetDescription(const Value: string);
    function GetPitch: Single;
    procedure SetPitch(const Value: Single);
  public
    property Description: string read GetDescription write SetDescription;
    property Pitch: Single read GetPitch write SetPitch;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractSoundSourceNode ----------------------------------------------- }

function TAbstractSoundSourceNode.GetDescription: string;
begin
  Result := FdDescription.Value;
end;

procedure TAbstractSoundSourceNode.SetDescription(const Value: string);
begin
  FdDescription.Send(Value);
end;

function TAbstractSoundSourceNode.GetPitch: Single;
begin
  Result := FdPitch.Value;
end;

procedure TAbstractSoundSourceNode.SetPitch(const Value: Single);
begin
  FdPitch.Send(Value);
end;

{$endif read_implementation}
