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
    function GetDirectOutput: boolean;
    procedure SetDirectOutput(const Value: boolean);
    function GetMustEvaluate: boolean;
    procedure SetMustEvaluate(const Value: boolean);
  public
    property DirectOutput: boolean read GetDirectOutput write SetDirectOutput;
    property MustEvaluate: boolean read GetMustEvaluate write SetMustEvaluate;

{$endif read_interface}

{$ifdef read_implementation}

{ TScriptNode ----------------------------------------------- }

function TScriptNode.GetDirectOutput: boolean;
begin
  Result := FdDirectOutput.Value;
end;

procedure TScriptNode.SetDirectOutput(const Value: boolean);
begin
  FdDirectOutput.Send(Value);
end;

function TScriptNode.GetMustEvaluate: boolean;
begin
  Result := FdMustEvaluate.Value;
end;

procedure TScriptNode.SetMustEvaluate(const Value: boolean);
begin
  FdMustEvaluate.Send(Value);
end;

{$endif read_implementation}
