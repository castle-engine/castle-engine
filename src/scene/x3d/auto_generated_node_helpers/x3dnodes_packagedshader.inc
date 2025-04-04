{ -*- buffer-read-only: t -*-

  Copyright 2015-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal }

{$ifdef read_interface}

  public
    { Create node fields and events. }
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdUrl: TMFString;
    { Internal wrapper for property @code(Url). This wrapper API may change, we advise to access simpler @code(Url) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdUrl: TMFString read FFdUrl;
    {  }
    procedure SetUrl(const Value: array of string); overload;
    {  }
    procedure SetUrl(const Value: TCastleStringList); overload;

{$endif read_interface}

{$ifdef read_implementation}

{ TPackagedShaderNode ----------------------------------------------- }

procedure TPackagedShaderNode.SetUrl(const Value: array of string);
begin
  FdUrl.Send(Value);
end;

procedure TPackagedShaderNode.SetUrl(const Value: TCastleStringList);
begin
  FdUrl.Send(Value);
end;

class function TPackagedShaderNode.ClassX3DType: String;
begin
  Result := 'PackagedShader';
end;

procedure TPackagedShaderNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, True, 'url', []);
   FdUrl.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdUrl);
end;

{$endif read_implementation}
