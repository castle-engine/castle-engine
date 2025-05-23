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

    strict private FFdControlPoint: TMFVec2d;
    { Internal wrapper for property @code(ControlPoint). This wrapper API may change, we advise to access simpler @code(ControlPoint) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdControlPoint: TMFVec2d read FFdControlPoint;
    {  }
    procedure SetControlPoint(const Value: array of TVector2Double); overload;
    {  }
    procedure SetControlPoint(const Value: TVector2DoubleList); overload;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractNurbsControlCurveNode ----------------------------------------------- }

procedure TAbstractNurbsControlCurveNode.SetControlPoint(const Value: array of TVector2Double);
begin
  FdControlPoint.Send(Value);
end;

procedure TAbstractNurbsControlCurveNode.SetControlPoint(const Value: TVector2DoubleList);
begin
  FdControlPoint.Send(Value);
end;

class function TAbstractNurbsControlCurveNode.ClassX3DType: String;
begin
  Result := 'X3DNurbsControlCurveNode';
end;

procedure TAbstractNurbsControlCurveNode.CreateNode;
begin
  inherited;

  FFdControlPoint := TMFVec2d.Create(Self, True, 'controlPoint', []);
   FdControlPoint.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdControlPoint);

  DefaultContainerField := 'children';
end;

{$endif read_implementation}
