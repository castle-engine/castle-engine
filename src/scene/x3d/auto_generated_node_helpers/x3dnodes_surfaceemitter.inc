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

  strict private
    function GetSurface: TAbstractGeometryNode;
    procedure SetSurface(const Value: TAbstractGeometryNode);
  public
    { Create node fields and events. }
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FEventSet_coordIndex: TMFInt32Event;
    {  }
    public property EventSet_coordIndex: TMFInt32Event read FEventSet_coordIndex;

    strict private FFdCoordIndex: TMFInt32;
    { Internal wrapper for property @code(CoordIndex). This wrapper API may change, we advise to access simpler @code(CoordIndex) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdCoordIndex: TMFInt32 read FFdCoordIndex;
    {  }
    procedure SetCoordIndex(const Value: array of Int32); overload;
    {  }
    procedure SetCoordIndex(const Value: TInt32List); overload;

    strict private FFdSurface: TSFNode;
    { Internal wrapper for property @code(Surface). This wrapper API may change, we advise to access simpler @code(Surface) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdSurface: TSFNode read FFdSurface;
    {  }
    property Surface: TAbstractGeometryNode read GetSurface write SetSurface;

{$endif read_interface}

{$ifdef read_implementation}

{ TSurfaceEmitterNode ----------------------------------------------- }

procedure TSurfaceEmitterNode.SetCoordIndex(const Value: array of Int32);
begin
  FdCoordIndex.Send(Value);
end;

procedure TSurfaceEmitterNode.SetCoordIndex(const Value: TInt32List);
begin
  FdCoordIndex.Send(Value);
end;

function TSurfaceEmitterNode.GetSurface: TAbstractGeometryNode;
begin
  if FdSurface.Value is TAbstractGeometryNode then
    Result := TAbstractGeometryNode(FdSurface.Value)
  else
    Result := nil;
end;

procedure TSurfaceEmitterNode.SetSurface(const Value: TAbstractGeometryNode);
begin
  FdSurface.Send(Value);
end;

class function TSurfaceEmitterNode.ClassX3DType: String;
begin
  Result := 'SurfaceEmitter';
end;

procedure TSurfaceEmitterNode.CreateNode;
begin
  inherited;

  FEventSet_coordIndex := TMFInt32Event.Create(Self, 'set_coordIndex', true);
  AddEvent(FEventSet_coordIndex);

  FFdCoordIndex := TMFInt32.Create(Self, False, 'coordIndex', [ -1]);
   FdCoordIndex.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdCoordIndex);

  FFdSurface := TSFNode.Create(Self, False, 'surface', [TAbstractGeometryNode]);
   FdSurface.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSurface);
end;

{$endif read_implementation}
