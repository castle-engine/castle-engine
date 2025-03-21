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
    function GetPickable: Boolean;
    procedure SetPickable(const Value: Boolean);
  public
    { Create node fields and events. }
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdObjectType: TMFString;
    { Internal wrapper for property @code(ObjectType). This wrapper API may change, we advise to access simpler @code(ObjectType) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdObjectType: TMFString read FFdObjectType;
    {  }
    procedure SetObjectType(const Value: array of string); overload;
    {  }
    procedure SetObjectType(const Value: TCastleStringList); overload;

    strict private FFdPickable: TSFBool;
    { Internal wrapper for property @code(Pickable). This wrapper API may change, we advise to access simpler @code(Pickable) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdPickable: TSFBool read FFdPickable;
    {  }
    property Pickable: Boolean read GetPickable write SetPickable;

{$endif read_interface}

{$ifdef read_implementation}

{ TPickableGroupNode ----------------------------------------------- }

procedure TPickableGroupNode.SetObjectType(const Value: array of string);
begin
  FdObjectType.Send(Value);
end;

procedure TPickableGroupNode.SetObjectType(const Value: TCastleStringList);
begin
  FdObjectType.Send(Value);
end;

function TPickableGroupNode.GetPickable: Boolean;
begin
  Result := FdPickable.Value;
end;

procedure TPickableGroupNode.SetPickable(const Value: Boolean);
begin
  FdPickable.Send(Value);
end;

class function TPickableGroupNode.ClassX3DType: String;
begin
  Result := 'PickableGroup';
end;

procedure TPickableGroupNode.CreateNode;
begin
  inherited;

  FFdObjectType := TMFString.Create(Self, True, 'objectType', ['ALL']);
   FdObjectType.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdObjectType);

  FFdPickable := TSFBool.Create(Self, True, 'pickable', true);
   FdPickable.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdPickable);
end;

{$endif read_implementation}
