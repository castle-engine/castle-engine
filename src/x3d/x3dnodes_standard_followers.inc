{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractFollowerNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;

    { Event out } { }
    private FEventIsActive: TSFBoolEvent;
    public property EventIsActive: TSFBoolEvent read FEventIsActive;

    {$I auto_generated_node_helpers/x3dnodes_x3dfollowernode.inc}
  end;

  TAbstractDamperNode = class(TAbstractFollowerNode)
  public
    procedure CreateNode; override;

    private FFdTau: TSFTime;
    public property FdTau: TSFTime read FFdTau;

    private FFdTolerance: TSFFloat;
    public property FdTolerance: TSFFloat read FFdTolerance;

    private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;

    {$I auto_generated_node_helpers/x3dnodes_x3ddampernode.inc}
  end;

  TAbstractChaserNode = class(TAbstractFollowerNode)
  public
    procedure CreateNode; override;

    private FFdDuration: TSFTime;
    public property FdDuration: TSFTime read FFdDuration;

    {$I auto_generated_node_helpers/x3dnodes_x3dchasernode.inc}
  end;

  TColorDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFColorEvent;
    public property EventSet_destination: TSFColorEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFColorEvent;
    public property EventSet_value: TSFColorEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFColorEvent;
    public property EventValue_changed: TSFColorEvent read FEventValue_changed;

    private FFdInitialDestination: TSFColor;
    public property FdInitialDestination: TSFColor read FFdInitialDestination;

    private FFdInitialValue: TSFColor;
    public property FdInitialValue: TSFColor read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_colordamper.inc}
  end;

  TCoordinateDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TMFVec3fEvent;
    public property EventSet_destination: TMFVec3fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TMFVec3fEvent;
    public property EventSet_value: TMFVec3fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TMFVec3fEvent;
    public property EventValue_changed: TMFVec3fEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec3f;
    public property FdInitialDestination: TMFVec3f read FFdInitialDestination;

    private FFdInitialValue: TMFVec3f;
    public property FdInitialValue: TMFVec3f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_coordinatedamper.inc}
  end;

  TOrientationChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFRotationEvent;
    public property EventSet_destination: TSFRotationEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFRotationEvent;
    public property EventSet_value: TSFRotationEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_orientationchaser.inc}
  end;

  TOrientationDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFRotationEvent;
    public property EventSet_destination: TSFRotationEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFRotationEvent;
    public property EventSet_value: TSFRotationEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFRotationEvent;
    public property EventValue_changed: TSFRotationEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_orientationdamper.inc}
  end;

  TPositionChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFVec3fEvent;
    public property EventSet_destination: TSFVec3fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFVec3fEvent;
    public property EventSet_value: TSFVec3fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_positionchaser.inc}
  end;

  TPositionChaser2DNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFVec2fEvent;
    public property EventSet_destination: TSFVec2fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFVec2fEvent;
    public property EventSet_value: TSFVec2fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFVec2fEvent;
    public property EventValue_changed: TSFVec2fEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_positionchaser2d.inc}
  end;

  TPositionDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFVec3fEvent;
    public property EventSet_destination: TSFVec3fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFVec3fEvent;
    public property EventSet_value: TSFVec3fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFVec3fEvent;
    public property EventValue_changed: TSFVec3fEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_positiondamper.inc}
  end;

  TPositionDamper2DNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFVec2fEvent;
    public property EventSet_destination: TSFVec2fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFVec2fEvent;
    public property EventSet_value: TSFVec2fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFVec2fEvent;
    public property EventValue_changed: TSFVec2fEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_positiondamper2d.inc}
  end;

  TScalarChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TSFFloatEvent;
    public property EventSet_destination: TSFFloatEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TSFFloatEvent;
    public property EventSet_value: TSFFloatEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TSFFloatEvent;
    public property EventValue_changed: TSFFloatEvent read FEventValue_changed;

    private FFdInitialDestination: TSFFloat;
    public property FdInitialDestination: TSFFloat read FFdInitialDestination;

    private FFdInitialValue: TSFFloat;
    public property FdInitialValue: TSFFloat read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_scalarchaser.inc}
  end;

  TTexCoordDamper2DNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventSet_destination: TMFVec2fEvent;
    public property EventSet_destination: TMFVec2fEvent read FEventSet_destination;

    { Event in } { }
    private FEventSet_value: TMFVec2fEvent;
    public property EventSet_value: TMFVec2fEvent read FEventSet_value;

    { Event out } { }
    private FEventValue_changed: TMFVec2fEvent;
    public property EventValue_changed: TMFVec2fEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec2f;
    public property FdInitialDestination: TMFVec2f read FFdInitialDestination;

    private FFdInitialValue: TMFVec2f;
    public property FdInitialValue: TMFVec2f read FFdInitialValue;

    {$I auto_generated_node_helpers/x3dnodes_texcoorddamper2d.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractFollowerNode.CreateNode;
begin
  inherited;

  FEventIsActive := TSFBoolEvent.Create(Self, 'isActive', false);
  AddEvent(FEventIsActive);

  DefaultContainerField := 'children';
end;

procedure TAbstractDamperNode.CreateNode;
begin
  inherited;

  FFdTau := TSFTime.Create(Self, 'tau', 0);
  AddField(FFdTau);
  { X3D specification comment: [0,Inf) }

  FFdTolerance := TSFFloat.Create(Self, 'tolerance', -1);
  AddField(FFdTolerance);
  { X3D specification comment: -1 or [0,Inf) }

  FFdOrder := TSFInt32.Create(Self, 'order', 0);
  FFdOrder.Exposed := false;
  AddField(FFdOrder);
  { X3D specification comment: [0..5] }

  DefaultContainerField := 'children';
end;

procedure TAbstractChaserNode.CreateNode;
begin
  inherited;

  FFdDuration := TSFTime.Create(Self, 'duration', 0);
  FFdDuration.Exposed := false;
  AddField(FFdDuration);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

procedure TColorDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFColorEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFColorEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFColorEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFColor.Create(Self, 'initialDestination', Vector3Single(0.8, 0.8, 0.8));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFColor.Create(Self, 'initialValue', Vector3Single(0.8, 0.8, 0.8));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TColorDamperNode.ClassX3DType: string;
begin
  Result := 'ColorDamper';
end;

class function TColorDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TCoordinateDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TMFVec3fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TMFVec3fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TMFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TMFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TMFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TCoordinateDamperNode.ClassX3DType: string;
begin
  Result := 'CoordinateDamper';
end;

class function TCoordinateDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TOrientationChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFRotationEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFRotationEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TOrientationChaserNode.ClassX3DType: string;
begin
  Result := 'OrientationChaser';
end;

class function TOrientationChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TOrientationDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFRotationEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFRotationEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFRotationEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TOrientationDamperNode.ClassX3DType: string;
begin
  Result := 'OrientationDamper';
end;

class function TOrientationDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFVec3fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFVec3fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionChaserNode.ClassX3DType: string;
begin
  Result := 'PositionChaser';
end;

class function TPositionChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionChaser2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFVec2fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFVec2fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);
  { X3D specification comment: 0 }

  FFdInitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);
  { X3D specification comment: 0 }

  DefaultContainerField := 'children';
end;

class function TPositionChaser2DNode.ClassX3DType: string;
begin
  Result := 'PositionChaser2D';
end;

class function TPositionChaser2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFVec3fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFVec3fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFVec3fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionDamperNode.ClassX3DType: string;
begin
  Result := 'PositionDamper';
end;

class function TPositionDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TPositionDamper2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFVec2fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFVec2fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionDamper2DNode.ClassX3DType: string;
begin
  Result := 'PositionDamper2D';
end;

class function TPositionDamper2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TScalarChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TSFFloatEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TSFFloatEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TSFFloatEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TSFFloat.Create(Self, 'initialDestination', 0);
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TSFFloat.Create(Self, 'initialValue', 0);
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TScalarChaserNode.ClassX3DType: string;
begin
  Result := 'ScalarChaser';
end;

class function TScalarChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TTexCoordDamper2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TMFVec2fEvent.Create(Self, 'set_destination', true);
  AddEvent(FEventSet_destination);

  FEventSet_value := TMFVec2fEvent.Create(Self, 'set_value', true);
  AddEvent(FEventSet_value);

  FEventValue_changed := TMFVec2fEvent.Create(Self, 'value_changed', false);
  AddEvent(FEventValue_changed);

  FFdInitialDestination := TMFVec2f.Create(Self, 'initialDestination', []);
  FFdInitialDestination.Exposed := false;
  AddField(FFdInitialDestination);

  FFdInitialValue := TMFVec2f.Create(Self, 'initialValue', []);
  FFdInitialValue.Exposed := false;
  AddField(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TTexCoordDamper2DNode.ClassX3DType: string;
begin
  Result := 'TexCoordDamper2D';
end;

class function TTexCoordDamper2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterFollowersNodes;
begin
  NodesManager.RegisterNodeClasses([
    TColorDamperNode,
    TCoordinateDamperNode,
    TOrientationChaserNode,
    TOrientationDamperNode,
    TPositionChaserNode,
    TPositionChaser2DNode,
    TPositionDamperNode,
    TPositionDamper2DNode,
    TScalarChaserNode,
    TTexCoordDamper2DNode
  ]);
end;

{$endif read_implementation}
