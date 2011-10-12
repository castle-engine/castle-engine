{
  Copyright 2008-2011 Michalis Kamburelis.

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

    { Event: SFBool, out } { }
    private FEventIsActive: TX3DEvent;
    public property EventIsActive: TX3DEvent read FEventIsActive;
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
  end;

  TAbstractChaserNode = class(TAbstractFollowerNode)
  public
    procedure CreateNode; override;

    private FFdDuration: TSFTime;
    public property FdDuration: TSFTime read FFdDuration;
  end;

  TColorDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFColor, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFColor, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFColor, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFColor;
    public property FdInitialDestination: TSFColor read FFdInitialDestination;

    private FFdInitialValue: TSFColor;
    public property FdInitialValue: TSFColor read FFdInitialValue;
  end;

  TCoordinateDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: MFVec3f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: MFVec3f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec3f;
    public property FdInitialDestination: TMFVec3f read FFdInitialDestination;

    private FFdInitialValue: TMFVec3f;
    public property FdInitialValue: TMFVec3f read FFdInitialValue;
  end;

  TOrientationChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFRotation, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;
  end;

  TOrientationDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFRotation, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;
  end;

  TPositionChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFVec3f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;
  end;

  TPositionChaser2DNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFVec2f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;
  end;

  TPositionDamperNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFVec3f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;
  end;

  TPositionDamper2DNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFVec2f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;
  end;

  TScalarChaserNode = class(TAbstractChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: SFFloat, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: SFFloat, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TSFFloat;
    public property FdInitialDestination: TSFFloat read FFdInitialDestination;

    private FFdInitialValue: TSFFloat;
    public property FdInitialValue: TSFFloat read FFdInitialValue;
  end;

  TTexCoordDamper2DNode = class(TAbstractDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec2f, in } { }
    private FEventSet_destination: TX3DEvent;
    public property EventSet_destination: TX3DEvent read FEventSet_destination;

    { Event: MFVec2f, in } { }
    private FEventSet_value: TX3DEvent;
    public property EventSet_value: TX3DEvent read FEventSet_value;

    { Event: MFVec2f, out } { }
    private FEventValue_changed: TX3DEvent;
    public property EventValue_changed: TX3DEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec2f;
    public property FdInitialDestination: TMFVec2f read FFdInitialDestination;

    private FFdInitialValue: TMFVec2f;
    public property FdInitialValue: TMFVec2f read FFdInitialValue;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractFollowerNode.CreateNode;
begin
  inherited;

  FEventIsActive := TX3DEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventIsActive);

  DefaultContainerField := 'children';
end;

procedure TAbstractDamperNode.CreateNode;
begin
  inherited;

  FFdTau := TSFTime.Create(Self, 'tau', 0);
  Fields.Add(FFdTau);
  { X3D specification comment: [0,Inf) }

  FFdTolerance := TSFFloat.Create(Self, 'tolerance', -1);
  Fields.Add(FFdTolerance);
  { X3D specification comment: -1 or [0,Inf) }

  FFdOrder := TSFInt32.Create(Self, 'order', 0);
  FFdOrder.Exposed := false;
  Fields.Add(FFdOrder);
  { X3D specification comment: [0..5] }

  DefaultContainerField := 'children';
end;

procedure TAbstractChaserNode.CreateNode;
begin
  inherited;

  FFdDuration := TSFTime.Create(Self, 'duration', 0);
  FFdDuration.Exposed := false;
  Fields.Add(FFdDuration);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

procedure TColorDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFColor, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFColor, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFColor, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFColor.Create(Self, 'initialDestination', Vector3Single(0.8, 0.8, 0.8));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFColor.Create(Self, 'initialValue', Vector3Single(0.8, 0.8, 0.8));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TColorDamperNode.ClassNodeTypeName: string;
begin
  Result := 'ColorDamper';
end;

class function TColorDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TCoordinateDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TMFVec3f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TMFVec3f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TMFVec3f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TMFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TMFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TCoordinateDamperNode.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDamper';
end;

class function TCoordinateDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TOrientationChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFRotation, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFRotation, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TOrientationChaserNode.ClassNodeTypeName: string;
begin
  Result := 'OrientationChaser';
end;

class function TOrientationChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TOrientationDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFRotation, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFRotation, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TOrientationDamperNode.ClassNodeTypeName: string;
begin
  Result := 'OrientationDamper';
end;

class function TOrientationDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFVec3f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFVec3f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionChaserNode.ClassNodeTypeName: string;
begin
  Result := 'PositionChaser';
end;

class function TPositionChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionChaser2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFVec2f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFVec2f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);
  { X3D specification comment: 0 }

  FFdInitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);
  { X3D specification comment: 0 }

  DefaultContainerField := 'children';
end;

class function TPositionChaser2DNode.ClassNodeTypeName: string;
begin
  Result := 'PositionChaser2D';
end;

class function TPositionChaser2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionDamperNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFVec3f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFVec3f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionDamperNode.ClassNodeTypeName: string;
begin
  Result := 'PositionDamper';
end;

class function TPositionDamperNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TPositionDamper2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFVec2f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFVec2f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TPositionDamper2DNode.ClassNodeTypeName: string;
begin
  Result := 'PositionDamper2D';
end;

class function TPositionDamper2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TScalarChaserNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TSFFloat, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TSFFloat, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TSFFloat, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TSFFloat.Create(Self, 'initialDestination', 0);
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TSFFloat.Create(Self, 'initialValue', 0);
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TScalarChaserNode.ClassNodeTypeName: string;
begin
  Result := 'ScalarChaser';
end;

class function TScalarChaserNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TTexCoordDamper2DNode.CreateNode;
begin
  inherited;

  FEventSet_destination := TX3DEvent.Create(Self, 'set_destination', TMFVec2f, true);
  Events.Add(FEventSet_destination);

  FEventSet_value := TX3DEvent.Create(Self, 'set_value', TMFVec2f, true);
  Events.Add(FEventSet_value);

  FEventValue_changed := TX3DEvent.Create(Self, 'value_changed', TMFVec2f, false);
  Events.Add(FEventValue_changed);

  FFdInitialDestination := TMFVec2f.Create(Self, 'initialDestination', []);
  FFdInitialDestination.Exposed := false;
  Fields.Add(FFdInitialDestination);

  FFdInitialValue := TMFVec2f.Create(Self, 'initialValue', []);
  FFdInitialValue.Exposed := false;
  Fields.Add(FFdInitialValue);

  DefaultContainerField := 'children';
end;

class function TTexCoordDamper2DNode.ClassNodeTypeName: string;
begin
  Result := 'TexCoordDamper2D';
end;

class function TTexCoordDamper2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
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
