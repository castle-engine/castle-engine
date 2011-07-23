{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DFollowerNode = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;

    { Event: SFBool, out } { }
    private FEventIsActive: TVRMLEvent;
    public property EventIsActive: TVRMLEvent read FEventIsActive;
  end;

  TNodeX3DDamperNode = class(TNodeX3DFollowerNode)
  public
    procedure CreateNode; override;

    private FFdTau: TSFTime;
    public property FdTau: TSFTime read FFdTau;

    private FFdTolerance: TSFFloat;
    public property FdTolerance: TSFFloat read FFdTolerance;

    private FFdOrder: TSFInt32;
    public property FdOrder: TSFInt32 read FFdOrder;
  end;

  TNodeX3DChaserNode = class(TNodeX3DFollowerNode)
  public
    procedure CreateNode; override;

    private FFdDuration: TSFTime;
    public property FdDuration: TSFTime read FFdDuration;
  end;

  TNodeColorDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFColor, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFColor, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFColor, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFColor;
    public property FdInitialDestination: TSFColor read FFdInitialDestination;

    private FFdInitialValue: TSFColor;
    public property FdInitialValue: TSFColor read FFdInitialValue;
  end;

  TNodeCoordinateDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: MFVec3f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: MFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec3f;
    public property FdInitialDestination: TMFVec3f read FFdInitialDestination;

    private FFdInitialValue: TMFVec3f;
    public property FdInitialValue: TMFVec3f read FFdInitialValue;
  end;

  TNodeOrientationChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFRotation, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;
  end;

  TNodeOrientationDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFRotation, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFRotation, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFRotation;
    public property FdInitialDestination: TSFRotation read FFdInitialDestination;

    private FFdInitialValue: TSFRotation;
    public property FdInitialValue: TSFRotation read FFdInitialValue;
  end;

  TNodePositionChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFVec3f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;
  end;

  TNodePositionChaser2D = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFVec2f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;
  end;

  TNodePositionDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFVec3f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFVec3f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec3f;
    public property FdInitialDestination: TSFVec3f read FFdInitialDestination;

    private FFdInitialValue: TSFVec3f;
    public property FdInitialValue: TSFVec3f read FFdInitialValue;
  end;

  TNodePositionDamper2D = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFVec2f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFVec2f;
    public property FdInitialDestination: TSFVec2f read FFdInitialDestination;

    private FFdInitialValue: TSFVec2f;
    public property FdInitialValue: TSFVec2f read FFdInitialValue;
  end;

  TNodeScalarChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: SFFloat, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: SFFloat, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TSFFloat;
    public property FdInitialDestination: TSFFloat read FFdInitialDestination;

    private FFdInitialValue: TSFFloat;
    public property FdInitialValue: TSFFloat read FFdInitialValue;
  end;

  TNodeTexCoordDamper2D = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec2f, in } { }
    private FEventSet_destination: TVRMLEvent;
    public property EventSet_destination: TVRMLEvent read FEventSet_destination;

    { Event: MFVec2f, in } { }
    private FEventSet_value: TVRMLEvent;
    public property EventSet_value: TVRMLEvent read FEventSet_value;

    { Event: MFVec2f, out } { }
    private FEventValue_changed: TVRMLEvent;
    public property EventValue_changed: TVRMLEvent read FEventValue_changed;

    private FFdInitialDestination: TMFVec2f;
    public property FdInitialDestination: TMFVec2f read FFdInitialDestination;

    private FFdInitialValue: TMFVec2f;
    public property FdInitialValue: TMFVec2f read FFdInitialValue;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DFollowerNode.CreateNode;
begin
  inherited;

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  DefaultContainerField := 'children';
end;

procedure TNodeX3DDamperNode.CreateNode;
begin
  inherited;

  FFdtau := TSFTime.Create(Self, 'tau', 0);
  Fields.Add(FFdtau);
  { X3D specification comment: [0,Inf) }

  FFdtolerance := TSFFloat.Create(Self, 'tolerance', -1);
  Fields.Add(FFdtolerance);
  { X3D specification comment: -1 or [0,Inf) }

  FFdorder := TSFInt32.Create(Self, 'order', 0);
  FFdorder.Exposed := false;
  Fields.Add(FFdorder);
  { X3D specification comment: [0..5] }

  DefaultContainerField := 'children';
end;

procedure TNodeX3DChaserNode.CreateNode;
begin
  inherited;

  FFdduration := TSFTime.Create(Self, 'duration', 0);
  FFdduration.Exposed := false;
  Fields.Add(FFdduration);
  { X3D specification comment: [0,Inf) }

  DefaultContainerField := 'children';
end;

procedure TNodeColorDamper.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFColor, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFColor, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFColor, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFColor.Create(Self, 'initialDestination', Vector3Single(0.8, 0.8, 0.8));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFColor.Create(Self, 'initialValue', Vector3Single(0.8, 0.8, 0.8));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeColorDamper.ClassNodeTypeName: string;
begin
  Result := 'ColorDamper';
end;

class function TNodeColorDamper.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCoordinateDamper.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TMFVec3f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TMFVec3f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TMFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TMFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeCoordinateDamper.ClassNodeTypeName: string;
begin
  Result := 'CoordinateDamper';
end;

class function TNodeCoordinateDamper.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeOrientationChaser.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFRotation, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFRotation, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeOrientationChaser.ClassNodeTypeName: string;
begin
  Result := 'OrientationChaser';
end;

class function TNodeOrientationChaser.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeOrientationDamper.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFRotation, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFRotation, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFRotation.Create(Self, 'initialDestination', Vector3Single(0, 1, 0), 0);
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFRotation.Create(Self, 'initialValue', Vector3Single(0, 1, 0), 0);
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeOrientationDamper.ClassNodeTypeName: string;
begin
  Result := 'OrientationDamper';
end;

class function TNodeOrientationDamper.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionChaser.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFVec3f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFVec3f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodePositionChaser.ClassNodeTypeName: string;
begin
  Result := 'PositionChaser';
end;

class function TNodePositionChaser.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionChaser2D.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFVec2f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFVec2f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);
  { X3D specification comment: 0 }

  FFdinitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);
  { X3D specification comment: 0 }

  DefaultContainerField := 'children';
end;

class function TNodePositionChaser2D.ClassNodeTypeName: string;
begin
  Result := 'PositionChaser2D';
end;

class function TNodePositionChaser2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionDamper.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFVec3f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFVec3f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFVec3f.Create(Self, 'initialDestination', Vector3Single(0, 0, 0));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFVec3f.Create(Self, 'initialValue', Vector3Single(0, 0, 0));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodePositionDamper.ClassNodeTypeName: string;
begin
  Result := 'PositionDamper';
end;

class function TNodePositionDamper.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionDamper2D.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFVec2f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFVec2f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFVec2f.Create(Self, 'initialDestination', Vector2Single(0, 0));
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFVec2f.Create(Self, 'initialValue', Vector2Single(0, 0));
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodePositionDamper2D.ClassNodeTypeName: string;
begin
  Result := 'PositionDamper2D';
end;

class function TNodePositionDamper2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeScalarChaser.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TSFFloat, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TSFFloat, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TSFFloat.Create(Self, 'initialDestination', 0);
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TSFFloat.Create(Self, 'initialValue', 0);
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeScalarChaser.ClassNodeTypeName: string;
begin
  Result := 'ScalarChaser';
end;

class function TNodeScalarChaser.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTexCoordDamper2D.CreateNode;
begin
  inherited;

  FEventset_destination := TVRMLEvent.Create(Self, 'set_destination', TMFVec2f, true);
  Events.Add(FEventset_destination);

  FEventset_value := TVRMLEvent.Create(Self, 'set_value', TMFVec2f, true);
  Events.Add(FEventset_value);

  FEventvalue_changed := TVRMLEvent.Create(Self, 'value_changed', TMFVec2f, false);
  Events.Add(FEventvalue_changed);

  FFdinitialDestination := TMFVec2f.Create(Self, 'initialDestination', []);
  FFdinitialDestination.Exposed := false;
  Fields.Add(FFdinitialDestination);

  FFdinitialValue := TMFVec2f.Create(Self, 'initialValue', []);
  FFdinitialValue.Exposed := false;
  Fields.Add(FFdinitialValue);

  DefaultContainerField := 'children';
end;

class function TNodeTexCoordDamper2D.ClassNodeTypeName: string;
begin
  Result := 'TexCoordDamper2D';
end;

class function TNodeTexCoordDamper2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterFollowersNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeColorDamper,
    TNodeCoordinateDamper,
    TNodeOrientationChaser,
    TNodeOrientationDamper,
    TNodePositionChaser,
    TNodePositionChaser2D,
    TNodePositionDamper,
    TNodePositionDamper2D,
    TNodeScalarChaser,
    TNodeTexCoordDamper2D
  ]);
end;

{$endif read_implementation}
