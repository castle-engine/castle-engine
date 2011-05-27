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
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;
  end;

  TNodeX3DDamperNode = class(TNodeX3DFollowerNode)
  public
    procedure CreateNode; override;

    private FFdtau: TSFTime;
    public property Fdtau: TSFTime read FFdtau;

    private FFdtolerance: TSFFloat;
    public property Fdtolerance: TSFFloat read FFdtolerance;

    private FFdorder: TSFInt32;
    public property Fdorder: TSFInt32 read FFdorder;
  end;

  TNodeX3DChaserNode = class(TNodeX3DFollowerNode)
  public
    procedure CreateNode; override;

    private FFdduration: TSFTime;
    public property Fdduration: TSFTime read FFdduration;
  end;

  TNodeColorDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFColor, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFColor, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFColor, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFColor;
    public property FdinitialDestination: TSFColor read FFdinitialDestination;

    private FFdinitialValue: TSFColor;
    public property FdinitialValue: TSFColor read FFdinitialValue;
  end;

  TNodeCoordinateDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec3f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: MFVec3f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TMFVec3f;
    public property FdinitialDestination: TMFVec3f read FFdinitialDestination;

    private FFdinitialValue: TMFVec3f;
    public property FdinitialValue: TMFVec3f read FFdinitialValue;
  end;

  TNodeOrientationChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFRotation, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFRotation;
    public property FdinitialDestination: TSFRotation read FFdinitialDestination;

    private FFdinitialValue: TSFRotation;
    public property FdinitialValue: TSFRotation read FFdinitialValue;
  end;

  TNodeOrientationDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFRotation, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFRotation, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFRotation;
    public property FdinitialDestination: TSFRotation read FFdinitialDestination;

    private FFdinitialValue: TSFRotation;
    public property FdinitialValue: TSFRotation read FFdinitialValue;
  end;

  TNodePositionChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFVec3f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFVec3f;
    public property FdinitialDestination: TSFVec3f read FFdinitialDestination;

    private FFdinitialValue: TSFVec3f;
    public property FdinitialValue: TSFVec3f read FFdinitialValue;
  end;

  TNodePositionChaser2D = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFVec2f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFVec2f;
    public property FdinitialDestination: TSFVec2f read FFdinitialDestination;

    private FFdinitialValue: TSFVec2f;
    public property FdinitialValue: TSFVec2f read FFdinitialValue;
  end;

  TNodePositionDamper = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec3f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFVec3f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFVec3f;
    public property FdinitialDestination: TSFVec3f read FFdinitialDestination;

    private FFdinitialValue: TSFVec3f;
    public property FdinitialValue: TSFVec3f read FFdinitialValue;
  end;

  TNodePositionDamper2D = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFVec2f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFVec2f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFVec2f;
    public property FdinitialDestination: TSFVec2f read FFdinitialDestination;

    private FFdinitialValue: TSFVec2f;
    public property FdinitialValue: TSFVec2f read FFdinitialValue;
  end;

  TNodeScalarChaser = class(TNodeX3DChaserNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: SFFloat, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TSFFloat;
    public property FdinitialDestination: TSFFloat read FFdinitialDestination;

    private FFdinitialValue: TSFFloat;
    public property FdinitialValue: TSFFloat read FFdinitialValue;
  end;

  TNodeTexCoordDamper2D = class(TNodeX3DDamperNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFVec2f, in } { }
    private FEventset_destination: TVRMLEvent;
    public property Eventset_destination: TVRMLEvent read FEventset_destination;

    { Event: MFVec2f, in } { }
    private FEventset_value: TVRMLEvent;
    public property Eventset_value: TVRMLEvent read FEventset_value;

    { Event: MFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;

    private FFdinitialDestination: TMFVec2f;
    public property FdinitialDestination: TMFVec2f read FFdinitialDestination;

    private FFdinitialValue: TMFVec2f;
    public property FdinitialValue: TMFVec2f read FFdinitialValue;
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

function TNodeColorDamper.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeCoordinateDamper.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeOrientationChaser.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeOrientationDamper.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodePositionChaser.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodePositionChaser2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodePositionDamper.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodePositionDamper2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeScalarChaser.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeTexCoordDamper2D.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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
