{
  Copyright 2011-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined by BitManagement (BS Contact) extensions. See
  - http://www.blaxxun.es/pdf/BS_Contact_VRML.en.pdf
    for some description (unfortunately, no precise spec),
  - http://www.bitmanagement.com/developer/contact/examples/layer/index.html
    for Layer2D / Layer3D specs.
}

{$ifdef read_interface}
  TCircleNode = class(TCircle2DNode)
  public
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TLayer2DNode = class(TGroupNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    private FFdBackground: TSFNode;
    public property FdBackground: TSFNode read FFdBackground;

    private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;
  end;

  TLayer3DNode = class(TGroupNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventAddChildrenLayer: TMFNodeEvent;
    public property EventAddChildrenLayer: TMFNodeEvent read FEventAddChildrenLayer;

    { Event in } { }
    private FEventRemoveChildrenLayer: TMFNodeEvent;
    public property EventRemoveChildrenLayer: TMFNodeEvent read FEventRemoveChildrenLayer;

    private FFdChildrenLayer: TMFNode;
    public property FdChildrenLayer: TMFNode read FFdChildrenLayer;

    private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    private FFdDepth: TSFInt32;
    public property FdDepth: TSFInt32 read FFdDepth;

    private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    private FFdBackground: TSFNode;
    public property FdBackground: TSFNode read FFdBackground;

    private FFdFog: TSFNode;
    public property FdFog: TSFNode read FFdFog;

    private FFdNavigationInfo: TSFNode;
    public property FdNavigationInfo: TSFNode read FFdNavigationInfo;

    private FFdViewpoint: TSFNode;
    public property FdViewpoint: TSFNode read FFdViewpoint;
  end;

  TMouseSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEventsProcessed: TSFBool;
    public property FdEventsProcessed: TSFBool read FFdEventsProcessed;

    { Event out } { }
    private FEventClient: TSFVec2fEvent;
    public property EventClient: TSFVec2fEvent read FEventClient;

    { Event out } { }
    private FEventPosition: TSFVec2fEvent;
    public property EventPosition: TSFVec2fEvent read FEventPosition;

    { Event out } { }
    private FEventLButton: TSFBoolEvent;
    public property EventLButton: TSFBoolEvent read FEventLButton;

    { Event out } { }
    private FEventMButton: TSFBoolEvent;
    public property EventMButton: TSFBoolEvent read FEventMButton;

    { Event out } { }
    private FEventRButton: TSFBoolEvent;
    public property EventRButton: TSFBoolEvent read FEventRButton;

    { Event out } { }
    private FEventMouseWheel: TSFFloatEvent;
    public property EventMouseWheel: TSFFloatEvent read FEventMouseWheel;
  end;

  TOrderedGroupNode = class(TGroupNode)
  public
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TCircleNode.ClassX3DType: string;
begin
  Result := 'Circle';
end;

class function TCircleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassX3DType);
end;

procedure TLayer2DNode.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  AddField(FFdTranslation);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  AddField(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TAbstractBackgroundNode]);
  AddField(FFdBackground);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TAbstractViewportNode]);
  AddField(FFdViewport);
end;

class function TLayer2DNode.ClassX3DType: string;
begin
  Result := 'Layer2D';
end;

class function TLayer2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassX3DType);
end;

procedure TLayer3DNode.CreateNode;
begin
  inherited;

  FEventAddChildrenLayer := TMFNodeEvent.Create(Self, 'addChildrenLayer', true);
  AddEvent(FEventAddChildrenLayer);

  FEventRemoveChildrenLayer := TMFNodeEvent.Create(Self, 'removeChildrenLayer', true);
  AddEvent(FEventRemoveChildrenLayer);

  FFdChildrenLayer := TMFNode.Create(Self, 'childrenLayer', [TLayer3DNode]);
  AddField(FFdChildrenLayer);

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  AddField(FFdTranslation);

  FFdDepth := TSFInt32.Create(Self, 'depth', 0);
  AddField(FFdDepth);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  AddField(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TAbstractBackgroundNode]);
  AddField(FFdBackground);

  FFdFog := TSFNode.Create(Self, 'fog', [TFogNode]);
  AddField(FFdFog);

  FFdNavigationInfo := TSFNode.Create(Self, 'navigationInfo', [TNavigationInfoNode]);
  AddField(FFdNavigationInfo);

  FFdViewpoint := TSFNode.Create(Self, 'viewpoint', [TAbstractViewpointNode]);
  AddField(FFdViewpoint);
end;

class function TLayer3DNode.ClassX3DType: string;
begin
  Result := 'Layer3D';
end;

class function TLayer3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassX3DType);
end;

procedure TMouseSensorNode.CreateNode;
begin
  inherited;

  FFdEventsProcessed := TSFBool.Create(Self, 'eventsProcessed', true);
  AddField(FFdEventsProcessed);

  FEventClient := TSFVec2fEvent.Create(Self, 'client', false);
  AddEvent(FEventClient);

  FEventPosition := TSFVec2fEvent.Create(Self, 'position', false);
  AddEvent(FEventPosition);

  FEventLButton := TSFBoolEvent.Create(Self, 'lButton', false);
  AddEvent(FEventLButton);

  FEventMButton := TSFBoolEvent.Create(Self, 'mButton', false);
  AddEvent(FEventMButton);

  FEventRButton := TSFBoolEvent.Create(Self, 'rButton', false);
  AddEvent(FEventRButton);

  FEventMouseWheel := TSFFloatEvent.Create(Self, 'mouseWheel', false);
  AddEvent(FEventMouseWheel);
end;

class function TMouseSensorNode.ClassX3DType: string;
begin
  Result := 'MouseSensor';
end;

class function TMouseSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassX3DType);
end;

class function TOrderedGroupNode.ClassX3DType: string;
begin
  Result := 'OrderedGroup';
end;

class function TOrderedGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassX3DType);
end;

procedure RegisterBitManagementNodes;
begin
  NodesManager.RegisterNodeClasses([
    TCircleNode,
    TLayer2DNode,
    TLayer3DNode,
    TMouseSensorNode,
    TOrderedGroupNode
  ]);
end;
{$endif read_implementation}
