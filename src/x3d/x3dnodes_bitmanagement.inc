{
  Copyright 2011-2013 Michalis Kamburelis.

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
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TLayer2DNode = class(TGroupNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
    class function ClassNodeTypeName: string; override;
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
    class function ClassNodeTypeName: string; override;
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
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TCircleNode.ClassNodeTypeName: string;
begin
  Result := 'Circle';
end;

class function TCircleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TLayer2DNode.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdTranslation);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TAbstractBackgroundNode]);
  Fields.Add(FFdBackground);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TAbstractViewportNode]);
  Fields.Add(FFdViewport);
end;

class function TLayer2DNode.ClassNodeTypeName: string;
begin
  Result := 'Layer2D';
end;

class function TLayer2DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TLayer3DNode.CreateNode;
begin
  inherited;

  FEventAddChildrenLayer := TMFNodeEvent.Create(Self, 'addChildrenLayer', true);
  Events.Add(FEventAddChildrenLayer);

  FEventRemoveChildrenLayer := TMFNodeEvent.Create(Self, 'removeChildrenLayer', true);
  Events.Add(FEventRemoveChildrenLayer);

  FFdChildrenLayer := TMFNode.Create(Self, 'childrenLayer', [TLayer3DNode]);
  Fields.Add(FFdChildrenLayer);

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdTranslation);

  FFdDepth := TSFInt32.Create(Self, 'depth', 0);
  Fields.Add(FFdDepth);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TAbstractBackgroundNode]);
  Fields.Add(FFdBackground);

  FFdFog := TSFNode.Create(Self, 'fog', [TFogNode]);
  Fields.Add(FFdFog);

  FFdNavigationInfo := TSFNode.Create(Self, 'navigationInfo', [TNavigationInfoNode]);
  Fields.Add(FFdNavigationInfo);

  FFdViewpoint := TSFNode.Create(Self, 'viewpoint', [TAbstractViewpointNode]);
  Fields.Add(FFdViewpoint);
end;

class function TLayer3DNode.ClassNodeTypeName: string;
begin
  Result := 'Layer3D';
end;

class function TLayer3DNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TMouseSensorNode.CreateNode;
begin
  inherited;

  FFdEventsProcessed := TSFBool.Create(Self, 'eventsProcessed', true);
  Fields.Add(FFdEventsProcessed);

  FEventClient := TSFVec2fEvent.Create(Self, 'client', false);
  Events.Add(FEventClient);

  FEventPosition := TSFVec2fEvent.Create(Self, 'position', false);
  Events.Add(FEventPosition);

  FEventLButton := TSFBoolEvent.Create(Self, 'lButton', false);
  Events.Add(FEventLButton);

  FEventMButton := TSFBoolEvent.Create(Self, 'mButton', false);
  Events.Add(FEventMButton);

  FEventRButton := TSFBoolEvent.Create(Self, 'rButton', false);
  Events.Add(FEventRButton);

  FEventMouseWheel := TSFFloatEvent.Create(Self, 'mouseWheel', false);
  Events.Add(FEventMouseWheel);
end;

class function TMouseSensorNode.ClassNodeTypeName: string;
begin
  Result := 'MouseSensor';
end;

class function TMouseSensorNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

class function TOrderedGroupNode.ClassNodeTypeName: string;
begin
  Result := 'OrderedGroup';
end;

class function TOrderedGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
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
