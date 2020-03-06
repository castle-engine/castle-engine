{
  Copyright 2011-2018 Michalis Kamburelis.

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
  end;

  TLayer2DNode = class(TGroupNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    strict private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    strict private FFdBackground: TSFNode;
    public property FdBackground: TSFNode read FFdBackground;

    strict private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;
  end;

  TLayer3DNode = class(TGroupNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventAddChildrenLayer: TMFNodeEvent;
    public property EventAddChildrenLayer: TMFNodeEvent read FEventAddChildrenLayer;

    { Event in } { }
    strict private FEventRemoveChildrenLayer: TMFNodeEvent;
    public property EventRemoveChildrenLayer: TMFNodeEvent read FEventRemoveChildrenLayer;

    strict private FFdChildrenLayer: TMFNode;
    public property FdChildrenLayer: TMFNode read FFdChildrenLayer;

    strict private FFdTranslation: TSFVec2f;
    public property FdTranslation: TSFVec2f read FFdTranslation;

    strict private FFdDepth: TSFInt32;
    public property FdDepth: TSFInt32 read FFdDepth;

    strict private FFdSize: TSFVec2f;
    public property FdSize: TSFVec2f read FFdSize;

    strict private FFdBackground: TSFNode;
    public property FdBackground: TSFNode read FFdBackground;

    strict private FFdFog: TSFNode;
    public property FdFog: TSFNode read FFdFog;

    strict private FFdNavigationInfo: TSFNode;
    public property FdNavigationInfo: TSFNode read FFdNavigationInfo;

    strict private FFdViewpoint: TSFNode;
    public property FdViewpoint: TSFNode read FFdViewpoint;
  end;

  TMouseSensorNode = class(TAbstractSensorNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdEventsProcessed: TSFBool;
    public property FdEventsProcessed: TSFBool read FFdEventsProcessed;

    { Event out } { }
    strict private FEventClient: TSFVec2fEvent;
    public property EventClient: TSFVec2fEvent read FEventClient;

    { Event out } { }
    strict private FEventPosition: TSFVec2fEvent;
    public property EventPosition: TSFVec2fEvent read FEventPosition;

    { Event out } { }
    strict private FEventLButton: TSFBoolEvent;
    public property EventLButton: TSFBoolEvent read FEventLButton;

    { Event out } { }
    strict private FEventMButton: TSFBoolEvent;
    public property EventMButton: TSFBoolEvent read FEventMButton;

    { Event out } { }
    strict private FEventRButton: TSFBoolEvent;
    public property EventRButton: TSFBoolEvent read FEventRButton;

    { Event out } { }
    strict private FEventMouseWheel: TSFFloatEvent;
    public property EventMouseWheel: TSFFloatEvent read FEventMouseWheel;
  end;

  TOrderedGroupNode = class(TGroupNode)
  public
    class function ClassX3DType: string; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TCircleNode.ClassX3DType: string;
begin
  Result := 'Circle';
end;

procedure TLayer2DNode.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, true, 'translation', TVector2.Zero);
  AddField(FFdTranslation);

  FFdSize := TSFVec2f.Create(Self, true, 'size', Vector2(-1, -1));
  AddField(FFdSize);

  FFdBackground := TSFNode.Create(Self, true, 'background', [TAbstractBackgroundNode]);
  AddField(FFdBackground);

  FFdViewport := TSFNode.Create(Self, true, 'viewport', [TAbstractViewportNode]);
  AddField(FFdViewport);
end;

class function TLayer2DNode.ClassX3DType: string;
begin
  Result := 'Layer2D';
end;

procedure TLayer3DNode.CreateNode;
begin
  inherited;

  FEventAddChildrenLayer := TMFNodeEvent.Create(Self, 'addChildrenLayer', true);
  AddEvent(FEventAddChildrenLayer);

  FEventRemoveChildrenLayer := TMFNodeEvent.Create(Self, 'removeChildrenLayer', true);
  AddEvent(FEventRemoveChildrenLayer);

  FFdChildrenLayer := TMFNode.Create(Self, true, 'childrenLayer', [TLayer3DNode]);
  AddField(FFdChildrenLayer);

  FFdTranslation := TSFVec2f.Create(Self, true, 'translation', TVector2.Zero);
  AddField(FFdTranslation);

  FFdDepth := TSFInt32.Create(Self, true, 'depth', 0);
  AddField(FFdDepth);

  FFdSize := TSFVec2f.Create(Self, true, 'size', Vector2(-1, -1));
  AddField(FFdSize);

  FFdBackground := TSFNode.Create(Self, true, 'background', [TAbstractBackgroundNode]);
  AddField(FFdBackground);

  FFdFog := TSFNode.Create(Self, true, 'fog', [TFogNode]);
  AddField(FFdFog);

  FFdNavigationInfo := TSFNode.Create(Self, true, 'navigationInfo', [TNavigationInfoNode]);
  AddField(FFdNavigationInfo);

  FFdViewpoint := TSFNode.Create(Self, true, 'viewpoint', [TAbstractViewpointNode]);
  AddField(FFdViewpoint);
end;

class function TLayer3DNode.ClassX3DType: string;
begin
  Result := 'Layer3D';
end;

procedure TMouseSensorNode.CreateNode;
begin
  inherited;

  FFdEventsProcessed := TSFBool.Create(Self, true, 'eventsProcessed', true);
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

class function TOrderedGroupNode.ClassX3DType: string;
begin
  Result := 'OrderedGroup';
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
