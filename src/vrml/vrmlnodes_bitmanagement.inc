{
  Copyright 2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
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
  TNodeCircle = class(TNodeCircle2D)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeLayer2D = class(TNodeGroup)
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

  TNodeLayer3D = class(TNodeGroup)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventAddChildrenLayer: TVRMLEvent;
    public property EventAddChildrenLayer: TVRMLEvent read FEventAddChildrenLayer;

    { Event: MFNode, in } { }
    private FEventRemoveChildrenLayer: TVRMLEvent;
    public property EventRemoveChildrenLayer: TVRMLEvent read FEventRemoveChildrenLayer;

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

  TNodeMouseSensor = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdEventsProcessed: TSFBool;
    public property FdEventsProcessed: TSFBool read FFdEventsProcessed;

    { Event: SFVec2f, out } { }
    private FEventClient: TVRMLEvent;
    public property EventClient: TVRMLEvent read FEventClient;

    { Event: SFVec2f, out } { }
    private FEventPosition: TVRMLEvent;
    public property EventPosition: TVRMLEvent read FEventPosition;

    { Event: SFBool, out } { }
    private FEventLButton: TVRMLEvent;
    public property EventLButton: TVRMLEvent read FEventLButton;

    { Event: SFBool, out } { }
    private FEventMButton: TVRMLEvent;
    public property EventMButton: TVRMLEvent read FEventMButton;

    { Event: SFBool, out } { }
    private FEventRButton: TVRMLEvent;
    public property EventRButton: TVRMLEvent read FEventRButton;

    { Event: SFFloat, out } { }
    private FEventMouseWheel: TVRMLEvent;
    public property EventMouseWheel: TVRMLEvent read FEventMouseWheel;
  end;

  TNodeOrderedGroup = class(TNodeGroup)
  public
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeCircle.ClassNodeTypeName: string;
begin
  Result := 'Circle';
end;

class function TNodeCircle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TNodeLayer2D.CreateNode;
begin
  inherited;

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdTranslation);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TNodeX3DBackgroundNode]);
  Fields.Add(FFdBackground);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdViewport);
end;

class function TNodeLayer2D.ClassNodeTypeName: string;
begin
  Result := 'Layer2D';
end;

class function TNodeLayer2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TNodeLayer3D.CreateNode;
begin
  inherited;

  FEventAddChildrenLayer := TVRMLEvent.Create(Self, 'addChildrenLayer', TMFNode, true);
  Events.Add(FEventAddChildrenLayer);

  FEventRemoveChildrenLayer := TVRMLEvent.Create(Self, 'removeChildrenLayer', TMFNode, true);
  Events.Add(FEventRemoveChildrenLayer);

  FFdChildrenLayer := TMFNode.Create(Self, 'childrenLayer', [TNodeLayer3D]);
  Fields.Add(FFdChildrenLayer);

  FFdTranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdTranslation);

  FFdDepth := TSFInt32.Create(Self, 'depth', 0);
  Fields.Add(FFdDepth);

  FFdSize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdSize);

  FFdBackground := TSFNode.Create(Self, 'background', [TNodeX3DBackgroundNode]);
  Fields.Add(FFdBackground);

  FFdFog := TSFNode.Create(Self, 'fog', [TNodeFog]);
  Fields.Add(FFdFog);

  FFdNavigationInfo := TSFNode.Create(Self, 'navigationInfo', [TNodeNavigationInfo]);
  Fields.Add(FFdNavigationInfo);

  FFdViewpoint := TSFNode.Create(Self, 'viewpoint', [TVRMLViewpointNode]);
  Fields.Add(FFdViewpoint);
end;

class function TNodeLayer3D.ClassNodeTypeName: string;
begin
  Result := 'Layer3D';
end;

class function TNodeLayer3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure TNodeMouseSensor.CreateNode;
begin
  inherited;

  FFdEventsProcessed := TSFBool.Create(Self, 'eventsProcessed', true);
  Fields.Add(FFdEventsProcessed);

  FEventClient := TVRMLEvent.Create(Self, 'client', TSFVec2f, false);
  Events.Add(FEventClient);

  FEventPosition := TVRMLEvent.Create(Self, 'position', TSFVec2f, false);
  Events.Add(FEventPosition);

  FEventLButton := TVRMLEvent.Create(Self, 'lButton', TSFBool, false);
  Events.Add(FEventLButton);

  FEventMButton := TVRMLEvent.Create(Self, 'mButton', TSFBool, false);
  Events.Add(FEventMButton);

  FEventRButton := TVRMLEvent.Create(Self, 'rButton', TSFBool, false);
  Events.Add(FEventRButton);

  FEventMouseWheel := TVRMLEvent.Create(Self, 'mouseWheel', TSFFloat, false);
  Events.Add(FEventMouseWheel);
end;

class function TNodeMouseSensor.ClassNodeTypeName: string;
begin
  Result := 'MouseSensor';
end;

class function TNodeMouseSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

class function TNodeOrderedGroup.ClassNodeTypeName: string;
begin
  Result := 'OrderedGroup';
end;

class function TNodeOrderedGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNBitManagementNodes + ClassNodeTypeName);
end;

procedure RegisterBitManagementNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeCircle,
    TNodeLayer2D,
    TNodeLayer3D,
    TNodeMouseSensor,
    TNodeOrderedGroup
  ]);
end;
{$endif read_implementation}
