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

  FFdtranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdtranslation);

  FFdsize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdsize);

  FFdbackground := TSFNode.Create(Self, 'background', [TNodeX3DBackgroundNode]);
  Fields.Add(FFdbackground);

  FFdviewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdviewport);
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

  FEventaddChildrenLayer := TVRMLEvent.Create(Self, 'addChildrenLayer', TMFNode, true);
  Events.Add(FEventaddChildrenLayer);

  FEventremoveChildrenLayer := TVRMLEvent.Create(Self, 'removeChildrenLayer', TMFNode, true);
  Events.Add(FEventremoveChildrenLayer);

  FFdchildrenLayer := TMFNode.Create(Self, 'childrenLayer', [TNodeLayer3D]);
  Fields.Add(FFdchildrenLayer);

  FFdtranslation := TSFVec2f.Create(Self, 'translation', ZeroVector2Single);
  Fields.Add(FFdtranslation);

  FFddepth := TSFInt32.Create(Self, 'depth', 0);
  Fields.Add(FFddepth);

  FFdsize := TSFVec2f.Create(Self, 'size', Vector2Single(-1, -1));
  Fields.Add(FFdsize);

  FFdbackground := TSFNode.Create(Self, 'background', [TNodeX3DBackgroundNode]);
  Fields.Add(FFdbackground);

  FFdfog := TSFNode.Create(Self, 'fog', [TNodeFog]);
  Fields.Add(FFdfog);

  FFdnavigationInfo := TSFNode.Create(Self, 'navigationInfo', [TNodeNavigationInfo]);
  Fields.Add(FFdnavigationInfo);

  FFdviewpoint := TSFNode.Create(Self, 'viewpoint', [TVRMLViewpointNode]);
  Fields.Add(FFdviewpoint);
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

  FFdeventsProcessed := TSFBool.Create(Self, 'eventsProcessed', true);
  Fields.Add(FFdeventsProcessed);

  FEventclient := TVRMLEvent.Create(Self, 'client', TSFVec2f, false);
  Events.Add(FEventclient);

  FEventposition := TVRMLEvent.Create(Self, 'position', TSFVec2f, false);
  Events.Add(FEventposition);

  FEventlButton := TVRMLEvent.Create(Self, 'lButton', TSFBool, false);
  Events.Add(FEventlButton);

  FEventmButton := TVRMLEvent.Create(Self, 'mButton', TSFBool, false);
  Events.Add(FEventmButton);

  FEventrButton := TVRMLEvent.Create(Self, 'rButton', TSFBool, false);
  Events.Add(FEventrButton);

  FEventmouseWheel := TVRMLEvent.Create(Self, 'mouseWheel', TSFFloat, false);
  Events.Add(FEventmouseWheel);
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
