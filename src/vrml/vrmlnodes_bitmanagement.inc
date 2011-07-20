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

    private FFdtranslation: TSFVec2f;
    public property Fdtranslation: TSFVec2f read FFdtranslation;

    private FFdsize: TSFVec2f;
    public property Fdsize: TSFVec2f read FFdsize;

    private FFdbackground: TSFNode;
    public property Fdbackground: TSFNode read FFdbackground;

    private FFdviewport: TSFNode;
    public property Fdviewport: TSFNode read FFdviewport;
  end;

  TNodeLayer3D = class(TNodeGroup)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddChildrenLayer: TVRMLEvent;
    public property EventaddChildrenLayer: TVRMLEvent read FEventaddChildrenLayer;

    { Event: MFNode, in } { }
    private FEventremoveChildrenLayer: TVRMLEvent;
    public property EventremoveChildrenLayer: TVRMLEvent read FEventremoveChildrenLayer;

    private FFdchildrenLayer: TMFNode;
    public property FdchildrenLayer: TMFNode read FFdchildrenLayer;

    private FFdtranslation: TSFVec2f;
    public property Fdtranslation: TSFVec2f read FFdtranslation;

    private FFddepth: TSFInt32;
    public property Fddepth: TSFInt32 read FFddepth;

    private FFdsize: TSFVec2f;
    public property Fdsize: TSFVec2f read FFdsize;

    private FFdbackground: TSFNode;
    public property Fdbackground: TSFNode read FFdbackground;

    private FFdfog: TSFNode;
    public property Fdfog: TSFNode read FFdfog;

    private FFdnavigationInfo: TSFNode;
    public property FdnavigationInfo: TSFNode read FFdnavigationInfo;

    private FFdviewpoint: TSFNode;
    public property Fdviewpoint: TSFNode read FFdviewpoint;
  end;

  TNodeMouseSensor = class(TNodeX3DSensorNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdeventsProcessed: TSFBool;
    public property FdeventsProcessed: TSFBool read FFdeventsProcessed;

    { Event: SFVec2f, out } { }
    private FEventclient: TVRMLEvent;
    public property Eventclient: TVRMLEvent read FEventclient;

    { Event: SFVec2f, out } { }
    private FEventposition: TVRMLEvent;
    public property Eventposition: TVRMLEvent read FEventposition;

    { Event: SFBool, out } { }
    private FEventlButton: TVRMLEvent;
    public property EventlButton: TVRMLEvent read FEventlButton;

    { Event: SFBool, out } { }
    private FEventmButton: TVRMLEvent;
    public property EventmButton: TVRMLEvent read FEventmButton;

    { Event: SFBool, out } { }
    private FEventrButton: TVRMLEvent;
    public property EventrButton: TVRMLEvent read FEventrButton;

    { Event: SFFloat, out } { }
    private FEventmouseWheel: TVRMLEvent;
    public property EventmouseWheel: TVRMLEvent read FEventmouseWheel;
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
