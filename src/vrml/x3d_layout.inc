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
  TNodeX3DLayoutNode = class(TNodeX3DChildNode)
  public
    procedure CreateNode; override;
  end;

  TNodeLayout = class(TNodeX3DLayoutNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdAlign: TMFString;
    public property FdAlign: TMFString read FFdAlign;

    private FFdOffset: TMFFloat;
    public property FdOffset: TMFFloat read FFdOffset;

    private FFdOffsetUnits: TMFString;
    public property FdOffsetUnits: TMFString read FFdOffsetUnits;

    private FFdScaleMode: TMFString;
    public property FdScaleMode: TMFString read FFdScaleMode;

    private FFdSize: TMFFloat;
    public property FdSize: TMFFloat read FFdSize;

    private FFdSizeUnits: TMFString;
    public property FdSizeUnits: TMFString read FFdSizeUnits;
  end;

  TNodeLayoutGroup = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;

    private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;
  end;

  TNodeLayoutLayer = class(TNodeX3DLayerNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventAddChildren: TVRMLEvent;
    public property EventAddChildren: TVRMLEvent read FEventAddChildren;

    { Event: MFNode, in } { }
    private FEventRemoveChildren: TVRMLEvent;
    public property EventRemoveChildren: TVRMLEvent read FEventRemoveChildren;

    private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;
  end;

  TNodeScreenFontStyle = class(TNodeX3DFontStyleNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFamily: TMFString;
    public property FdFamily: TMFString read FFdFamily;

    private FFdHorizontal: TSFBool;
    public property FdHorizontal: TSFBool read FFdHorizontal;

    private FFdJustify: TMFString;
    public property FdJustify: TMFString read FFdJustify;

    private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;

    private FFdLeftToRight: TSFBool;
    public property FdLeftToRight: TSFBool read FFdLeftToRight;

    private FFdPointSize: TSFFloat;
    public property FdPointSize: TSFFloat read FFdPointSize;

    private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    private FFdStyle: TSFString;
    public property FdStyle: TSFString read FFdStyle;

    private FFdTopToBottom: TSFBool;
    public property FdTopToBottom: TSFBool read FFdTopToBottom;
  end;

  TNodeScreenGroup = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DLayoutNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TNodeLayout.CreateNode;
begin
  inherited;

  FFdalign := TMFString.Create(Self, 'align', ['CENTER','CENTER']);
  Fields.Add(FFdalign);
  { X3D specification comment: ["LEFT"|"CENTER"|"RIGHT","BOTTOM"|"CENTER"|"TOP"] }

  FFdoffset := TMFFloat.Create(Self, 'offset', [0,0]);
  Fields.Add(FFdoffset);
  { X3D specification comment: (-Inf,Inf) }

  FFdoffsetUnits := TMFString.Create(Self, 'offsetUnits', ['WORLD','WORLD']);
  Fields.Add(FFdoffsetUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  FFdscaleMode := TMFString.Create(Self, 'scaleMode', ['NONE','NONE']);
  Fields.Add(FFdscaleMode);
  { X3D specification comment: ["NONE","FRACTION","STRETCH","PIXEL"] }

  FFdsize := TMFFloat.Create(Self, 'size', [1,1]);
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsizeUnits := TMFString.Create(Self, 'sizeUnits', ['WORLD','WORLD']);
  Fields.Add(FFdsizeUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  DefaultContainerField := 'children';
end;

class function TNodeLayout.ClassNodeTypeName: string;
begin
  Result := 'Layout';
end;

class function TNodeLayout.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeLayoutGroup.CreateNode;
begin
  inherited;

  FFdlayout := TSFNode.Create(Self, 'layout', [TNodeX3DLayoutNode]);
  Fields.Add(FFdlayout);

  FFdviewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdviewport);

  DefaultContainerField := 'children';
end;

class function TNodeLayoutGroup.ClassNodeTypeName: string;
begin
  Result := 'LayoutGroup';
end;

class function TNodeLayoutGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeLayoutLayer.CreateNode;
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeX3DChildNode]);
  Fields.Add(FFdchildren);

  FFdlayout := TSFNode.Create(Self, 'layout', [TNodeX3DLayoutNode]);
  Fields.Add(FFdlayout);
end;

class function TNodeLayoutLayer.ClassNodeTypeName: string;
begin
  Result := 'LayoutLayer';
end;

class function TNodeLayoutLayer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeScreenFontStyle.CreateNode;
begin
  inherited;

  FFdfamily := TMFString.Create(Self, 'family', ['SERIF']);
  FFdfamily.Exposed := false;
  Fields.Add(FFdfamily);

  FFdhorizontal := TSFBool.Create(Self, 'horizontal', true);
  FFdhorizontal.Exposed := false;
  Fields.Add(FFdhorizontal);

  FFdjustify := TMFString.Create(Self, 'justify', ['BEGIN']);
  FFdjustify.Exposed := false;
  Fields.Add(FFdjustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdlanguage := TSFString.Create(Self, 'language', '');
  FFdlanguage.Exposed := false;
  Fields.Add(FFdlanguage);

  FFdleftToRight := TSFBool.Create(Self, 'leftToRight', true);
  FFdleftToRight.Exposed := false;
  Fields.Add(FFdleftToRight);

  FFdpointSize := TSFFloat.Create(Self, 'pointSize', 12.0);
  FFdpointSize.Exposed := false;
  Fields.Add(FFdpointSize);
  { X3D specification comment: (0,Inf) }

  FFdspacing := TSFFloat.Create(Self, 'spacing', 1.0);
  FFdspacing.Exposed := false;
  Fields.Add(FFdspacing);
  { X3D specification comment: [0,Inf) }

  FFdstyle := TSFString.Create(Self, 'style', 'PLAIN');
  FFdstyle.Exposed := false;
  Fields.Add(FFdstyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdtopToBottom := TSFBool.Create(Self, 'topToBottom', true);
  FFdtopToBottom.Exposed := false;
  Fields.Add(FFdtopToBottom);
end;

class function TNodeScreenFontStyle.ClassNodeTypeName: string;
begin
  Result := 'ScreenFontStyle';
end;

class function TNodeScreenFontStyle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeScreenGroup.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeScreenGroup.ClassNodeTypeName: string;
begin
  Result := 'ScreenGroup';
end;

class function TNodeScreenGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterLayoutNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeLayout,
    TNodeLayoutGroup,
    TNodeLayoutLayer,
    TNodeScreenFontStyle,
    TNodeScreenGroup
  ]);
end;

{$endif read_implementation}
