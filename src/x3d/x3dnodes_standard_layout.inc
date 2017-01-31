{
  Copyright 2008-2017 Michalis Kamburelis.

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
  TAbstractLayoutNode = class(TAbstractChildNode)
  public
    procedure CreateNode; override;

    {$I auto_generated_node_helpers/x3dnodes_x3dlayoutnode.inc}
  end;

  TLayoutNode = class(TAbstractLayoutNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    {$I auto_generated_node_helpers/x3dnodes_layout.inc}
  end;

  TLayoutGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;

    private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;

    {$I auto_generated_node_helpers/x3dnodes_layoutgroup.inc}
  end;

  TLayoutLayerNode = class(TAbstractLayerNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event in } { }
    private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;

    {$I auto_generated_node_helpers/x3dnodes_layoutlayer.inc}
  end;

  TScreenFontStyleNode = class(TAbstractFontStyleNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
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

    {$I auto_generated_node_helpers/x3dnodes_screenfontstyle.inc}
  end;

  TScreenGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    {$I auto_generated_node_helpers/x3dnodes_screengroup.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractLayoutNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

procedure TLayoutNode.CreateNode;
begin
  inherited;

  FFdAlign := TMFString.Create(Self, 'align', ['CENTER','CENTER']);
  AddField(FFdAlign);
  { X3D specification comment: ["LEFT"|"CENTER"|"RIGHT","BOTTOM"|"CENTER"|"TOP"] }

  FFdOffset := TMFFloat.Create(Self, 'offset', [0,0]);
  AddField(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FFdOffsetUnits := TMFString.Create(Self, 'offsetUnits', ['WORLD','WORLD']);
  AddField(FFdOffsetUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  FFdScaleMode := TMFString.Create(Self, 'scaleMode', ['NONE','NONE']);
  AddField(FFdScaleMode);
  { X3D specification comment: ["NONE","FRACTION","STRETCH","PIXEL"] }

  FFdSize := TMFFloat.Create(Self, 'size', [1,1]);
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSizeUnits := TMFString.Create(Self, 'sizeUnits', ['WORLD','WORLD']);
  AddField(FFdSizeUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  DefaultContainerField := 'children';
end;

class function TLayoutNode.ClassX3DType: string;
begin
  Result := 'Layout';
end;

class function TLayoutNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TLayoutGroupNode.CreateNode;
begin
  inherited;

  FFdLayout := TSFNode.Create(Self, 'layout', [TAbstractLayoutNode]);
  AddField(FFdLayout);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TAbstractViewportNode]);
  AddField(FFdViewport);

  DefaultContainerField := 'children';
end;

class function TLayoutGroupNode.ClassX3DType: string;
begin
  Result := 'LayoutGroup';
end;

class function TLayoutGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TLayoutLayerNode.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, 'children', [TAbstractChildNode]);
  AddField(FFdChildren);

  FFdLayout := TSFNode.Create(Self, 'layout', [TAbstractLayoutNode]);
  AddField(FFdLayout);
end;

class function TLayoutLayerNode.ClassX3DType: string;
begin
  Result := 'LayoutLayer';
end;

class function TLayoutLayerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TScreenFontStyleNode.CreateNode;
begin
  inherited;

  FFdFamily := TMFString.Create(Self, 'family', ['SERIF']);
  FFdFamily.Exposed := false;
  AddField(FFdFamily);

  FFdHorizontal := TSFBool.Create(Self, 'horizontal', true);
  FFdHorizontal.Exposed := false;
  AddField(FFdHorizontal);

  FFdJustify := TMFString.Create(Self, 'justify', ['BEGIN']);
  FFdJustify.Exposed := false;
  AddField(FFdJustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdLanguage := TSFString.Create(Self, 'language', '');
  FFdLanguage.Exposed := false;
  AddField(FFdLanguage);

  FFdLeftToRight := TSFBool.Create(Self, 'leftToRight', true);
  FFdLeftToRight.Exposed := false;
  AddField(FFdLeftToRight);

  FFdPointSize := TSFFloat.Create(Self, 'pointSize', 12.0);
  FFdPointSize.Exposed := false;
  AddField(FFdPointSize);
  { X3D specification comment: (0,Inf) }

  FFdSpacing := TSFFloat.Create(Self, 'spacing', 1.0);
  FFdSpacing.Exposed := false;
  AddField(FFdSpacing);
  { X3D specification comment: [0,Inf) }

  FFdStyle := TSFString.Create(Self, 'style', 'PLAIN');
  FFdStyle.Exposed := false;
  AddField(FFdStyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdTopToBottom := TSFBool.Create(Self, 'topToBottom', true);
  FFdTopToBottom.Exposed := false;
  AddField(FFdTopToBottom);
end;

class function TScreenFontStyleNode.ClassX3DType: string;
begin
  Result := 'ScreenFontStyle';
end;

class function TScreenFontStyleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TScreenGroupNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TScreenGroupNode.ClassX3DType: string;
begin
  Result := 'ScreenGroup';
end;

class function TScreenGroupNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterLayoutNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLayoutNode,
    TLayoutGroupNode,
    TLayoutLayerNode,
    TScreenFontStyleNode,
    TScreenGroupNode
  ]);
end;

{$endif read_implementation}
