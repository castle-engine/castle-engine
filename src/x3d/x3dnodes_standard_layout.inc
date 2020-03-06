{
  Copyright 2008-2018 Michalis Kamburelis.

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

    strict private FFdAlign: TMFString;
    public property FdAlign: TMFString read FFdAlign;

    strict private FFdOffset: TMFFloat;
    public property FdOffset: TMFFloat read FFdOffset;

    strict private FFdOffsetUnits: TMFString;
    public property FdOffsetUnits: TMFString read FFdOffsetUnits;

    strict private FFdScaleMode: TMFString;
    public property FdScaleMode: TMFString read FFdScaleMode;

    strict private FFdSize: TMFFloat;
    public property FdSize: TMFFloat read FFdSize;

    strict private FFdSizeUnits: TMFString;
    public property FdSizeUnits: TMFString read FFdSizeUnits;

    {$I auto_generated_node_helpers/x3dnodes_layout.inc}
  end;

  TLayoutGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;

    strict private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;

    {$I auto_generated_node_helpers/x3dnodes_layoutgroup.inc}
  end;

  TLayoutLayerNode = class(TAbstractLayerNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    { Event in } { }
    strict private FEventAddChildren: TMFNodeEvent;
    public property EventAddChildren: TMFNodeEvent read FEventAddChildren;

    { Event in } { }
    strict private FEventRemoveChildren: TMFNodeEvent;
    public property EventRemoveChildren: TMFNodeEvent read FEventRemoveChildren;

    strict private FFdChildren: TMFNode;
    public property FdChildren: TMFNode read FFdChildren;

    strict private FFdLayout: TSFNode;
    public property FdLayout: TSFNode read FFdLayout;

    {$I auto_generated_node_helpers/x3dnodes_layoutlayer.inc}
  end;

  TScreenFontStyleNode = class(TAbstractFontStyleNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdFamily: TMFString;
    public property FdFamily: TMFString read FFdFamily;

    strict private FFdHorizontal: TSFBool;
    public property FdHorizontal: TSFBool read FFdHorizontal;

    strict private FFdJustify: TMFString;
    public property FdJustify: TMFString read FFdJustify;

    strict private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;

    strict private FFdLeftToRight: TSFBool;
    public property FdLeftToRight: TSFBool read FFdLeftToRight;

    strict private FFdPointSize: TSFFloat;
    public property FdPointSize: TSFFloat read FFdPointSize;

    strict private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    strict private FFdStyle: TSFString;
    public property FdStyle: TSFString read FFdStyle;

    strict private FFdTopToBottom: TSFBool;
    public property FdTopToBottom: TSFBool read FFdTopToBottom;

    {$I auto_generated_node_helpers/x3dnodes_screenfontstyle.inc}
  end;

  TScreenGroupNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

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

  FFdAlign := TMFString.Create(Self, true, 'align', ['CENTER','CENTER']);
  AddField(FFdAlign);
  { X3D specification comment: ["LEFT"|"CENTER"|"RIGHT","BOTTOM"|"CENTER"|"TOP"] }

  FFdOffset := TMFFloat.Create(Self, true, 'offset', [0,0]);
  AddField(FFdOffset);
  { X3D specification comment: (-Inf,Inf) }

  FFdOffsetUnits := TMFString.Create(Self, true, 'offsetUnits', ['WORLD','WORLD']);
  AddField(FFdOffsetUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  FFdScaleMode := TMFString.Create(Self, true, 'scaleMode', ['NONE','NONE']);
  AddField(FFdScaleMode);
  { X3D specification comment: ["NONE","FRACTION","STRETCH","PIXEL"] }

  FFdSize := TMFFloat.Create(Self, true, 'size', [1,1]);
  AddField(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSizeUnits := TMFString.Create(Self, true, 'sizeUnits', ['WORLD','WORLD']);
  AddField(FFdSizeUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  DefaultContainerField := 'children';
end;

class function TLayoutNode.ClassX3DType: string;
begin
  Result := 'Layout';
end;

procedure TLayoutGroupNode.CreateNode;
begin
  inherited;

  FFdLayout := TSFNode.Create(Self, true, 'layout', [TAbstractLayoutNode]);
  AddField(FFdLayout);

  FFdViewport := TSFNode.Create(Self, true, 'viewport', [TAbstractViewportNode]);
  AddField(FFdViewport);

  DefaultContainerField := 'children';
end;

class function TLayoutGroupNode.ClassX3DType: string;
begin
  Result := 'LayoutGroup';
end;

procedure TLayoutLayerNode.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, true, 'children', [TAbstractChildNode]);
  AddField(FFdChildren);

  FFdLayout := TSFNode.Create(Self, true, 'layout', [TAbstractLayoutNode]);
  AddField(FFdLayout);
end;

class function TLayoutLayerNode.ClassX3DType: string;
begin
  Result := 'LayoutLayer';
end;

procedure TScreenFontStyleNode.CreateNode;
begin
  inherited;

  FFdFamily := TMFString.Create(Self, false, 'family', ['SERIF']);
  AddField(FFdFamily);

  FFdHorizontal := TSFBool.Create(Self, false, 'horizontal', true);
  AddField(FFdHorizontal);

  FFdJustify := TMFString.Create(Self, false, 'justify', ['BEGIN']);
  AddField(FFdJustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdLanguage := TSFString.Create(Self, false, 'language', '');
  AddField(FFdLanguage);

  FFdLeftToRight := TSFBool.Create(Self, false, 'leftToRight', true);
  AddField(FFdLeftToRight);

  FFdPointSize := TSFFloat.Create(Self, false, 'pointSize', 12.0);
  AddField(FFdPointSize);
  { X3D specification comment: (0,Inf) }

  FFdSpacing := TSFFloat.Create(Self, false, 'spacing', 1.0);
  AddField(FFdSpacing);
  { X3D specification comment: [0,Inf) }

  FFdStyle := TSFString.Create(Self, false, 'style', 'PLAIN');
  AddField(FFdStyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdTopToBottom := TSFBool.Create(Self, false, 'topToBottom', true);
  AddField(FFdTopToBottom);
end;

class function TScreenFontStyleNode.ClassX3DType: string;
begin
  Result := 'ScreenFontStyle';
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
