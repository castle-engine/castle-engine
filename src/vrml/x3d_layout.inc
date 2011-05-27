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
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdalign: TMFString;
    public property Fdalign: TMFString read FFdalign;

    private FFdoffset: TMFFloat;
    public property Fdoffset: TMFFloat read FFdoffset;

    private FFdoffsetUnits: TMFString;
    public property FdoffsetUnits: TMFString read FFdoffsetUnits;

    private FFdscaleMode: TMFString;
    public property FdscaleMode: TMFString read FFdscaleMode;

    private FFdsize: TMFFloat;
    public property Fdsize: TMFFloat read FFdsize;

    private FFdsizeUnits: TMFString;
    public property FdsizeUnits: TMFString read FFdsizeUnits;
  end;

  TNodeLayoutGroup = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdlayout: TSFNode;
    public property Fdlayout: TSFNode read FFdlayout;

    private FFdviewport: TSFNode;
    public property Fdviewport: TSFNode read FFdviewport;
  end;

  TNodeLayoutLayer = class(TNodeX3DLayerNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: MFNode, in } { }
    private FEventaddChildren: TVRMLEvent;
    public property EventaddChildren: TVRMLEvent read FEventaddChildren;

    { Event: MFNode, in } { }
    private FEventremoveChildren: TVRMLEvent;
    public property EventremoveChildren: TVRMLEvent read FEventremoveChildren;

    private FFdchildren: TMFNode;
    public property Fdchildren: TMFNode read FFdchildren;

    private FFdlayout: TSFNode;
    public property Fdlayout: TSFNode read FFdlayout;
  end;

  TNodeScreenFontStyle = class(TNodeX3DFontStyleNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdfamily: TMFString;
    public property Fdfamily: TMFString read FFdfamily;

    private FFdhorizontal: TSFBool;
    public property Fdhorizontal: TSFBool read FFdhorizontal;

    private FFdjustify: TMFString;
    public property Fdjustify: TMFString read FFdjustify;

    private FFdlanguage: TSFString;
    public property Fdlanguage: TSFString read FFdlanguage;

    private FFdleftToRight: TSFBool;
    public property FdleftToRight: TSFBool read FFdleftToRight;

    private FFdpointSize: TSFFloat;
    public property FdpointSize: TSFFloat read FFdpointSize;

    private FFdspacing: TSFFloat;
    public property Fdspacing: TSFFloat read FFdspacing;

    private FFdstyle: TSFString;
    public property Fdstyle: TSFString read FFdstyle;

    private FFdtopToBottom: TSFBool;
    public property FdtopToBottom: TSFBool read FFdtopToBottom;
  end;

  TNodeScreenGroup = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean; override;
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

function TNodeLayout.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeLayoutGroup.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeLayoutLayer.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeScreenFontStyle.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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

function TNodeScreenGroup.SuggestedVRMLVersion(
  out Version: TVRMLVersion; out SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);
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
