{
  Copyright 2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{$ifdef read_interface}
  TNodeX3DLayoutNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeLayout = class(TNodeX3DLayoutNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdlayout: TSFNode;
    public property Fdlayout: TSFNode read FFdlayout;

    private FFdviewport: TSFNode;
    public property Fdviewport: TSFNode read FFdviewport;
  end;

  TNodeLayoutLayer = class(TNodeX3DLayerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
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
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DLayoutNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeLayout.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdalign := TMFString.Create('align', ['CENTER','CENTER']);
  Fields.Add(FFdalign);
  { X3D specification comment: ["LEFT"|"CENTER"|"RIGHT","BOTTOM"|"CENTER"|"TOP"] }

  FFdoffset := TMFFloat.Create('offset', [0,0]);
  Fields.Add(FFdoffset);
  { X3D specification comment: (-Inf,Inf) }

  FFdoffsetUnits := TMFString.Create('offsetUnits', ['WORLD','WORLD']);
  Fields.Add(FFdoffsetUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  FFdscaleMode := TMFString.Create('scaleMode', ['NONE','NONE']);
  Fields.Add(FFdscaleMode);
  { X3D specification comment: ["NONE","FRACTION","STRETCH","PIXEL"] }

  FFdsize := TMFFloat.Create('size', [1,1]);
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdsizeUnits := TMFString.Create('sizeUnits', ['WORLD','WORLD']);
  Fields.Add(FFdsizeUnits);
  { X3D specification comment: ["WORLD","FRACTION","PIXEL"] }

  DefaultContainerField := 'children';
end;

class function TNodeLayout.ClassNodeTypeName: string;
begin
  Result := 'Layout';
end;

function TNodeLayout.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLayout.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLayoutGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
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
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLayoutGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLayoutLayer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeX3DChildNode]);
  Fields.Add(FFdchildren);

  FFdlayout := TSFNode.Create(Self, 'layout', [TNodeX3DLayoutNode]);
  Fields.Add(FFdlayout);

  DefaultContainerField := 'children';
end;

class function TNodeLayoutLayer.ClassNodeTypeName: string;
begin
  Result := 'LayoutLayer';
end;

function TNodeLayoutLayer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLayoutLayer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeScreenFontStyle.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdfamily := TMFString.Create('family', ['SERIF']);
  FFdfamily.Exposed := false;
  Fields.Add(FFdfamily);

  FFdhorizontal := TSFBool.Create('horizontal', true);
  FFdhorizontal.Exposed := false;
  Fields.Add(FFdhorizontal);

  FFdjustify := TMFString.Create('justify', ['BEGIN']);
  FFdjustify.Exposed := false;
  Fields.Add(FFdjustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdlanguage := TSFString.Create('language', '');
  FFdlanguage.Exposed := false;
  Fields.Add(FFdlanguage);

  FFdleftToRight := TSFBool.Create('leftToRight', true);
  FFdleftToRight.Exposed := false;
  Fields.Add(FFdleftToRight);

  FFdpointSize := TSFFloat.Create('pointSize', 12.0);
  FFdpointSize.Exposed := false;
  Fields.Add(FFdpointSize);
  { X3D specification comment: (0,Inf) }

  FFdspacing := TSFFloat.Create('spacing', 1.0);
  FFdspacing.Exposed := false;
  Fields.Add(FFdspacing);
  { X3D specification comment: [0,Inf) }

  FFdstyle := TSFString.Create('style', 'PLAIN');
  FFdstyle.Exposed := false;
  Fields.Add(FFdstyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdtopToBottom := TSFBool.Create('topToBottom', true);
  FFdtopToBottom.Exposed := false;
  Fields.Add(FFdtopToBottom);

  DefaultContainerField := 'children';
end;

class function TNodeScreenFontStyle.ClassNodeTypeName: string;
begin
  Result := 'ScreenFontStyle';
end;

function TNodeScreenFontStyle.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeScreenFontStyle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeScreenGroup.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

class function TNodeScreenGroup.ClassNodeTypeName: string;
begin
  Result := 'ScreenGroup';
end;

function TNodeScreenGroup.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
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
