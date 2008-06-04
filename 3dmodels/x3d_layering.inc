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
  TNodeX3DLayerNode = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdisPickable: TSFBool;
    public property FdisPickable: TSFBool read FFdisPickable;

    private FFdviewport: TSFNode;
    public property Fdviewport: TSFNode read FFdviewport;
  end;

  TNodeX3DViewportNode = class(TNodeX3DGroupingNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeLayer = class(TNodeX3DLayerNode)
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
  end;

  TNodeLayerSet = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdactiveLayer: TSFInt32;
    public property FdactiveLayer: TSFInt32 read FFdactiveLayer;

    private FFdlayers: TMFNode;
    public property Fdlayers: TMFNode read FFdlayers;

    private FFdorder: TMFInt32;
    public property Fdorder: TMFInt32 read FFdorder;
  end;

  TNodeViewport = class(TNodeX3DGroupingNode, INodeX3DBoundedObject)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclipBoundary: TMFFloat;
    public property FdclipBoundary: TMFFloat read FFdclipBoundary;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DLayerNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdisPickable := TSFBool.Create('isPickable', true);
  Fields.Add(FFdisPickable);

  FFdviewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdviewport);

  DefaultContainerField := 'layers';
end;

constructor TNodeX3DViewportNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;
end;

constructor TNodeLayer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create('addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create('removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeX3DChildNode]);
  Fields.Add(FFdchildren);
end;

class function TNodeLayer.ClassNodeTypeName: string;
begin
  Result := 'Layer';
end;

function TNodeLayer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLayer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLayerSet.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdactiveLayer := TSFInt32.Create('activeLayer', 0);
  Fields.Add(FFdactiveLayer);
  { X3D specification comment: (-Inf,Inf) }

  FFdlayers := TMFNode.Create(Self, 'layers', [TNodeX3DLayerNode]);
  Fields.Add(FFdlayers);

  FFdorder := TMFInt32.Create('order', [0]);
  Fields.Add(FFdorder);
  { X3D specification comment: (0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLayerSet.ClassNodeTypeName: string;
begin
  Result := 'LayerSet';
end;

function TNodeLayerSet.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeLayerSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeViewport.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclipBoundary := TMFFloat.Create('clipBoundary', [0.0]);
  Fields.Add(FFdclipBoundary);
  { X3D specification comment: 1 0 1  [0,1] }
end;

class function TNodeViewport.ClassNodeTypeName: string;
begin
  Result := 'Viewport';
end;

function TNodeViewport.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeViewport.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterLayeringNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeLayer,
    TNodeLayerSet,
    TNodeViewport
  ]);
end;

{$endif read_implementation}
