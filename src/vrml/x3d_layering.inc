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
  TNodeX3DLayerNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;

    private FFdIsPickable: TSFBool;
    public property FdIsPickable: TSFBool read FFdIsPickable;

    private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;
  end;

  TNodeX3DViewportNode = class(TNodeX3DGroupingNode)
  public
    procedure CreateNode; override;
  end;

  TNodeLayer = class(TNodeX3DLayerNode)
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
  end;

  TNodeLayerSet = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdActiveLayer: TSFInt32;
    public property FdActiveLayer: TSFInt32 read FFdActiveLayer;

    private FFdLayers: TMFNode;
    public property FdLayers: TMFNode read FFdLayers;

    private FFdOrder: TMFInt32;
    public property FdOrder: TMFInt32 read FFdOrder;
  end;

  TNodeViewport = class(TNodeX3DGroupingNode, INodeX3DBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClipBoundary: TMFFloat;
    public property FdClipBoundary: TMFFloat read FFdClipBoundary;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DLayerNode.CreateNode;
begin
  inherited;

  FFdIsPickable := TSFBool.Create(Self, 'isPickable', true);
  Fields.Add(FFdIsPickable);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdViewport);

  DefaultContainerField := 'layers';
end;

procedure TNodeX3DViewportNode.CreateNode;
begin
  inherited;
end;

procedure TNodeLayer.CreateNode;
begin
  inherited;

  FEventAddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventAddChildren);

  FEventRemoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, 'children', [TNodeX3DChildNode]);
  Fields.Add(FFdChildren);
end;

class function TNodeLayer.ClassNodeTypeName: string;
begin
  Result := 'Layer';
end;

class function TNodeLayer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeLayerSet.CreateNode;
begin
  inherited;

  FFdActiveLayer := TSFInt32.Create(Self, 'activeLayer', 0);
  Fields.Add(FFdActiveLayer);
  { X3D specification comment: (-Inf,Inf) }

  FFdLayers := TMFNode.Create(Self, 'layers', [TNodeX3DLayerNode]);
  Fields.Add(FFdLayers);

  FFdOrder := TMFInt32.Create(Self, 'order', [0]);
  Fields.Add(FFdOrder);
  { X3D specification comment: (0,Inf) }

  DefaultContainerField := 'children';
end;

class function TNodeLayerSet.ClassNodeTypeName: string;
begin
  Result := 'LayerSet';
end;

class function TNodeLayerSet.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeViewport.CreateNode;
begin
  inherited;

  FFdClipBoundary := TMFFloat.Create(Self, 'clipBoundary', [0.0]);
  Fields.Add(FFdClipBoundary);
  { X3D specification comment: 1 0 1  [0,1] }
end;

class function TNodeViewport.ClassNodeTypeName: string;
begin
  Result := 'Viewport';
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
