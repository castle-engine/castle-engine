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

    private FFdisPickable: TSFBool;
    public property FdisPickable: TSFBool read FFdisPickable;

    private FFdviewport: TSFNode;
    public property Fdviewport: TSFNode read FFdviewport;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
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
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclipBoundary: TMFFloat;
    public property FdclipBoundary: TMFFloat read FFdclipBoundary;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DLayerNode.CreateNode;
begin
  inherited;

  FFdisPickable := TSFBool.Create(Self, 'isPickable', true);
  Fields.Add(FFdisPickable);

  FFdviewport := TSFNode.Create(Self, 'viewport', [TNodeX3DViewportNode]);
  Fields.Add(FFdviewport);

  DefaultContainerField := 'layers';
end;

procedure TNodeX3DViewportNode.CreateNode;
begin
  inherited;
end;

procedure TNodeLayer.CreateNode;
begin
  inherited;

  FEventaddChildren := TVRMLEvent.Create(Self, 'addChildren', TMFNode, true);
  Events.Add(FEventaddChildren);

  FEventremoveChildren := TVRMLEvent.Create(Self, 'removeChildren', TMFNode, true);
  Events.Add(FEventremoveChildren);

  FFdchildren := TMFNode.Create(Self, 'children', [TNodeX3DChildNode]);
  Fields.Add(FFdchildren);
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

  FFdactiveLayer := TSFInt32.Create(Self, 'activeLayer', 0);
  Fields.Add(FFdactiveLayer);
  { X3D specification comment: (-Inf,Inf) }

  FFdlayers := TMFNode.Create(Self, 'layers', [TNodeX3DLayerNode]);
  Fields.Add(FFdlayers);

  FFdorder := TMFInt32.Create(Self, 'order', [0]);
  Fields.Add(FFdorder);
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

  FFdclipBoundary := TMFFloat.Create(Self, 'clipBoundary', [0.0]);
  Fields.Add(FFdclipBoundary);
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
