{
  Copyright 2008-2016 Michalis Kamburelis.

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
  TAbstractLayerNode = class(TAbstractNode)
  public
    procedure CreateNode; override;

    private FFdIsPickable: TSFBool;
    public property FdIsPickable: TSFBool read FFdIsPickable;

    private FFdViewport: TSFNode;
    public property FdViewport: TSFNode read FFdViewport;
  end;

  TAbstractViewportNode = class(TAbstractX3DGroupingNode)
  public
    procedure CreateNode; override;
  end;

  TLayerNode = class(TAbstractLayerNode)
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
  end;

  TLayerSetNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdActiveLayer: TSFInt32;
    public property FdActiveLayer: TSFInt32 read FFdActiveLayer;

    private FFdLayers: TMFNode;
    public property FdLayers: TMFNode read FFdLayers;

    private FFdOrder: TMFInt32;
    public property FdOrder: TMFInt32 read FFdOrder;
  end;

  TViewportNode = class(TAbstractX3DGroupingNode, IAbstractBoundedObject)
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdClipBoundary: TMFFloat;
    public property FdClipBoundary: TMFFloat read FFdClipBoundary;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractLayerNode.CreateNode;
begin
  inherited;

  FFdIsPickable := TSFBool.Create(Self, 'isPickable', true);
  AddField(FFdIsPickable);

  FFdViewport := TSFNode.Create(Self, 'viewport', [TAbstractViewportNode]);
  AddField(FFdViewport);

  DefaultContainerField := 'layers';
end;

procedure TAbstractViewportNode.CreateNode;
begin
  inherited;
end;

procedure TLayerNode.CreateNode;
begin
  inherited;

  FEventAddChildren := TMFNodeEvent.Create(Self, 'addChildren', true);
  AddEvent(FEventAddChildren);

  FEventRemoveChildren := TMFNodeEvent.Create(Self, 'removeChildren', true);
  AddEvent(FEventRemoveChildren);

  FFdChildren := TMFNode.Create(Self, 'children', [TAbstractChildNode]);
  AddField(FFdChildren);
end;

class function TLayerNode.ClassX3DType: string;
begin
  Result := 'Layer';
end;

class function TLayerNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TLayerSetNode.CreateNode;
begin
  inherited;

  FFdActiveLayer := TSFInt32.Create(Self, 'activeLayer', 0);
  AddField(FFdActiveLayer);
  { X3D specification comment: (-Inf,Inf) }

  FFdLayers := TMFNode.Create(Self, 'layers', [TAbstractLayerNode]);
  AddField(FFdLayers);

  FFdOrder := TMFInt32.Create(Self, 'order', [0]);
  AddField(FFdOrder);
  { X3D specification comment: (0,Inf) }

  DefaultContainerField := 'children';
end;

class function TLayerSetNode.ClassX3DType: string;
begin
  Result := 'LayerSet';
end;

class function TLayerSetNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure TViewportNode.CreateNode;
begin
  inherited;

  FFdClipBoundary := TMFFloat.Create(Self, 'clipBoundary', [0.0]);
  AddField(FFdClipBoundary);
  { X3D specification comment: 1 0 1  [0,1] }
end;

class function TViewportNode.ClassX3DType: string;
begin
  Result := 'Viewport';
end;

class function TViewportNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassX3DType);
end;

procedure RegisterLayeringNodes;
begin
  NodesManager.RegisterNodeClasses([
    TLayerNode,
    TLayerSetNode,
    TViewportNode
  ]);
end;

{$endif read_implementation}
