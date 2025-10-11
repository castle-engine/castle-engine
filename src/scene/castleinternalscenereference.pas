{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Customize animation in a reference scene for some calculations
  (TCastleSceneInstance). }
unit CastleInternalSceneReference;

{$I castleconf.inc}

interface

uses Generics.Collections,
  CastleShapes;

type
  TInstanceTransformList = class;

  { Current transformation maintained by @link(TCastleSceneInstance).
    Equivalent to @link(TTransformNode) in @link(TShapeTreeTransform),
    but with customized current state. }
  // TODO: Need all TShapeTree equivalents, with TInstanceTree, TInstanceTreeGroup etc.?
  TInstanceTransform = class
    ShapeTree: TShapeTreeTransform;
    Translation: TVector3;
    Rotation: TQuaternion;
    Scale: TVector3;
    { Children order just like ShapeTree children. }
    Children: TInstanceTransformList;
  end;

  TInstanceTransformList = class(specialize TObjectList<TInstanceTransform>);

  { Customize played animation and animation time in a reference
    @link(TCastleScene) in @link(Reference).

    Assumes that @link(TCastleScene) is unchanged throughout the life of this.
    In particular, we may keep references to some nodes,
    and just assume that they keep existing as long as TCastleSceneInstance exists.
    We also keep references to @link(TCastleSceneCore.Shapes) tree and parts
    of it.

    For now, this just allows to calculate bounding boxes in such
    "customized scene".
    In the future, it may be exposed and allow to render the scene
    with a customized animation. See roadmap item
    @url(https://castle-engine.io/roadmap#_allow_animating_selected_node_graph_properties_without_changing_the_graph_to_be_able_to_reuse_graphs
    Allow animating selected node graph properties, without changing the graph, to be able to reuse graphs). }
  TCastleSceneInstance = class {in the future: (TCastleTransform)}
  public
    Shapes: TInstanceTransform;
    FReference: TCastleScene;
    FReferenceObserver: ...;
    procedure ForceAnimationPose(const AnimationName: String;
      const TimeInAnimation: TFloatTime);
  published
    { Original scene that we customize in this TCastleSceneInstance. }
    property Reference: TCastleScene read FReference write SetReference;
  end;

implementation

end.
