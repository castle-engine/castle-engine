{
  Copyright 2010-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Additional 3D objects derived from TCastleTransform (TAlive, T3DMoving...). }
unit Castle3D deprecated 'use CastleTransform, CastleTransformExtra';

{$I castleconf.inc}

interface

uses SysUtils, Classes, Math, Generics.Collections, Kraft,
  CastleVectors, CastleFrustum, CastleBoxes, CastleClassUtils, CastleKeysMouse,
  CastleRectangles, CastleUtils, CastleTimeUtils,
  CastleSoundEngine, CastleSectors, CastleCameras, CastleTriangles,
  CastleTransform, CastleTransformExtra, CastleScene;

type
  T3D                         = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';
  T3DList                     = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';
  T3DCustomTranslated         = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';
  T3DTranslated               = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';
  T3DCustomTransform          = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';
  T3DTransform                = CastleTransform.TCastleTransform deprecated 'use TCastleTransform from CastleTransform unit';

  T3DOrient = class(CastleTransform.TCastleTransform)
  private
    FNavigation: TCastleWalkNavigation;
  protected
    procedure ChangedTransform; override;
  public
    { Camera that is automatically synchronized with this 3D object. }
    property Camera: TCastleWalkNavigation read FNavigation; deprecated 'instead of using this, better define your own TCastleWalkNavigation instance synchronized with this TCastleTransform';
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    constructor Create(AOwner: TComponent); override;
  end deprecated 'use TCastleTransform from CastleTransform unit';

  T3DListCore                 = CastleTransform.TCastleTransformList;
  T3DWorld                    = TCastleRootTransform;
  TCollisionDetails           = CastleTransform.TCollisionDetails;
  TRayCollision               = CastleTransform.TRayCollision;
  TRayCollisionNode           = CastleTransform.TRayCollisionNode;
  PRayCollisionNode           = CastleTransform.PRayCollisionNode;
  TRemoveType                 = CastleTransform.TRemoveType;
  TPrepareResourcesOption     = CastleTransform.TPrepareResourcesOption;
  TPrepareResourcesOptions    = CastleTransform.TPrepareResourcesOptions;
  TAbstractLightInstancesList = CastleTransform.TAbstractLightInstancesList;
  TRenderFromViewFunction     = CastleTransform.TRenderFromViewFunction;
  TRenderingPass              = CastleTransform.TInternalRenderingPass;
  TRenderParams               = CastleTransform.TRenderParams;
  TBaseShadowVolumeRenderer   = CastleTransform.TBaseShadowVolumeRenderer;
  TVisibleChange              = CastleTransform.TVisibleChange;
  TVisibleChanges             = CastleTransform.TVisibleChanges;
  TRenderStatistics           = CastleTransform.TRenderStatistics;
  TRigidBody                  = CastleTransform.TRigidBody;
  TCollider                   = CastleTransform.TCollider;
  TSphereCollider             = CastleTransform.TSphereCollider;
  TBoxCollider                = CastleTransform.TBoxCollider;
  TPlaneCollider              = CastleTransform.TPlaneCollider;

  T3DMoving       = CastleTransformExtra.TCastleMoving deprecated 'use TCastleMoving';
  T3DLinearMoving = CastleTransformExtra.TCastleLinearMoving deprecated 'use TCastleLinearMoving';
  TAlive          = CastleTransformExtra.TCastleAlive deprecated 'use TCastleAlive';

  T3DAlive = TAlive deprecated 'use TCastleAlive';

  T3DExistsEvent = function(const Item: T3D): boolean of object;

const
  MaxSingle = Math.MaxSingle;

  vcVisibleGeometry    = CastleTransform.vcVisibleGeometry;
  vcVisibleNonGeometry = CastleTransform.vcVisibleNonGeometry;
  prShadowVolume       = CastleTransform.prShadowVolume;
  prSpatial            = CastleTransform.prSpatial;
  prScreenEffects      = CastleTransform.prScreenEffects;
  prRender             = CastleTransform.prRenderSelf;
  prBackground         = CastleTransform.prBackground;
  prBoundingBox        = CastleTransform.prBoundingBox;
  rtNone               = CastleTransform.rtNone;
  rtRemove             = CastleTransform.rtRemove;
  rtRemoveAndFree      = CastleTransform.rtRemoveAndFree;
  otUpYDirectionMinusZ = CastleTransform.otUpYDirectionMinusZ;
  otUpZDirectionMinusY = CastleTransform.otUpZDirectionMinusY;
  otUpZDirectionX      = CastleTransform.otUpZDirectionX;

var
  { Creatures, items and possibly other 3D stuff may look at these variables
    to display additional features of 3D objects, helpful to debug collisions,
    AI and other things.
    @groupBegin }
  RenderDebug3D: boolean = false
    {$ifndef CASTLE_ENGINE_LAZARUS_PACKAGE}
    // workaround for Lazarus <= 1.8.0: CodeTools cannot parse this
    deprecated 'use Player.RenderDebug, TCreature.RenderDebug, TItemOnWorld.RenderDebug'
    {$endif};
  RenderDebugCaptions: boolean = false
    {$ifndef CASTLE_ENGINE_LAZARUS_PACKAGE}
    // workaround for Lazarus <= 1.8.0: CodeTools cannot parse this
    deprecated 'use TCreature.RenderDebug'
    {$endif};
  { @groupEnd }

implementation

uses CastleLog;

{ T3DOrient ------------------------------------------------------------------ }

constructor T3DOrient.Create(AOwner: TComponent);
begin
  inherited;
  FNavigation := TCastleWalkNavigation.Create(Self);
end;

procedure T3DOrient.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
var
  P, D, U: TVector3;
begin
  inherited;
  // synchronize Position, Direction, Up *from* Camera
  {$warnings off} // knowingly using deprecated
  FNavigation.Camera.GetView(P, D, U);
  {$warnings on}
  SetView(P, D, U);
end;

procedure T3DOrient.ChangedTransform;
var
  P, D, U: TVector3;
begin
  inherited;
  // synchronize Position, Direction, Up *to* Camera
  GetView(P, D, U);
  {$warnings off} // knowingly using deprecated
  FNavigation.Camera.SetView(P, D, U);
  {$warnings on}
end;

end.
