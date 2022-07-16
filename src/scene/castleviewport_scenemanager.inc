{%MainUnit castleviewport.pas}
{
  Copyright 2013-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
//type (continue exising type declaration)

  { Deprecated way to manage transformations and scenes.
    To upgrade:
    - use @link(TCastleViewport)
    - set FullSize, AutoCamera and AutoNavigation to true. }
  TCastleSceneManager = class(TCastleAutoNavigationViewport)
  strict private
    FDefaultViewport: boolean;
    {$warnings off} // using deprecated in deprecated
    FViewports: TCastleViewportList;
    {$warnings on}

    function GetMoveLimit: TBox3D;
    procedure SetMoveLimit(const Value: TBox3D);
    function GetTimeScale: Single;
    procedure SetTimeScale(const Value: Single);
    procedure SetDefaultViewport(const Value: boolean);
    function GetMainCamera: TCastleCamera;
    procedure SetMainCamera(const Value: TCastleCamera);
    function GetMainSceneInternal: TCastleScene;
    procedure SetMainScene(const Value: TCastleScene);
    function GetUseHeadlight: TUseHeadlight;
    procedure SetUseHeadlight(const Value: TUseHeadlight);
    function GetHeadlightNode: TAbstractLightNode;
    procedure SetHeadlightNode(const Value: TAbstractLightNode);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Render; override;

    { Limit the movement allowed by @link(TCastleAbstractRootTransform.WorldMoveAllowed).
      Ignored when empty (default).
      @seealso TCastleAbstractRootTransform.MoveLimit }
    property MoveLimit: TBox3D read GetMoveLimit write SetMoveLimit;
      {$ifdef FPC}deprecated 'use Items.MoveLimit';{$endif}

    { List of viewports connected to this scene manager.
      This contains all TCastleViewport instances that have
      TCastleViewport.SceneManager set to us. Also it contains Self
      (this very scene manager) if and only if DefaultViewport = @true
      (because when DefaultViewport, scene manager acts as an
      additional viewport too).

      This list is read-only from the outside! It's automatically managed
      in this unit (when you change TCastleViewport.SceneManager
      or TCastleSceneManager.DefaultViewport, we automatically update this list
      as appropriate). }
    property Viewports: TCastleViewportList read FViewports;

    { Up vector, according to gravity. Gravity force pulls in -GravityUp direction. }
    function GravityUp: TVector3; deprecated 'use Camera.GravityUp';

    { See @link(TCastleRootTransform.HeadlightNode). }
    property HeadlightNode: TAbstractLightNode
      read GetHeadlightNode write SetHeadlightNode;
      {$ifdef FPC}deprecated 'use Items.HeadlightNode';{$endif}

    { See @link(TCastleAbstractRootTransform.MainCamera). }
    property MainCamera: TCastleCamera read GetMainCamera write SetMainCamera;
      {$ifdef FPC}deprecated 'use Items.MainCamera';{$endif}

    { See @link(TCastleAbstractRootTransform.PhysicsProperties). }
    function PhysicsProperties: TPhysicsProperties;
      deprecated 'use Items.PhysicsProperties';

    { See @link(TCastleAbstractRootTransform.TimeScale). }
    property TimeScale: Single read GetTimeScale write SetTimeScale
      {$ifdef FPC}default 1{$endif};
      {$ifdef FPC}deprecated 'use Items.TimeScale';{$endif}

    { See @link(TCastleRootTransform.MainScene). }
    property MainScene: TCastleScene read GetMainSceneInternal write SetMainScene;
      {$ifdef FPC}deprecated 'use Items.MainScene';{$endif}

    { See @link(TCastleRootTransform.UseHeadlight). }
    property UseHeadlight: TUseHeadlight
      read GetUseHeadlight write SetUseHeadlight default hlMainScene;
      {$ifdef FPC}deprecated 'use Items.UseHeadlight';{$endif}
  published
    { Should we render the 3D world in a default viewport that covers
      the whole window. This is usually what you want. For more complicated
      uses, you can turn this off, and use explicit TCastleViewport
      (connected to this scene manager by TCastleViewport.SceneManager property)
      for making your world visible. }
    property DefaultViewport: boolean
      read FDefaultViewport write SetDefaultViewport default true;

    property FullSize default true;
    property AutoCamera default true;
    property AutoNavigation default true;
  end {$ifdef FPC}deprecated 'use TCastleViewport to render scenes. To have the same initial behavior, set FullSize, AutoCamera and AutoNavigation to true'{$endif};

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleSceneManager -------------------------------------------------------- }

constructor TCastleSceneManager.Create(AOwner: TComponent);
begin
  inherited;

  FDefaultViewport := true;
  FullSize := true;
  AutoNavigation := true;
  AutoCamera := true;

  {$warnings off} // using deprecated in deprecated
  FViewports := TCastleViewportList.Create(false);
  {$warnings on}
  FViewports.SceneManager := Self;
  if DefaultViewport then FViewports.Add(Self);
end;

destructor TCastleSceneManager.Destroy;
var
  I: Integer;
begin
  if FViewports <> nil then
  begin
    for I := 0 to FViewports.Count - 1 do
      if FViewports[I] is TCastleViewport then
      begin
        {$ifdef FPC}
        {$warnings off} // using deprecated in deprecated
        Assert(
          (TCastleViewport(FViewports[I]).SceneManager = Self) or
          (TCastleViewport(FViewports[I]).SceneManager = nil)
        );
        {$warnings on}
        {$endif}

        { Set SceneManager by direct field (FSceneManager),
          otherwise TCastleViewport.SetSceneManager would try to update
          our Viewports list, that we iterate over right now... }
        TCastleViewport(FViewports[I]).FSceneManager := nil;
      end;
    FreeAndNil(FViewports);
  end;

  inherited;
end;

procedure TCastleSceneManager.Render;
begin
  if not DefaultViewport then Exit;
  inherited;
end;

procedure TCastleSceneManager.SetDefaultViewport(const Value: boolean);
begin
  if Value <> FDefaultViewport then
  begin
    FDefaultViewport := Value;
    if DefaultViewport then
      Viewports.Add(Self)
    else
      Viewports.Remove(Self);
  end;
end;

function TCastleSceneManager.GetMainCamera: TCastleCamera;
begin
  Result := Items.MainCamera;
end;

procedure TCastleSceneManager.SetMainCamera(const Value: TCastleCamera);
begin
  Items.MainCamera := Value;
end;

function TCastleSceneManager.GetMainSceneInternal: TCastleScene;
begin
  {$warnings off} // using deprecated in deprecated
  Result := Items.MainScene;
  {$warnings on}
end;

procedure TCastleSceneManager.SetMainScene(const Value: TCastleScene);
begin
  {$warnings off} // using deprecated in deprecated
  Items.MainScene := Value;
  {$warnings on}
end;

function TCastleSceneManager.GetUseHeadlight: TUseHeadlight;
begin
  {$warnings off} // using deprecated in deprecated
  Result := Items.UseHeadlight;
  {$warnings on}
end;

procedure TCastleSceneManager.SetUseHeadlight(const Value: TUseHeadlight);
begin
  {$warnings off} // using deprecated in deprecated
  Items.UseHeadlight := Value;
  {$warnings on}
end;

function TCastleSceneManager.GravityUp: TVector3;
begin
  Result := Camera.GravityUp;
end;

function TCastleSceneManager.GetHeadlightNode: TAbstractLightNode;
begin
  Result := Items.HeadlightNode;
end;

procedure TCastleSceneManager.SetHeadlightNode(const Value: TAbstractLightNode);
begin
  Items.HeadlightNode := Value;
end;

function TCastleSceneManager.GetMoveLimit: TBox3D;
begin
  Result := Items.MoveLimit;
end;

procedure TCastleSceneManager.SetMoveLimit(const Value: TBox3D);
begin
  Items.MoveLimit := Value;
end;

function TCastleSceneManager.PhysicsProperties: TPhysicsProperties;
begin
  Result := Items.PhysicsProperties;
end;

function TCastleSceneManager.GetTimeScale: Single;
begin
  Result := Items.TimeScale;
end;

procedure TCastleSceneManager.SetTimeScale(const Value: Single);
begin
  Items.TimeScale := Value;
end;

{$endif read_implementation}
