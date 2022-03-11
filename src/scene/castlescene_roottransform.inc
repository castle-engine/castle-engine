{%MainUnit castlescene.pas}
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

{ TCastleRootTransform (with MainScene property that refers to TCastleScene). }

{$ifdef read_interface}

type
  { Possible value of @link(TCastleRootTransform.UseHeadlight). }
  TUseHeadlight = (
    { Always show a headlight.
      The headlight properties (color, intensity, shape)
      are taken from @link(TCastleRootTransform.HeadlightNode). }
    hlOn,

    { Never show a headlight. }
    hlOff,

    { Show a headlight following the @link(TCastleRootTransform.MainScene)
      properties.

      If @link(TCastleRootTransform.MainScene) is @nil,
      there is no headlight.

      If @link(TCastleRootTransform.MainScene) is assigned,
      the headlight is shown if
      @link(TCastleSceneCore.HeadlightOn MainScene.HeadlightOn)
      is @true.
      @link(TCastleSceneCore.HeadlightOn MainScene.HeadlightOn) in turn
      is configutable, and by default looks at X3D NavigationInfo.headlight field.
      (If no NavigationInfo node is present, the headlight is shown,
      as NavigationInfo.headlight is @true by default.)

      The headlight properties (color, intensity, shape)
      follow @link(TCastleSceneCore.CustomHeadlight MainScene.CustomHeadlight),
      which can be customized in an X3D file using
      https://castle-engine.io/x3d_implementation_navigation_extensions.php#section_ext_headlight .
      If no @link(TCastleSceneCore.CustomHeadlight MainScene.CustomHeadlight)
      is set, we use @link(TCastleRootTransform.HeadlightNode). }
    hlMainScene
  );

  { Root of transformations and scenes (tree of TCastleTransform and TCastleScene). }
  TCastleRootTransform = class(TCastleAbstractRootTransform)
  strict private
    FMainScene: TCastleScene;
    { We observe freeing of MainScene using MainSceneObserver,
      not using direct MainScene.FreeNotification(Self).
      This way our RemoveFreeNotification doesn't accidentally remove the notification
      which should remain because of other reasons
      (when MainScene is our direct child, or when MainScene is part of our world,
      which is actually almost always the case). }
    MainSceneObserver: TFreeNotificationObserver;
    FUseHeadlight: TUseHeadlight;
    FHeadlightNode: TAbstractLightNode;
    FInternalScenesCastGlobalLights: TCastleSceneList;
    procedure SetMainScene(const Value: TCastleScene);
    procedure MainScene_BoundViewpointChanged(Sender: TObject);
    procedure MainScene_BoundViewpointVectorsChanged(Sender: TObject);
    procedure MainScene_BoundNavigationInfoChanged(Sender: TObject);
    function GetHeadlightNode: TAbstractLightNode;
    procedure SetHeadlightNode(const Node: TAbstractLightNode);
    procedure MainSceneFreeNotification(const Sender: TFreeNotificationObserver);
  private
    procedure RegisterCastGlobalLights(const S: TCastleScene);
    procedure UnregisterCastGlobalLights(const S: TCastleScene);
  protected
    function IgnoreTransparentItem(
      const Sender: TObject; const Triangle: PTriangle): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;

    { @exclude
      TCastleScene instances that have TCastleScene.CastGlobalLights.
      May be @nil, equivalent to empty. }
    property InternalScenesCastGlobalLights: TCastleSceneList read FInternalScenesCastGlobalLights;

    { Determines the headlight look, if we use a headlight
      (which is determined by the algorithm described at @link(UseHeadlight) and
      @link(TUseHeadlight)).
      By default it's a simplest directional headlight,
      but you can customize it, and thus you can use a point light
      or a spot light for a headlight.
      Just like https://castle-engine.io/x3d_implementation_navigation_extensions.php#section_ext_headlight .

      This is never @nil.
      Assigning here @nil simply causes us to recreate it using
      the simplest directional headlight. }
    property HeadlightNode: TAbstractLightNode
      read GetHeadlightNode write SetHeadlightNode;

    { Headlight used to light the scene.

      Returns non-nil headlight node
      if the algorithm described at @link(UseHeadlight) and
      @link(TUseHeadlight) indicates we should use a headlight.
      Otherwise returns @nil, to indicate we do not show a headlight now.

      @exclude }
    function InternalHeadlight: TAbstractLightNode;
  published
    { The main scene of the world. It's not necessary to set this.
      It adds some optional features that require a notion of
      the "main" scene to make sense.

      The scene you set here @italic(must) also be added to our @link(Items).

      The MainScene is used for a couple of things:

      @unorderedList(
        @item(Determines initial @link(TCastleViewport.Camera) (if @link(TCastleViewport.AutoCamera))
          and @link(TCastleViewport.Navigation) (if @link(TCastleViewport.AutoNavigation)) properties.
          This includes camera position, orientation, projection.
          This includes navigation type (examine, walk, fly), move speed.
          These follow the X3D Viewpoint, OrthoViewpoint
          and NavigationInfo nodes defined inside the MainScene.

          Later changes to nodes Viewpoint/NavigatioInfo using X3D events inside
          MainScene may also influence the @link(TCastleViewport.Camera) and @link(TCastleViewport.Navigation).
          For example in X3D you can animate the Viewpoint position,
          and it will actually animate our @link(TCastleViewport.Camera) position.

          If both @link(TCastleViewport.AutoCamera) and @link(TCastleViewport.AutoNavigation) are @false,
          none of these things are applied.
        )

        @item(Determines what background is rendered.
          If the MainScene contains an X3D Background node (or other
          node descending from TAbstractBackgroundNode),
          it will be used.

          This is overridden if you set @link(TCastleViewport.Background)
          to non-nil. MainScene's background is ignored in this case.

          When neither MainScene background is present,
          nor @link(TCastleViewport.Background) is specified,
          then we render a background using @link(TCastleViewport.BackgroundColor).

          Note that when @link(TCastleViewport.Transparent) is @true,
          we never render any background (neither from MainScene,
          nor from @link(TCastleViewport.BackgroundColor)).
        )

        @item(Determines whether headlight is used if @link(UseHeadlight)
          is hlMainScene. The value of
          @link(TCastleSceneCore.HeadlightOn MainScene.HeadlightOn)
          then determines the headlight.
          The initial
          @link(TCastleSceneCore.HeadlightOn MainScene.HeadlightOn)
          value depends on the X3D NavigationInfo node inside MainScene.)

        @item(Determines the main light casting shadow volumes.)

        @item(Determines lights shining on all scenes,
          as this scene has by default @link(TCastleScene.CastGlobalLights)
          when you set it as MainScene.
          This can also be achieved on any other scene too,
          by setting @link(TCastleScene.CastGlobalLights).)

        @item(Determines fog on all scenes, if @link(TCastleViewport.UseGlobalFog).)

        @item(The screen effects defined inside MainScene (TScreenEffectNode)
          are automatically used.)

        @item(Main scene is informed about pointing device move,
          even when the pointing device is @italic(not) over this scene,
          and this scene is @italic(not) capturing pointing device events
          (the scene "capturing" events once you click on it).
        )
      )

      Freeing MainScene will automatically set this property to @nil. }
    property MainScene: TCastleScene read FMainScene write SetMainScene;

    { Whether the headlight is shown, see @link(TUseHeadlight) for possible values. }
    property UseHeadlight: TUseHeadlight
      read FUseHeadlight write FUseHeadlight default hlMainScene;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleRootTransform ------------------------------------------------------- }

constructor TCastleRootTransform.Create(AOwner: TComponent);
begin
  inherited;
  MainSceneObserver := TFreeNotificationObserver.Create(Self);
  MainSceneObserver.OnFreeNotification := {$ifdef FPC}@{$endif}MainSceneFreeNotification;
  FUseHeadlight := hlMainScene;
end;

destructor TCastleRootTransform.Destroy;
begin
  { unregister self from MainScene callbacs,
    make MainScene.RemoveFreeNotification(Self)... this is all
    done by SetMainScene(nil) already. }
  MainScene := nil;

  FreeIfUnusedAndNil(FHeadlightNode);
  FreeAndNil(FInternalScenesCastGlobalLights);
  inherited;
end;

procedure TCastleRootTransform.SetMainScene(const Value: TCastleScene);
begin
  if FMainScene <> Value then
  begin
    MainSceneObserver.Observed := Value;

    if FMainScene <> nil then
    begin
      FMainScene.OnBoundViewpointVectorsChanged := nil;
      FMainScene.OnBoundNavigationInfoFieldsChanged := nil;
      { Check FMainScene.*Stack <> nil,
        because this may happen from MainScene destruction notification,
        when *Stack is already freed. }
      if FMainScene.ViewpointStack <> nil then
        FMainScene.ViewpointStack.OnBoundChanged := nil;
      if FMainScene.NavigationInfoStack <> nil then
        FMainScene.NavigationInfoStack.OnBoundChanged := nil;
    end;

    FMainScene := Value;

    if FMainScene <> nil then
    begin
      FMainScene.OnBoundViewpointVectorsChanged := {$ifdef FPC}@{$endif}MainScene_BoundViewpointVectorsChanged;
      FMainScene.OnBoundNavigationInfoFieldsChanged := {$ifdef FPC}@{$endif}MainScene_BoundNavigationInfoChanged;
      FMainScene.ViewpointStack.OnBoundChanged := {$ifdef FPC}@{$endif}MainScene_BoundViewpointChanged;
      FMainScene.NavigationInfoStack.OnBoundChanged := {$ifdef FPC}@{$endif}MainScene_BoundNavigationInfoChanged;
    end;

    { Setting MainScene may change lights, if MainScene determines the headlight
      when UseHeadlight = hlMainScene.
      So we use vcVisibleNonGeometry.
    }
    VisibleChangeHere([vcVisibleNonGeometry]);
  end;
end;

procedure TCastleRootTransform.MainSceneFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  MainScene := nil;
end;

procedure TCastleRootTransform.MainScene_BoundViewpointChanged(Sender: TObject);
begin
  if (MainCamera <> nil) and
     Assigned(MainCamera.InternalOnSceneBoundViewpointChanged) then
              MainCamera.InternalOnSceneBoundViewpointChanged(Sender);
end;

procedure TCastleRootTransform.MainScene_BoundViewpointVectorsChanged(Sender: TObject);
begin
  if (MainCamera <> nil) and
     Assigned(MainCamera.InternalOnSceneBoundViewpointVectorsChanged) then
              MainCamera.InternalOnSceneBoundViewpointVectorsChanged(Sender);
end;

procedure TCastleRootTransform.MainScene_BoundNavigationInfoChanged(Sender: TObject);
begin
  if (MainCamera <> nil) and
     Assigned(MainCamera.InternalOnSceneBoundNavigationInfoChanged) then
              MainCamera.InternalOnSceneBoundNavigationInfoChanged(Sender);
end;

function TCastleRootTransform.GetHeadlightNode: TAbstractLightNode;
begin
  { HeadlightNode is never nil, so recreate it now if nil. }

  if FHeadlightNode = nil then
    { Nothing more needed, all DirectionalLight default properties
      are suitable for default headlight. }
    FHeadlightNode := TDirectionalLightNode.Create;

  Result := FHeadlightNode;
end;

procedure TCastleRootTransform.SetHeadlightNode(const Node: TAbstractLightNode);
begin
  if FHeadlightNode <> Node then
  begin
    FreeIfUnusedAndNil(FHeadlightNode);
    FHeadlightNode := Node;
  end;
end;

function TCastleRootTransform.InternalHeadlight: TAbstractLightNode;
begin
  Result := nil;

  case UseHeadlight of
    hlOn : Result := HeadlightNode;
    hlOff: Result := nil;
    hlMainScene:
      if (MainScene <> nil) and MainScene.HeadlightOn then
      begin
        Result := MainScene.CustomHeadlight;
        if Result = nil then
          Result := HeadlightNode;
        Assert(Result <> nil);
      end;
    {$ifndef COMPILER_CASE_ANALYSIS}
    else raise EInternalError.Create(2018081902);
    {$endif}
  end;
end;

function TCastleRootTransform.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'MainScene') or
     (PropertyName = 'UseHeadlight') then
      Result := [psBasic]
    else
      Result := inherited PropertySections(PropertyName);
end;

function TCastleRootTransform.IgnoreTransparentItem(
  const Sender: TObject; const Triangle: PTriangle): Boolean;
begin
  Result := Triangle^.IsTransparent;
end;

procedure TCastleRootTransform.RegisterCastGlobalLights(const S: TCastleScene);
begin
  if FInternalScenesCastGlobalLights = nil then
    FInternalScenesCastGlobalLights := TCastleSceneList.Create(false);
  FInternalScenesCastGlobalLights.Add(S);
end;

procedure TCastleRootTransform.UnregisterCastGlobalLights(const S: TCastleScene);
var
  I: Integer;
begin
  if FInternalScenesCastGlobalLights = nil then
    I := -1
  else
    I := FInternalScenesCastGlobalLights.IndexOf(S);

  if I = -1 then
  begin
    WritelnWarning('Scene called UnregisterCastGlobalLights, but it was not on the list InternalScenesCastGlobalLights');
    Exit;
  end;

  FInternalScenesCastGlobalLights.Delete(I);
end;

{$endif read_implementation}
