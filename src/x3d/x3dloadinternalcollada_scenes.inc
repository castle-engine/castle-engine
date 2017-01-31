{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_implementation_nested}
var
  { List of Collada visual scenes, for <visual_scene> Collada elements.
    Every visual scene is X3D TAbstractX3DGroupingNode instance.
    This is for Collada >= 1.4.x (for Collada < 1.4.x,
    the <scene> element is directly placed as a rendered scene). }
  VisualScenes: TX3DNodeList;

{ Read <visual_scene>. Obtained scene X3D node is added both
  to VisualScenes list and VisualScenesSwitch.choice. }
procedure ReadVisualScene(VisualScenesSwitch: TSwitchNode;
  VisualSceneElement: TDOMElement);
var
  VisualSceneId: string;
  Group: TGroupNode;
begin
  if not VisualSceneElement.AttributeString('id', VisualSceneId) then
    VisualSceneId := '';

  Group := TGroupNode.Create(VisualSceneId, BaseUrl);
  VisualScenes.Add(Group);
  VisualScenesSwitch.FdChildren.Add(Group);

  ReadNodesSequence(Group, VisualSceneElement);
end;

{ Read <library_visual_scenes> from Collada 1.4.x }
procedure ReadLibraryVisualScenes(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
  LibraryId: string;
  VisualScenesSwitch: TSwitchNode;
begin
  if not LibraryElement.AttributeString('id', LibraryId) then
    LibraryId := '';

  { Library of visual scenes is simply a X3D Switch, with each
    scene inside as one choice. This way we export to X3D all
    scenes from Collada, even those not chosen as current scene.
    That's good --- it's always nice to keep some data when
    converting. }

  VisualScenesSwitch := TSwitchNode.Create(LibraryId, BaseUrl);
  ResultModel.FdChildren.Add(VisualScenesSwitch);

  I := LibraryElement.ChildrenIterator('visual_scene');
  try
    while I.GetNext do
      ReadVisualScene(VisualScenesSwitch, I.Current);
      { other I.Current.TagName not supported for now }
  finally FreeAndNil(I) end;
end;

{ Read <scene> element. }
procedure ReadSceneElement(SceneElement: TDOMElement);
var
  SceneId: string;

  procedure Collada14;
  var
    InstanceVisualScene: TDOMElement;
    VisualSceneId: string;
    VisualScene: TX3DNode;
  begin
    InstanceVisualScene := SceneElement.ChildElement(
      'instance_visual_scene', false);
    if InstanceVisualScene <> nil then
    begin
      if InstanceVisualScene.AttributeString('url', VisualSceneId) and
         SCharIs(VisualSceneId, 1, '#') then
      begin
        Delete(VisualSceneId, 1, 1);
        VisualScene := VisualScenes.FindName(VisualSceneId);
        if VisualScene = nil then
          WritelnWarning('Collada', Format('<instance_visual_scene> instantiates non-existing ' +
            '<visual_scene> element "%s"', [VisualSceneId])) else
          ResultModel.FdChildren.Add(VisualScene);
      end;
    end;
  end;

  procedure Collada13;
  var
    Group: TGroupNode;
  begin
    Group := TGroupNode.Create(SceneId, BaseUrl);
    ResultModel.FdChildren.Add(Group);

    ReadNodesSequence(Group, SceneElement);
  end;

begin
  if not SceneElement.AttributeString('id', SceneId) then
    SceneId := '';

  { <scene> element is different in two Collada versions, it's most clear
    to just branch and do different procedure depending on Collada version. }

  if Version14 then
    Collada14 else
    Collada13;
end;

{$endif read_implementation_nested}
