{
  Copyright 2003-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLEventsProcessor (ancestor of TVRMLScene, with which the nodes
  implementations interact) and related types. }

{$ifdef read_interface}

  { Describe what visible thing changed
    for TVRMLEventsProcessor.VisibleSceneChange. }
  TVisibleSceneChange = (
    { Something visible in the scene geometry changed.
      "Geometry" means that this is applicable only to actual 3D shape
      changes. (Think about "does depth buffer from some point in space
      changes" --- this is actually why we have separate prVisibleSceneGeometry
      and prVisibleSceneNonGeometry for now, as GeneratedShadowMap
      does need to be updated only on geometry changes.) So it's not applicable
      when only light conditions, materials, textures and such change. }
    prVisibleSceneGeometry,
    { Something visible,  but not geometry, in the scene changed. }
    prVisibleSceneNonGeometry,
    { Viewer (the settings passed to ViewerChanged) changed. }
    prViewer);
  TVisibleSceneChanges = set of TVisibleSceneChange;

  TVRMLBindableStackBasic = class(TVRMLNodesList)
  public
    { Handle set_bind event send to given Node.
      This always generates appropriate events. }
    procedure Set_Bind(Node: TNodeX3DBindableNode; const Value: boolean); virtual; abstract;
  end;

  { Basic abstract object that can handle VRML/X3D events. }
  TVRMLEventsProcessor = class(T3D)
  protected
    procedure ExecuteCompiledScript(const HandlerName: string;  ReceivedValue: TVRMLField); virtual; abstract;
  public
    function GetTime: TVRMLTime; virtual; abstract;
    function GetViewpointStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetNavigationInfoStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetBackgroundStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetFogStack: TVRMLBindableStackBasic; virtual; abstract;

    procedure ChangedFields(Node: TVRMLNode; FieldOrEvent: TVRMLFieldOrEvent); virtual; abstract;
    procedure VisibleSceneChange(const Changes: TVisibleSceneChanges); virtual; abstract;
  end;
{$endif read_interface}

{$ifdef read_implementation}
{$endif read_implementation}
