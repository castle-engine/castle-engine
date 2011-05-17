{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Lights initialized from VRML/X3D file (TVRMLLightSet). }
unit VRMLLightSet;

interface

uses VectorMath, VRMLNodes;

type
  { Light set initialized from 3d model file (like VRML).

    Lights list is calculated in the constructor.
    If you change anything inside the
    RootNode you should recalculate Lights by calling CalculateLights.

    Remember that on TDynLightInstanceArray light nodes are only referenced,
    so you can't free RootNode after creating this object if you plan
    to use generated Lights somewhere. So RootNode must be valid for the lifetime
    of this object. So the easiest situation is if you can let OwnsRootNode to be
    true (this object than frees RootNode in his destructor). }
  TVRMLLightSet = class
  private
    FOwnsRootNode: boolean;
    FRootNode: TVRMLNode;
    FLights: TDynLightInstanceArray;
    procedure AddToLights(
      Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  public
    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
    destructor Destroy; override;

    { If OwnsRootNode then in destructor we will call RootNode.Free. }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode;
    property RootNode: TVRMLNode read FRootNode;

    { Lights list.
      Contents of this object are readonly from outside.
      Regenerate them manually (when you change anything in RootNode)
      by calling CalculateLights. }
    property Lights: TDynLightInstanceArray read FLights;

    { Recalculate Lights property (based on RootNode). }
    procedure CalculateLights; virtual;

    { Detect position/direction of the main light that produces shadows.
      This is useful when you want to make shadows on the scene
      from only a single light, but your scene has many lights.

      The main light is simply one with both @code(kambiShadows) and
      @code(kambiShadowsMain) fields set to @true. See
      [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php#section_ext_shadows]
      for more info.
      If no light with kambiShadows = kambiShadowsMain = TRUE
      is present then this function returns @false,
      since AMainLightPosition cannot be calculated.

      AMainLightPosition[3] is always set to 1
      (positional light) or 0 (indicates that this is a directional light).

      @seealso TVRMLScene.MainLightForShadows
      @seealso TKamAbstractViewport.MainLightForShadows }
    function MainLightForShadows(
      out AMainLightPosition: TVector4Single): boolean;
  end;

implementation

uses SysUtils;

{ TVRMLLightSet ------------------------------------------------------------ }

constructor TVRMLLightSet.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
  inherited Create;
  FRootNode := ARootNode;
  FOwnsRootNode := AOwnsRootNode;

  FLights := TDynLightInstanceArray.Create;

  CalculateLights;
end;

destructor TVRMLLightSet.Destroy;
begin
  if OwnsRootNode then FreeAndNil(FRootNode);
  FLights.Free;
  inherited;
end;

procedure TVRMLLightSet.AddToLights(
  Node: TVRMLNode; StateStack: TVRMLGraphTraverseStateStack;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
begin
  Lights.Add((Node as TNodeX3DLightNode).CreateLightInstance(StateStack.Top));
end;

procedure TVRMLLightSet.CalculateLights;
begin
  Lights.Length := 0;
  RootNode.Traverse(TNodeX3DLightNode, @AddToLights);
end;

function TVRMLLightSet.MainLightForShadows(
  out AMainLightPosition: TVector4Single): boolean;
var
  MyLightNum: Integer;
  L: PLightInstance;
begin
  { Find main light, set Result and AMainLightPosition. }
  Result := false;
  L := Lights.Pointers[0];
  for MyLightNum := 0 to Lights.Count - 1 do
  begin
    if L^.Node.FdKambiShadows.Value and
       L^.Node.FdKambiShadowsMain.Value then
    begin
      Result := true;
      if L^.Node is TVRMLPositionalLightNode then
        AMainLightPosition := Vector4Single(L^.Location, 1) else
      if L^.Node is TVRMLDirectionalLightNode then
        AMainLightPosition := Vector4Single(L^.Direction, 0) else
        raise Exception.CreateFmt('TVRMLGLLightSet.MainLightForShadows: ' +
          'light node "%s" cannot be used to cast shadows, it has no position ' +
          'and no direction', [L^.Node.NodeTypeName]);
      Break;
    end;
    Inc(L);
  end;
end;

end.
