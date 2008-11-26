{
  Copyright 2003-2004,2007-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(Implementacja prostej klasy @link(TVRMLLightSet)
  = pojemnika na tablice
  Lights: TDynActiveLight generowana zawsze z jednego RootNode.
  Podstawa dla @link(VRMLLightSetGL) ale czasem przydatne tez bez tego -
  np. w genLightMap.)
}

unit VRMLLightSet;

interface

uses VectorMath, VRMLNodes;

{ Lights are inited in constructor and if you change anything inside the
  RootNode you should regenerate Lights by calling CalculateLights.

  Remember that on TDynActiveLightArray light nodes are only referenced,
  so you can't free RootNode after creating this object if you plan
  to use generated Lights somewhere. So RootNode must be valid for the lifetime
  of this object. So the easiest situation is if you can let OwnsRootNode to be
  true (this object than frees RootNode in his destructor).
}
type
  TVRMLLightSet = class
  private
    FOwnsRootNode: boolean;
    FRootNode: TVRMLNode;
    FLights: TDynActiveLightArray;
    procedure AddToLights(
      Node: TVRMLNode; State: TVRMLGraphTraverseState;
      ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
  public
    { if OwnsRootNode then in destructor we will call RootNode.Free }
    property OwnsRootNode: boolean read FOwnsRootNode write FOwnsRootNode;
    property RootNode: TVRMLNode read FRootNode;

    { contents of this object are readonly from outside.
      Regenerate them manually (when you change anything in RootNode)
      by calling CalculateLights. }
    property Lights: TDynActiveLightArray read FLights;
    { recalculate Lights property (based on RootNode) }
    procedure CalculateLights; virtual;

    constructor Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TVRMLLightSet ------------------------------------------------------------ }

procedure TVRMLLightSet.AddToLights(
  Node: TVRMLNode; State: TVRMLGraphTraverseState;
  ParentInfo: PTraversingInfo; var TraverseIntoChildren: boolean);
begin
  Lights.AppendItem((Node as TVRMLLightNode).CreateActiveLight(State));
end;

procedure TVRMLLightSet.CalculateLights;
begin
 Lights.Length := 0;
 RootNode.Traverse(TVRMLLightNode, @AddToLights);
end;

constructor TVRMLLightSet.Create(ARootNode: TVRMLNode; AOwnsRootNode: boolean);
begin
 inherited Create;
 FRootNode := ARootNode;
 FOwnsRootNode := AOwnsRootNode;

 FLights := TDynActiveLightArray.Create;

 CalculateLights;
end;

destructor TVRMLLightSet.Destroy;
begin
 if OwnsRootNode then FreeAndNil(FRootNode);
 FLights.Free;
 inherited;
end;

end.