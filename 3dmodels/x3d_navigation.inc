{
  Copyright 2008 Michalis Kamburelis.

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

{$ifdef read_interface}

  { X3D LOD node.

    This is implemented as a different class than VRML 2.0 LOD
    node (TNodeLOD_2) as VRML 2.0 LOD is not perfectly compatible.
    Name changes from "level" to "children".
    See TNodeSwitch_3 for more reasoning. }
  TNodeLOD_3 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdcenter: TSFVec3f;
    public property Fdcenter: TSFVec3f read FFdcenter;

    private FFdrange: TMFFloat;
    public property Fdrange: TMFFloat read FFdrange;

    private FFdforceTransitions: TSFBool;
    public property FdforceTransitions: TSFBool read FFdforceTransitions;

    { Event: SFInt32, out } { }
    private FEventlevel_changed: TVRMLEvent;
    public property Eventlevel_changed: TVRMLEvent read FEventlevel_changed;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

class function TNodeLOD_3.ClassNodeTypeName: string;
begin
  Result := 'LOD';
end;

class function TNodeLOD_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeLOD_3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdcenter := TSFVec3f.Create('center', ZeroVector3Single);
  FFdcenter.Exposed := false;
  Fields.Add(FFdcenter);

  FFdrange := TMFFloat.Create('range', []);
  FFdrange.Exposed := false;
  Fields.Add(FFdrange);

  FFdforceTransitions := TSFBool.Create('forceTransitions', false);
  FFdforceTransitions.Exposed := false;
  Fields.Add(FFdforceTransitions);

  FEventlevel_changed := TVRMLEvent.Create('level_changed', TSFInt32, false);
  Events.Add(FEventlevel_changed);

  DefaultContainerField := 'children';
end;

class function TNodeLOD_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

function TNodeLOD_3.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeLOD_3.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdChildren.Items.Count >= 1 then
    Func(Self, FdChildren.Items[0]);
end;

{$endif read_implementation}
