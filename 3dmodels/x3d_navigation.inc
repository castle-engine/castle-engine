{
  Copyright 2002-2008 Michalis Kamburelis.

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

  { LOD node.

    Compatibility node: this class is used for both VRML 97 and X3D.
    Although field name changed for X3D (in VRML 97, "children"
    was called "level"), this is handled seamlessly using our
    TVRMLFieldOrEvent.AddAlternativeField mechanism. In other words,
    programmer simply works with FdChildren field, and when reading/writing
    VRML file we take care to actually use either "children" or "level" name,
    depending on used VRML version. }
  TNodeLOD_2 = class(TNodeX3DGroupingNode)
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

constructor TNodeLOD_2.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FdChildren.AddAlternativeName('level', 2);

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

class function TNodeLOD_2.ClassNodeTypeName: string;
begin
  Result := 'LOD';
end;

class function TNodeLOD_2.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeLOD_2.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 2;
end;

function TNodeLOD_2.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeLOD_2.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  { For now we simply always use the best LOD version,
    avoiding whole issue of choosing proper LOD child. }
  if FdChildren.Items.Count >= 1 then
    Func(Self, FdChildren.Items[0]);
end;

function TNodeLOD_2.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

{$endif read_implementation}
