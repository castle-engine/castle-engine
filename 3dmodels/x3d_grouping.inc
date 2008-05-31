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

  { X3D Switch node.

    This is implemented as a different class than VRML 2.0 Switch
    node (TNodeSwitch_2) as VRML 2.0 Switch is not perfectly compatible.
    In particular, in X3D field name changed from "choice" to "children".
    This seems like a trivial change, but note that events, routes,
    scripts may all refer to the field name, so implementing a simple
    "alias field name" is actually non-trivial (and not worthy, if it
    can be fixed by simply making separate TNodeSwitch_3). }
  TNodeSwitch_3 = class(TNodeX3DGroupingNode)
  protected
    procedure DirectEnumerateActive(
      Func: TEnumerateChildrenFunction); override;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    private FFdwhichChoice: TSFInt32;
    public property FdwhichChoice: TSFInt32 read FFdwhichChoice;

    class function ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
      override;

    function ChildrenField: TMFNode; override;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  INodeX3DBoundedObject = interface(IVRMLNode)
    property FdbboxCenter: TSFVec3f { read GetFdbboxCenter }; { }
    property FdbboxSize: TSFVec3f { read GetFdbboxSize }; { }
  end;

{$endif read_interface}

{$ifdef read_implementation}

class function TNodeSwitch_3.ClassNodeTypeName: string;
begin
  Result := 'Switch';
end;

class function TNodeSwitch_3.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSwitch_3.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;

  FFdwhichChoice := TSFInt32.Create('whichChoice', -1);
  Fields.Add(FFdwhichChoice);

  DefaultContainerField := 'children';
end;

class function TNodeSwitch_3.ForVRMLVersion(const VerMajor, VerMinor: Integer): boolean;
begin
  Result := VerMajor >= 3;
end;

function TNodeSwitch_3.ChildrenField: TMFNode;
begin
  Result := FdChildren;
end;

procedure TNodeSwitch_3.DirectEnumerateActive(Func: TEnumerateChildrenFunction);
begin
  if Between(FdWhichChoice.Value, 0, FdChildren.Count - 1) then
    Func(Self, FdChildren.Items[FdWhichChoice.Value]);
end;

{$endif read_implementation}
