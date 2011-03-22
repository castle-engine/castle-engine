{
  Copyright 2008-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Nodes that are defined by BitManagement (BS Contact) extensions.
  See http://www.blaxxun.es/pdf/BS_Contact_VRML.en.pdf
  for some description (unfortunately, no precise spec). }

{$ifdef read_interface}
  TNodeOrderedGroup = class(TNodeGroup_2)
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeCircle = class(TNodeCircle2D)
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}
class function TNodeOrderedGroup.ClassNodeTypeName: string;
begin
  Result := 'OrderedGroup';
end;

class function TNodeOrderedGroup.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeCircle.ClassNodeTypeName: string;
begin
  Result := 'Circle';
end;

class function TNodeCircle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

procedure RegisterBitManagementNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeOrderedGroup,
    TNodeCircle
  ]);
end;
{$endif read_implementation}
