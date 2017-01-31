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

{$ifdef read_interface_nested}

function ReadChildText(Element: TDOMElement; const ChildTagName: string): string;
  forward;

{$endif read_interface_nested}

{$ifdef read_implementation_nested}

{ Read the contents of the text data inside single child ChildTagName.
  Useful to handle things like <init_from> and <source> elements in Collada.
  Returns empty string if ChildTagName not present (or present present more
  than once) in Element. }
function ReadChildText(Element: TDOMElement; const ChildTagName: string): string;
var
  Child: TDOMElement;
begin
  Child := Element.ChildElement(ChildTagName, false);
  if Child <> nil then
    Result := Child.TextData else
    Result := '';
end;

{ Read the contents of the text data inside single child ChildTagName,
  interpret them as Float.

  Returns false when such child not found (or occurs more than once),
  or when text cannot be converted to float.
  In this case, Value is guaranteed not to be modified. }
function ReadChildFloat(Element: TDOMElement; const ChildTagName: string;
  var Value: Float): boolean;
var
  Child: TDOMElement;
begin
  Child := Element.ChildElement(ChildTagName, false);
  Result := Child <> nil;
  if Result then
  try
    Value := StrToFloat(Child.TextData);
  except on EConvertError do Result := false; end;
end;

function ReadChildFloat(Element: TDOMElement; const ChildTagName: string;
  var Value: Single): boolean;
var
  ValueFloat: Float;
begin
  ValueFloat := 0; { avoid uninitialized warnings }
  Result := ReadChildFloat(Element, ChildTagName, ValueFloat);
  if Result then
    Value := ValueFloat;
end;

{ Read the contents of the text data inside single child ChildTagName,
  interpret them as TVector3Single.

  Returns false when such child not found (or occurs more than once),
  or when cannot be converted to float.
  In this case, Value is guaranteed not to be modified. }
function ReadChildVector(Element: TDOMElement; const ChildTagName: string;
  var Value: TVector3Single): boolean;
var
  Child: TDOMElement;
begin
  Child := Element.ChildElement(ChildTagName, false);
  Result := Child <> nil;
  if Result then
  try
    Value := Vector3SingleFromStr(Child.TextData);
  except on EConvertError do Result := false; end;
end;

{ Read <library> element.
  Only for Collada < 1.4.x (Collada >= 1.4.x has  <library_xxx> elements). }
procedure ReadLibrary(LibraryElement: TDOMElement);
var
  LibraryType: string;
begin
  if LibraryElement.AttributeString('type', LibraryType) then
  begin
    if LibraryType = 'MATERIAL' then
      ReadLibraryMaterials(LibraryElement) else
    if LibraryType = 'GEOMETRY' then
      ReadLibraryGeometries(LibraryElement);
      { other LibraryType not supported for now }
  end;
end;

{ Read <instance_*>, adding resulting X3D nodes into ParentGroup. }
procedure ReadInstance(ParentGroup: TAbstractX3DGroupingNode;
  InstantiatingElement: TDOMElement; List: TX3DNodeList);
var
  Id: string;
  Node: TX3DNode;
begin
  if InstantiatingElement.AttributeString('url', Id) and
     SCharIs(Id, 1, '#') then
  begin
    Delete(Id, 1, 1);
    Node := List.FindName(Id);
    if Node = nil then
      WritelnWarning('Collada', Format('<node> instantiates non-existing element "%s"',
        [Id])) else
      ParentGroup.FdChildren.Add(Node);
  end else
    WritelnWarning('Collada', Format('Element <%s> missing url attribute (that has to start with #)',
      [InstantiatingElement.TagName]));
end;

{$endif read_implementation_nested}
