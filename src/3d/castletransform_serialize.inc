{%MainUnit castletransform.pas}
{
  Copyright 2018-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Integration of TCastleTransform with CastleComponentSerialize. }

{$ifdef read_interface}

{ Save / load TCastleTransform (or descendant) to a .castle-transform file.

  An example:
  @includeCode(../../examples/short_api_samples/transform_save_load/transform_save_load.dpr) }
procedure TransformSave(const T: TCastleTransform; const Url: String);
function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;

{$endif read_interface}

{$ifdef read_implementation}

procedure TransformSave(const T: TCastleTransform; const Url: String);
begin
  ComponentSave(T, Url);
end;

function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;
begin
  Result := ComponentLoad(Url, Owner) as TCastleTransform;
end;

{$endif read_implementation}
