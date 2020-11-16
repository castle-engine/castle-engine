{ Getting and setting metadata of X3D nodes. }

uses SysUtils, Math,
  X3DNodes;
var
  Root: TX3DRootNode;
  Transform: TTransformNode;
  MetadataString: TMetadataStringNode;
begin
  Root := TX3DRootNode.Create;

  Transform := TTransformNode.Create;
  Root.AddChildren(Transform);

  { Advised way: use properties
    MetadataBoolean / MetadataString  / MetadataInteger / MetadataDouble.
    Think of each metadata, as a mapping from a unique key (string) ->
    to an array of booleans / strings / integers / doubles.

    Underneath, more complicated scenarios are possible, but if you only
    use these properties to get/set metadata, then it remains simple. }

  Transform.MetadataBoolean['my_boolean_value', 0] := true;
  Assert(Transform.MetadataBoolean['my_boolean_value', 0]);

  Transform.MetadataString['my_string_value', 0] := 'apple';
  Transform.MetadataString['my_string_value', 2] := 'banana';
  Assert((Transform.FindMetadata('my_string_value') as TMetadataStringNode).FdValue.Count = 3);
  Assert(Transform.MetadataString['my_string_value', 0] = 'apple');
  Assert(Transform.MetadataString['my_string_value', 1] = '');
  Assert(Transform.MetadataString['my_string_value', 2] = 'banana');

  Transform.MetadataInteger['my_integer_value', 2] := 123;
  Assert(Transform.MetadataInteger['my_integer_value', 0] = 0);
  Assert(Transform.MetadataInteger['my_integer_value', 1] = 0);
  Assert(Transform.MetadataInteger['my_integer_value', 2] = 123);

  Transform.MetadataDouble['my_double_value', 0] := 123.456;
  Assert(SameValue(Transform.MetadataDouble['my_double_value', 0], 123.456));

  { More manual way (not advised) }
  MetadataString := TMetadataStringNode.Create;
  MetadataString.NameField := 'fruit-type';
  MetadataString.SetValue(['banana']);
  (Transform.Metadata as TMetadataSetNode).FdValue.Add(MetadataString);

  Save3D(Root, 'my.x3d');

  FreeAndNil(Root);
end.
