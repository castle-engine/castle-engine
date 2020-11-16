{ Getting and setting metadata of X3D nodes.

  Note: for more details, see the MetadataXxx API docs,
  and testcase in tests/code/testx3dnodes.pas . }

{$apptype CONSOLE}

uses SysUtils, Math,
  X3DNodes;
var
  Root: TX3DRootNode;
  Transform: TTransformNode;
  MetadataString: TMetadataStringNode;
  I, StringCount: Integer;
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
  Writeln('Got back Boolean: ', Transform.MetadataBoolean['my_boolean_value', 0]);

  Transform.MetadataString['my_string_value', 0] := 'apple';
  Transform.MetadataString['my_string_value', 2] := 'banana';
  StringCount := (Transform.FindMetadata('my_string_value') as TMetadataStringNode).FdValue.Count;
  Writeln('Got back String array Count ', StringCount);
  for I := 0 to StringCount - 1 do
    Writeln('Got back String array[', I, ']: ', Transform.MetadataString['my_string_value', I]);

  Transform.MetadataInteger['my_integer_value', 2] := 123;
  Writeln('Got back Integer: ', Transform.MetadataInteger['my_integer_value', 2]);

  Transform.MetadataDouble['my_double_value', 0] := 123.456;
  Writeln('Got back Double: ', Transform.MetadataDouble['my_double_value', 0]:1:6);

  { More manual way (not advised) }
  MetadataString := TMetadataStringNode.Create;
  MetadataString.NameField := 'fruit-type';
  MetadataString.SetValue(['banana']);
  (Transform.Metadata as TMetadataSetNode).FdValue.Add(MetadataString);

  Save3D(Root, 'my.x3d');

  FreeAndNil(Root);
end.
