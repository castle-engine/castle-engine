{ Getting and setting metadata of X3D nodes.

  Note: for more details, see the MetadataXxx API docs,
  and testcase in tests/code/testx3dnodes.pas . }

{$apptype CONSOLE}

uses SysUtils, Math,
  X3DNodes, X3DLoad;
var
  Root: TX3DRootNode;
  Transform: TTransformNode;
  MetadataString: TMetadataStringNode;
  I, StringCount: Integer;
begin
  Root := TX3DRootNode.Create;

  Transform := TTransformNode.Create;
  Root.AddChildren(Transform);

  { Use properties
    MetadataBooleanArray / MetadataStringArray  / MetadataIntegerArray / MetadataDoubleArray.
    Think of each metadata, as a mapping from a unique key (string) ->
    to an array of booleans / strings / integers / doubles.

    You can use even simpler
    MetadataBoolean / MetadataString  / MetadataInteger / MetadataDouble
    that get/set first item of the respective arrays,
    which allows you to think that each metadata, is a mapping from a unique key (string) ->
    one boolean / string / integer / double.

    Underneath, more complicated scenarios are possible, but if you only
    use these properties to get/set metadata, then it remains simple. }

  Transform.MetadataBoolean['my_boolean_value'] := true;
  Writeln('Got back Boolean: ', Transform.MetadataBoolean['my_boolean_value']);

  Transform.MetadataStringArray['my_string_value', 0] := 'apple';
  Transform.MetadataStringArray['my_string_value', 2] := 'banana';
  StringCount := (Transform.FindMetadata('my_string_value') as TMetadataStringNode).FdValue.Count;
  Writeln('Got back String array Count ', StringCount);
  for I := 0 to StringCount - 1 do
    Writeln('Got back String array[', I, ']: ', Transform.MetadataStringArray['my_string_value', I]);

  Transform.MetadataIntegerArray['my_integer_value', 2] := 123;
  Writeln('Got back Integer: ', Transform.MetadataIntegerArray['my_integer_value', 2]);

  Transform.MetadataDouble['my_double_value'] := 123.456;
  Writeln('Got back Double: ', Transform.MetadataDouble['my_double_value']:1:6);

  { More manual way (not advised) }
  MetadataString := TMetadataStringNode.Create;
  MetadataString.NameField := 'fruit-type';
  MetadataString.SetValue(['banana']);
  (Transform.Metadata as TMetadataSetNode).FdValue.Add(MetadataString);

  SaveNode(Root, 'my.x3d');

  FreeAndNil(Root);
end.
