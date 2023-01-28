(* Include editor_units defined in CastleEngineManifest.xml.

  We include units this way, through a unit (not by referencing them in lpk),
  because referencing units in lpk would require to determine their filenames
  (case, Pascal extension).
  E.g. we would need to clarify in lpk whether FooBar unit is in
  foobar.pas, FooBar.pas, foobar.pp and so on.
  And we don't have this information defined in CastleEngineManifest.xml
  (and we don't need it, FPC can figure it out on it's own).

  We include units inside a package castle_editor_automatic_package.lpk,
  not directly inside castle_editor.lpi.
  This way castle_editor_automatic_package.lpk can use ${EXTRA_COMPILER_OPTIONS}
  (for example, if you define -Mdelphi in CastleEngineManifest.xml, it will work
  correctly; even though the castle_editor code still uses ObjFpc mode).
*)
unit CastleEditorCustomComponents;

interface

uses ${EDITOR_UNITS};

implementation

uses CastleComponentSerialize;

initialization
  InternalCustomComponentsForProject := '${NAME}';
end.
