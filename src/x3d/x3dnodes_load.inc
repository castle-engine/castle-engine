{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Container tracking VRML/X3D node and prototype names during parsing.
    Used by both classic and XML VRML/X3D readers. }
  TX3DReaderNames = class(TX3DReader)
  private
    FNodes: TX3DNodeNames;
    FPrototypes: TX3DPrototypeNames;
    FImported: TX3DNodeNames;
    FExported: TX3DNodeNames;
    FImportable: TX3DImportableNames;
    procedure CommonCreate(const AAutoRemoveNodes: boolean);
  public
    constructor Create(const AAutoRemoveNodes: boolean;
      const ABaseUrl: string;
      const AVersion: TX3DVersion);
    constructor CreateCopy(const AAutoRemoveNodes: boolean;
      Source: TX3DReader);
    destructor Destroy; override;

    { Extract names, before destructing this object.
      This method can be used only right before calling the destructor.
      It copies the prototype, exported, imported names list (names visible
      from the outside), and sets them to @nil (to avoid releasing them
      at destruction). }
    procedure ExtractNames(out APrototypes: TX3DPrototypeNames;
      out AExported, AImported: TX3DNodeNames);

    { Current namespace for DEF/USE.

      This is a list without duplicates with all
      currently known node names. Objects[] of this list point to
      actual TX3DNode instances. If many instances had the same NodeName,
      only the last instance will be referenced here, following VRML spec
      (last DEF takes precedence).

      Internal notes: ParseNode doesn't modify this, only TX3DNode.Parse
      can do this. }
    property Nodes: TX3DNodeNames read FNodes;

    { Current namespace of PROTO names. }
    property Prototypes: TX3DPrototypeNames read FPrototypes;

    { Currently IMPORTed nodes.

      The nodes on this list are "bound" to their aliases,
      as this is the name under which they are visible in the current namespace.
      Alias is the identifier after the "AS" keyword in the "IMPORT" declaration
      (or, if no "AS xxx" clause was present, then alias is just the name
      under which node was exported). }
    property Imported: TX3DNodeNames read FImported;

    { Currently EXPORTed nodes from this scene.

      The nodes on this list are "bound" to their
      aliases, as this is the name under which they are visible for
      the outside VRML scenes (that can import these nodes).
      Alias is the identifier after the "AS" keyword in "EXPORT" declaration
      (or, if no "AS xxx" clause, then alias is just normal node name). }
    property Exported: TX3DNodeNames read FExported;

    { Currently loaded Inlines with importable nodes.

      The mechanism is that when you load an Inline node, the resulting
      "Exported" nodes (from the namespace within the Inline) get added
      to this "Importable" list. Then the "IMPORT" clause in this
      namespace can make "Importable" nodes into actually "Imported".

      This is a list with strings representing Inline node names
      (there's no way to IMPORT from unnamed Inline nodes).
      Objects[] of this list are instances of TX3DNodeNames
      corresponding to exported names within the inline. }
    property Importable: TX3DImportableNames read FImportable;

    procedure DoExport(E: TX3DExport);
    procedure DoImport(I: TX3DImport);
  end;

{$endif}

{$ifdef read_implementation}

{ TX3DReaderNames ----------------------------------------------------------------- }

constructor TX3DReaderNames.Create(const AAutoRemoveNodes: boolean;
  const ABaseUrl: string; const AVersion: TX3DVersion);
begin
  inherited Create(ABaseUrl, AVersion);
  CommonCreate(AAutoRemoveNodes);
end;

constructor TX3DReaderNames.CreateCopy(const AAutoRemoveNodes: boolean;
  Source: TX3DReader);
begin
  inherited CreateCopy(Source);
  CommonCreate(AAutoRemoveNodes);
end;

procedure TX3DReaderNames.CommonCreate(const AAutoRemoveNodes: boolean);
begin
  FNodes := TX3DNodeNames.Create(AAutoRemoveNodes);
  FPrototypes := TX3DPrototypeNames.Create;
  FImported := TX3DNodeNames.Create(AAutoRemoveNodes);
  FExported := TX3DNodeNames.Create(AAutoRemoveNodes);
  FImportable := TX3DImportableNames.Create;
end;

destructor TX3DReaderNames.Destroy;
begin
  FreeAndNil(FNodes);
  FreeAndNil(FPrototypes);
  FreeAndNil(FImported);
  FreeAndNil(FExported);
  FreeAndNil(FImportable);
  inherited;
end;

procedure TX3DReaderNames.ExtractNames(out APrototypes: TX3DPrototypeNames;
  out AExported, AImported: TX3DNodeNames);
begin
  APrototypes := FPrototypes;
  AExported := FExported;
  AImported := FImported;

  FPrototypes := nil;
  FExported := nil;
  FImported := nil;
end;

procedure TX3DReaderNames.DoExport(E: TX3DExport);
var
  ExportedNode: TX3DNode;
  IgnoreNodeFinished: boolean;
begin
  ExportedNode := Nodes.Bound(E.ExportedNodeName, IgnoreNodeFinished);
  if ExportedNode = nil then
  begin
    WritelnWarning('VRML/X3D', Format('Exported node name "%s" not found', [E.ExportedNodeName]));
    Exit;
  end;

  Exported.Bind(ExportedNode, true, E.ExportedNodeAlias);
end;

procedure TX3DReaderNames.DoImport(I: TX3DImport);
var
  ImportedNames: TX3DNodeNames;
  ImportedNamesIndex: Integer;
  ImportedNode: TX3DNode;
  IgnoreNodeFinished: boolean;
begin
  ImportedNamesIndex := Importable.IndexOf(I.InlineNodeName);
  if ImportedNamesIndex = -1 then
  begin
    WritelnWarning('VRML/X3D', Format('Inline node name "%s" not found (or nothing was EXPORTed from it), cannot IMPORT', [I.InlineNodeName]));
    Exit;
  end;

  ImportedNames := Importable.Objects[ImportedNamesIndex] as TX3DNodeNames;

  ImportedNode := ImportedNames.Bound(I.ImportedNodeName, IgnoreNodeFinished);
  if ImportedNode = nil then
  begin
    WritelnWarning('VRML/X3D', Format('Imported node name "%s" not found in inline "%s"', [I.ImportedNodeName, I.InlineNodeName]));
    Exit;
  end;

  Imported.Bind(ImportedNode, true, I.ImportedNodeAlias);
end;

{$endif}
