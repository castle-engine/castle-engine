{
  Copyright 2008-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  TColladaGeometry = class
    { Collada geometry id. }
    Name: string;
    Primitives: TColladaPrimitiveList;
    constructor Create;
    destructor Destroy; override;
  end;

  TColladaGeometryList = class ({$ifdef FPC}specialize{$endif} TObjectList<TColladaGeometry>)
  public
    { Find item with given Name, @nil if not found. }
    function Find(const Name: string): TColladaGeometry;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TColladaGeometry.Create;
begin
  Primitives := TColladaPrimitiveList.Create;
end;

destructor TColladaGeometry.Destroy;
begin
  FreeAndNil(Primitives);
  inherited;
end;

function TColladaGeometryList.Find(const Name: string): TColladaGeometry;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = Name then
      Exit(Items[I]);

  Result := nil;
end;

{$endif read_implementation}

{$ifdef read_implementation_nested}

var
  { List of Collada geometries. }
  Geometries: TColladaGeometryList;

{ Add Collada geometry instance (many X3D Shape nodes) to given X3D Group. }
procedure AddGeometryInstance(Group: TAbstractGroupingNode;
  Geometry: TColladaGeometry; InstantiatingElement: TDOMElement);

  { For Collada material id, return the X3D Appearance (or @nil if not found). }
  function MaterialToX3D(MaterialId: string;
    InstantiatingElement: TDOMElement): TAppearanceNode;
  var
    BindMaterial, Technique: TDOMElement;
    InstanceMaterialSymbol, InstanceMaterialTarget: string;
    I: TXMLElementIterator;
    Effect: TColladaEffect;
  begin
    if MaterialId = '' then Exit(nil);

    { For InstantiatingElement = instance_geometry (Collada 1.4.x), this
      must be present.
      For InstantiatingElement = instance (Collada 1.3.x), this must not
      be present.
      (But we actually don't check these conditions, just handle any case.) }
    BindMaterial := InstantiatingElement.ChildElement('bind_material', false);
    if BindMaterial <> nil then
    begin
      Technique := BindMaterial.ChildElement('technique_common', false);
      if Technique <> nil then
      begin
        { read <instance_material list inside.
          This may contain multiple materials, but actually we're only
          interested in a single material, so we look for material with
          symbol = MaterialId. }
        I := Technique.ChildrenIterator('instance_material');
        try
          while I.GetNext do
            if I.Current.AttributeString('symbol', InstanceMaterialSymbol) and
               (InstanceMaterialSymbol = MaterialId) and
               I.Current.AttributeString('target', InstanceMaterialTarget) then
            begin
              { this should be true, target is URL }
              if SCharIs(InstanceMaterialTarget, 1, '#') then
                Delete(InstanceMaterialTarget, 1, 1);

              { replace MaterialId with what is indicated by
                  <instance_material target="..."> }
              MaterialId := InstanceMaterialTarget;
            end;
        finally FreeAndNil(I) end;
      end;
    end;

    Materials.TryGetValue(MaterialId, Effect); // Effect will be nil if not found
    if Effect = nil then
    begin
      WritelnWarning('Collada', Format('Referencing non-existing material name "%s"',
        [MaterialId]));
      Result := nil;
    end else
    begin
      Result := Effect.Appearance;
    end;
  end;

var
  Shape: TShapeNode;
  I: Integer;
  Primitive: TColladaPrimitive;
begin
  for I := 0 to Geometry.Primitives.Count - 1 do
  begin
    Primitive := Geometry.Primitives[I];
    Shape := TShapeNode.Create('', BaseUrl);
    Group.AddChildren(Shape);
    Shape.Geometry := Primitive.X3DGeometry;
    Shape.Appearance := MaterialToX3D(Primitive.Material, InstantiatingElement);
  end;
end;

{ Read <instance_geometry>, adding resulting X3D nodes into
  ParentGroup. Actually, this is also for reading <instance> in Collada 1.3.1. }
procedure ReadInstanceGeometry(ParentGroup: TAbstractGroupingNode;
  InstantiatingElement: TDOMElement);
var
  GeometryId: string;
  Geometry: TColladaGeometry;
begin
  if InstantiatingElement.AttributeString('url', GeometryId) and
     SCharIs(GeometryId, 1, '#') then
  begin
    Delete(GeometryId, 1, 1);
    Geometry := Geometries.Find(GeometryId);
    if Geometry = nil then
      WritelnWarning('Collada', Format('<node> instantiates non-existing <geometry> element "%s"',
        [GeometryId])) else
      AddGeometryInstance(ParentGroup, Geometry, InstantiatingElement);
  end else
    WritelnWarning('Collada', Format('Element <%s> missing url attribute (that has to start with #)',
      [InstantiatingElement.TagName]));
end;

{ Read <geometry>. It is added to the Geometries list. }
procedure ReadGeometry(GeometryElement: TDOMElement);
var
  { Collada Geometry constructed, available to all Read* primitives local procedures. }
  Geometry: TColladaGeometry;

  Sources: TColladaSourceList;

  { Read "double sided" geometry property.

    There is no standard for this it seems, only some special MAYA markers:
    <extra><technique profile="MAYA"><double_sided>1</double_sided></technique></extra>
    Blender Collada 1.4 exporter, as well as various models from collada.org/owl
    use this, so it seems like a standard in practice. }
  function ReadDoubleSided(GeometryE: TDOMElement): boolean;
  var
    Child: TDOMElement;
    Profile: string;
  begin
    Result := false;

    Child := GeometryE.ChildElement('extra', false);
    if Child = nil then Exit;

    Child := Child.ChildElement('technique', false);
    if Child = nil then Exit;

    if not (Child.AttributeString('profile', Profile) and
            (Profile = 'MAYA')) then
      Exit;

    Child := Child.ChildElement('double_sided', false);
    if Child = nil then Exit;

    Result := StrToIntDef(Child.TextData, 0) <> 0;
  end;

  { Read <source> within <mesh>. }
  procedure ReadSource(SourceElement: TDOMElement);
  var
    FloatArray, Technique, Accessor: TDOMElement;
    SeekPos, FloatArrayCount, I: Integer;
    FloatArrayContents, Token, AccessorSource, ParamName, ParamType, FloatArrayId: string;
    Source: TColladaSource;
    It: TXMLElementIterator;
  begin
    Source := TColladaSource.Create;
    Sources.Add(Source);

    if not SourceElement.AttributeString('id', Source.Name) then
      Source.Name := '';

    FloatArray := SourceElement.ChildElement('float_array', false);
    if FloatArray <> nil then
    begin
      if not FloatArray.AttributeInteger('count', FloatArrayCount) then
      begin
        FloatArrayCount := 0;
        WritelnWarning('Collada', '<float_array> without a count attribute');
      end;

      if not FloatArray.AttributeString('id', FloatArrayId) then
        FloatArrayId := '';

      Source.Floats.Count := FloatArrayCount;
      FloatArrayContents := FloatArray.TextData;

      SeekPos := 1;
      for I := 0 to FloatArrayCount - 1 do
      begin
        Token := NextToken(FloatArrayContents, SeekPos);
        if Token = '' then
        begin
          WritelnWarning('Collada', 'Actual number of tokens in <float_array>' +
            ' less than declated in the count attribute');
          Break;
        end;
        Source.Floats.L[I] := StrToFloatDot(Token);
      end;

      Technique := SourceElement.ChildElement('technique_common', false);
      if Technique = nil then
      begin
        Technique := SourceElement.ChildElement('technique', false);
        { TODO: actually, I should search for technique with profile="COMMON"
          in this case, right ? }
      end;

      if Technique <> nil then
      begin
        Accessor := Technique.ChildElement('accessor', false);

        { read <accessor> attributes }

        if not Accessor.AttributeInteger('count', Source.Count) then
        begin
          WritelnWarning('Collada', '<accessor> has no count attribute');
          Source.Count := 0;
        end;

        if not Accessor.AttributeInteger('stride', Source.Stride) then
          { default, according to Collada spec }
          Source.Stride := 1;

        if not Accessor.AttributeInteger('offset', Source.Offset) then
          { default, according to Collada spec }
          Source.Offset := 0;

        if not Accessor.AttributeString('source', AccessorSource) then
        begin
          WritelnWarning('Collada', '<accessor> has no source attribute');
          AccessorSource := '';
        end;

        { We read AccessorSource only to check here }
        if AccessorSource <> '#' + FloatArrayId then
          WritelnWarning('Collada', '<accessor> source does not refer to <float_array> in the same <source>, this is not supported');

        It := Accessor.ChildrenIterator('param');
        try
          while It.GetNext do
          begin
            if not It.Current.AttributeString('name', ParamName) then
            begin
              WritelnWarning('Collada', 'Missing "name" of <param>');
              { TODO: this should not be required? }
              Continue;
            end;

            if not It.Current.AttributeString('type', ParamType) then
            begin
              WritelnWarning('Collada', 'Missing "type" of <param>');
              Continue;
            end;

            if ParamType <> 'float' then
            begin
              WritelnWarning('Collada', Format('<param> type "%s" is not supported',
                [ParamType]));
              Continue;
            end;

            Source.Params.Add(ParamName);
          end;
        finally FreeAndNil(It) end;
      end;
    end;
  end;

var
  { We take care to share coordinates, tex coordinates and normal vectors,
    so that they are stored efficiently in memory and can be saved using
    X3D DEF/USE mechanism. Coordinates are always shared by all primitives
    within the geometry. Each primitive may potentially have different tex coords
    or normals arrays, or they may share the same. }

  { Collada coordinates.
    Based on <source> indicated by last <vertices> element. }
  Coord: TCoordinateNode;
  { Assigning to Coord using AssignToVectorXYZ went without trouble. }
  CoordCorrect: boolean;

  { Collada texture coords used by last primitive.
    Based on <source> indicated by <input semantic="TEXCOORD"> element within
    primitive. Name corresponds to <source> id. }
  LastTexCoord: TTextureCoordinateNode;

  { Collada normal used by last primitive.
    Based on <source> indicated by <input semantic="NORMAL"> element within
    primitive. Name corresponds to <source> id. }
  LastNormal: TNormalNode;

  { DoubleSided, should be used by ReadPrimitiveCommon to set X3D solid field. }
  DoubleSided: boolean;

  { Read <vertices> within <mesh> }
  procedure ReadVertices(VerticesElement: TDOMElement);
  var
    I: TXMLElementIterator;
    InputSemantic, InputSourceName, Id: string;
    InputSource: TColladaSource;
  begin
    if Coord <> nil then
    begin
      WritelnWarning('Collada', '<vertices> specified multiple times within a geometry');
      FreeIfUnusedAndNil(Coord);
    end;

    if not VerticesElement.AttributeString('id', Id) then
      Id := '';

    Coord := TCoordinateNode.Create(Id, BaseUrl);
    CoordCorrect := true;

    I := VerticesElement.ChildrenIterator('input');
    try
      while I.GetNext do
        if I.Current.AttributeString('semantic', InputSemantic) and
           (InputSemantic = 'POSITION') and
           I.Current.AttributeString('source', InputSourceName) and
           SCharIs(InputSourceName, 1, '#') then
        begin
          Delete(InputSourceName, 1, 1); { delete leading '#' char }
          InputSource := Sources.Find(InputSourceName);
          if InputSource <> nil then
          begin
            CoordCorrect := InputSource.AssignToVectorXYZ(Coord.FdPoint.Items);
            Exit;
          end else
          begin
            WritelnWarning('Collada', Format('Source attribute ' +
              '(of <input> element within <vertices>) ' +
              'references non-existing source "%s"', [InputSourceName]));
          end;
        end;
    finally FreeAndNil(I) end;

    WritelnWarning('Collada', '<vertices> element has no <input> child' +
      ' with semantic="POSITION" and some source attribute');
  end;

  { Read common things of primitive (<poly*>, <tri*>, <lines*>) within <mesh>.
    - Creates X3D geometry node, initializes it's coordinates
      and other fiels (but leaves *Index fields empty).
    - Adds it to Geometry.Primitives.
    - Returns Indexes instance, to parse indexes of this primitive. }
  function ReadPrimitiveCommon(PrimitiveE: TDOMElement): TColladaIndexes;
  var
    I: TXMLElementIterator;
    InputSemantic, InputSourceId, X3DGeometryName: string;
    Primitive: TColladaPrimitive;
    InputSource: TColladaSource;
    IndexedFaceSet: TIndexedFaceSetNode;
    IndexedLineSet: TIndexedLineSetNode;
    InputsCount, CoordIndexOffset, TexCoordIndexOffset, NormalIndexOffset: Integer;
  begin
    CoordIndexOffset := 0;
    TexCoordIndexOffset := -1;
    NormalIndexOffset := -1;

    Primitive := TColladaPrimitive.Create;
    Geometry.Primitives.Add(Primitive);

    IndexedFaceSet := nil;
    IndexedLineSet := nil;

    X3DGeometryName := Format('%s_collada_primitive_%d',
      { There may be multiple primitives inside a single Collada <geometry> node,
        so use Geometry.Primitives.Count to name them uniquely for X3D. }
      [Geometry.Name, Geometry.Primitives.Count]);

    if (PrimitiveE.TagName = 'lines') or
       (PrimitiveE.TagName = 'linestrips') then
    begin
      IndexedLineSet := TIndexedLineSetNode.Create(X3DGeometryName, BaseUrl);
      IndexedLineSet.FdCoord.Value := Coord;
      Primitive.X3DGeometry := IndexedLineSet;
    end else
    begin
      IndexedFaceSet := TIndexedFaceSetNode.Create(X3DGeometryName, BaseUrl);
      IndexedFaceSet.FdSolid.Value := not DoubleSided;
      { For VRML >= 2.0, creaseAngle is 0 by default.
        TODO: what is the default normal generation for Collada? }
      IndexedFaceSet.FdCreaseAngle.Value := NiceCreaseAngle;
      IndexedFaceSet.FdCoord.Value := Coord;
      Primitive.X3DGeometry := IndexedFaceSet;
    end;

    if PrimitiveE.AttributeString('material', Primitive.Material) then
    begin
      { Collada 1.4.1 spec says that this is just material name.
        Collada 1.3.1 spec says that this is URL. }
      if (not Version14) and SCharIs(Primitive.Material, 1, '#') then
        Delete(Primitive.Material, 1, 1);
    end; { else leave Primitive.Material as '' }

    InputsCount := 0;

    I := PrimitiveE.ChildrenIterator('input');
    try
      while I.GetNext do
      begin
        { we must count all inputs, since parsing <p> elements depends
          on InputsCount }
        Inc(InputsCount);
        if I.Current.AttributeString('semantic', InputSemantic) then
        begin
          if InputSemantic = 'VERTEX' then
          begin
            if not (I.Current.AttributeString('source', InputSourceId) and
                    (Coord <> nil) and
                    (InputSourceId = '#' + Coord.X3DName))  then
              WritelnWarning('Collada', '<input> with semantic="VERTEX" (of primitive element within <mesh>) does not reference <vertices> element within the same <mesh>');

            { Collada requires offset in this case.
              For us, if there's no offset, just leave CoordIndexOffset default. }
            I.Current.AttributeInteger('offset', CoordIndexOffset);
          end else
          if InputSemantic = 'TEXCOORD' then
          begin
            I.Current.AttributeInteger('offset', TexCoordIndexOffset);

            { Only for IndexedFaceSet.
              In case of trouble with coordinates, don't use texCoord,
              they may be invalid (this is for invalid Blender 1.3 exporter) }
            if (IndexedFaceSet = nil) or (not CoordCorrect) then Continue;

            if I.Current.AttributeString('source', InputSourceId) then
            begin
              if SCharIs(InputSourceId, 1, '#') then
                Delete(InputSourceId, 1, 1);
              if ((LastTexCoord <> nil) and
                  (LastTexCoord.X3DName = InputSourceId)) then
                { we can reuse last X3D tex coord node }
                IndexedFaceSet.FdTexCoord.Value := LastTexCoord else
              begin
                InputSource := Sources.Find(InputSourceId);
                if InputSource <> nil then
                begin
                  { create and use new X3D tex coord node }
                  FreeIfUnusedAndNil(LastTexCoord);
                  LastTexCoord := TTextureCoordinateNode.Create(InputSourceId, BaseUrl);
                  InputSource.AssignToVectorST(LastTexCoord.FdPoint.Items);
                  IndexedFaceSet.FdTexCoord.Value := LastTexCoord;
                end else
                  WritelnWarning('Collada', Format('<source> with id "%s" for texture coordinates not found',
                    [InputSourceId]));
              end;
            end else
              WritelnWarning('Collada', 'Missing source for <input> with semantic="TEXCOORD". We have texture coord indexes, but they will be ignored, since we have no actual texture coords');
          end else
          if InputSemantic = 'NORMAL' then
          begin
            I.Current.AttributeInteger('offset', NormalIndexOffset);

            { Only for IndexedFaceSet.
              In case of trouble with coordinates, don't use normals,
              they may be invalid (this is for invalid Blender 1.3 exporter) }
            if (IndexedFaceSet = nil) or (not CoordCorrect) then Continue;

            if I.Current.AttributeString('source', InputSourceId) then
            begin
              if SCharIs(InputSourceId, 1, '#') then
                Delete(InputSourceId, 1, 1);
              if ((LastNormal <> nil) and
                  (LastNormal.X3DName = InputSourceId)) then
                { we can reuse last X3D normal node }
                IndexedFaceSet.FdNormal.Value := LastNormal else
              begin
                InputSource := Sources.Find(InputSourceId);
                if InputSource <> nil then
                begin
                  { create and use new X3D normal node }
                  FreeIfUnusedAndNil(LastNormal);
                  LastNormal := TNormalNode.Create(InputSourceId, BaseUrl);
                  InputSource.AssignToVectorXYZ(LastNormal.FdVector.Items);
                  IndexedFaceSet.FdNormal.Value := LastNormal;
                end else
                  WritelnWarning('Collada', Format('<source> with id "%s" for normals not found',
                    [InputSourceId]));
              end;
            end else
              WritelnWarning('Collada', 'Missing source for <input> with semantic="NORMAL". We have normal indexes, but they will be ignored, since we have no actual normals');
          end;
        end;
      end;
    finally FreeAndNil(I) end;

    Result := TColladaIndexes.Create(Primitive.X3DGeometry,
      InputsCount, CoordIndexOffset, TexCoordIndexOffset, NormalIndexOffset);
  end;

  { Read <polygons> within <mesh> }
  procedure ReadPolygons(PrimitiveE: TDOMElement);
  var
    I: TXMLElementIterator;
    Indexes: TColladaIndexes;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      I := PrimitiveE.ChildrenIterator('p');
      try
        while I.GetNext do
        begin
          Indexes.BeginElement(I.Current);
          while Indexes.ReadAddVertex do ;
          Indexes.PolygonEnd;
        end;
      finally FreeAndNil(I) end;
    finally FreeAndNil(Indexes) end;
  end;

  { Read <polylist> within <mesh> }
  procedure ReadPolylist(PrimitiveE: TDOMElement);
  var
    VCountE, P: TDOMElement;
    I: Integer;
    Indexes: TColladaIndexes;
    VCount: TIntegersParser;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      VCountE := PrimitiveE.ChildElement('vcount', false);
      P := PrimitiveE.ChildElement('p', false);

      if (VCountE <> nil) and (P <> nil) then
      begin
        Indexes.BeginElement(P);
        VCount := TIntegersParser.Create(VCountE);
        try
          { we will parse both VCount and Indexes now, at the same time }
          while VCount.GetNext do
          begin
            for I := 0 to VCount.Current - 1 do
              if not Indexes.ReadAddVertex then
              begin
                WritelnWarning('Collada', 'Unexpected end of <p> data in <polylist>');
                Exit;
              end;
            Indexes.PolygonEnd;
          end;
        finally FreeAndNil(VCount) end;
      end;
    finally FreeAndNil(Indexes) end;
  end;

  { Read <triangles> within <mesh> }
  procedure ReadTriangles(PrimitiveE: TDOMElement);
  var
    P: TDOMElement;
    Indexes: TColladaIndexes;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      P := PrimitiveE.ChildElement('p', false);
      if P <> nil then
      begin
        Indexes.BeginElement(P);
        repeat
          if not Indexes.ReadAddVertex then Break;
          if not Indexes.ReadAddVertex then Break;
          if not Indexes.ReadAddVertex then Break;
          Indexes.PolygonEnd;
        until false;
      end;
    finally FreeAndNil(Indexes) end;
  end;

  { Read <trifans> within <mesh> }
  procedure ReadTriFans(PrimitiveE: TDOMElement);
  var
    I: TXMLElementIterator;
    Indexes: TColladaIndexes;
    Vertex1, Vertex2, VertexPrevious, VertexNext: TIndex;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      I := PrimitiveE.ChildrenIterator('p');
      try
        while I.GetNext do
        begin
          Indexes.BeginElement(I.Current);
          if Indexes.ReadVertex(Vertex1) and
             Indexes.ReadVertex(Vertex2) and
             Indexes.ReadVertex(VertexPrevious) then
          begin
            Indexes.AddVertex(Vertex1);
            Indexes.AddVertex(Vertex2);
            Indexes.AddVertex(VertexPrevious);
            Indexes.PolygonEnd;
            while Indexes.ReadVertex(VertexNext) do
            begin
              Indexes.AddVertex(Vertex1);
              Indexes.AddVertex(VertexPrevious);
              Indexes.AddVertex(VertexNext);
              Indexes.PolygonEnd;
              VertexPrevious := VertexNext;
            end;
          end;
        end;
      finally FreeAndNil(I) end;
    finally FreeAndNil(Indexes) end;
  end;

  { Read <tristrips> within <mesh> }
  procedure ReadTriStrips(PrimitiveE: TDOMElement);
  var
    I: TXMLElementIterator;
    Indexes: TColladaIndexes;
    Vertex1, Vertex2, Vertex3, VertexNext: TIndex;
    Turn: boolean;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      I := PrimitiveE.ChildrenIterator('p');
      try
        while I.GetNext do
        begin
          Turn := true;
          Indexes.BeginElement(I.Current);
          if Indexes.ReadVertex(Vertex1) and
             Indexes.ReadVertex(Vertex2) and
             Indexes.ReadVertex(Vertex3) then
          begin
            Indexes.AddVertex(Vertex1);
            Indexes.AddVertex(Vertex2);
            Indexes.AddVertex(Vertex3);
            Indexes.PolygonEnd;
            while Indexes.ReadVertex(VertexNext) do
            begin
              Indexes.AddVertex(Vertex3);
              Indexes.AddVertex(Vertex2);
              Indexes.AddVertex(VertexNext);
              Indexes.PolygonEnd;
              if Turn then
                Vertex2 := VertexNext else
                Vertex3 := VertexNext;
              Turn := not Turn;
            end;
          end;
        end;
      finally FreeAndNil(I) end;
    finally FreeAndNil(Indexes) end;
  end;

  { Read <lines> within <mesh> }
  procedure ReadLines(PrimitiveE: TDOMElement);
  var
    P: TDOMElement;
    Indexes: TColladaIndexes;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      P := PrimitiveE.ChildElement('p', false);
      if P <> nil then
      begin
        Indexes.BeginElement(P);
        repeat
          if not Indexes.ReadAddVertex then Break;
          if not Indexes.ReadAddVertex then Break;
          Indexes.PolygonEnd;
        until false;
      end;
    finally FreeAndNil(Indexes) end;
  end;

  { TODO: <linestrips> should work fine, but untested }
  { Read <linestrips> within <mesh> }
  procedure ReadLineStrips(PrimitiveE: TDOMElement);
  var
    I: TXMLElementIterator;
    Indexes: TColladaIndexes;
  begin
    Indexes := ReadPrimitiveCommon(PrimitiveE);
    try
      I := PrimitiveE.ChildrenIterator('p');
      try
        while I.GetNext do
        begin
          Indexes.BeginElement(I.Current);
          while Indexes.ReadAddVertex do ;
          Indexes.PolygonEnd;
        end;
      finally FreeAndNil(I) end;
    finally FreeAndNil(Indexes) end;
  end;

var
  Mesh: TDOMElement;
  I: TXMLElementIterator;
  GeometryId: string;
begin
  if not GeometryElement.AttributeString('id', GeometryId) then
    GeometryId := '';

  DoubleSided := ReadDoubleSided(GeometryElement);

  Geometry := TColladaGeometry.Create;
  Geometry.Name := GeometryId;
  Geometries.Add(Geometry);

  Mesh := GeometryElement.ChildElement('mesh', false);
  if Mesh <> nil then
  begin
    Coord := nil;
    LastTexCoord := nil;
    LastNormal := nil;
    Sources := TColladaSourceList.Create;
    try
      I := Mesh.ChildrenIterator;
      try
        while I.GetNext do
          if I.Current.TagName = 'source' then
            ReadSource(I.Current) else
          if I.Current.TagName = 'vertices' then
            ReadVertices(I.Current) else
          if I.Current.TagName = 'polygons' then
            ReadPolygons(I.Current) else
          if I.Current.TagName = 'polylist' then
            ReadPolylist(I.Current) else
          if I.Current.TagName = 'triangles' then
            ReadTriangles(I.Current) else
          if I.Current.TagName = 'trifans' then
            ReadTriFans(I.Current) else
          if I.Current.TagName = 'tristrips' then
            ReadTriStrips(I.Current) else
          if I.Current.TagName = 'lines' then
            ReadLines(I.Current) else
          if I.Current.TagName = 'linestrips' then
            ReadLineStrips(I.Current) else
            WritelnWarning('Collada', Format('Element "%s" within <mesh> not supported',
              [I.Current.TagName]));
      finally FreeAndNil(I) end;
    finally
      FreeAndNil(Sources);
      FreeIfUnusedAndNil(Coord);
      { actually LastTexCoord is for sure used by something,
        we don't really have to finalize it here }
      FreeIfUnusedAndNil(LastTexCoord);
    end;
  end;
end;

{ Read <library_geometries> (Collada >= 1.4.x) or
  <library type="GEOMETRY"> (Collada < 1.4.x). }
procedure ReadLibraryGeometries(LibraryElement: TDOMElement);
var
  I: TXMLElementIterator;
begin
  I := LibraryElement.ChildrenIterator('geometry');
  try
    while I.GetNext do
      ReadGeometry(I.Current);
      { other I.Current.TagName not supported for now }
  finally FreeAndNil(I) end;
end;

{$endif read_implementation_nested}
