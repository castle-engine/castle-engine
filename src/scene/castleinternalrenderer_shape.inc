{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Shape class necessary by TRenderer. }

{$ifdef read_interface}
//type
  { Shape that can be rendered. }
  TX3DRendererShape = class(TShape)
  private
    { Generate VBO if needed, and reload VBO contents.
      Assumes GLVertexBufferObject is true.

      Arrays data @italic(must not) be freed (by TGeometryArrays.FreeData)
      before calling this method. Also, this method will always call
      Arrays.FreeData. So do not load the same TGeometryArrays instance
      twice to the Vbo.

      We always keep assertion that Vbo is loaded <=> Arrays data is freed. }
    procedure LoadArraysToVbo;
  public
    { Non-nil means that we have obtained TShaderProgramCache instance,
      with valid Hash and ShaderProgram. Note that ShaderProgram may still
      be @nil, if it failed to link.

      Separate values for each rendering pass, since different rendering
      passes probably have different GlobalLights and so will require different
      shaders. This makes multi-pass rendering, like for shadow volumes,
      play nicely with shaders. Otherwise we could recreate shaders at each
      rendering pass. }
    ProgramCache: array [TTotalRenderingPass] of TShaderProgramCache;

    Cache: TShapeCache;

    { Assign this each time before passing this shape to RenderShape. }
    Fog: TFogFunctionality;

    { For implementing TextureCoordinateGenerator.mode = "MIRROR-PLANE". }
    MirrorPlaneUniforms: TMirrorPlaneUniforms;

    { Is bump mapping allowed by the current shape.
      Fully calculated only after InitMeshRenderer, as determining GeneratorClass
      is needed to set this. }
    BumpMappingAllowed: Boolean;

    { Is bump mapping used for current shape.
      Fully calculated only during render, after BumpMappingAllowed is calculated
      and after textures are applied.
      This is determined by BumpMappingAllowed,
      global BumpMapping, and by the texture information for current
      shape (whether user provided normal map, height map etc.) }
    BumpMappingUsed: Boolean;

    { Along with Shape.BumpMappingUsed, this is calculated (use only if Shape.BumpMappingUsed). }
    BumpMappingTextureCoordinatesId: Cardinal;

    destructor Destroy; override;
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ Sometimes the updates indicated by Shape.Cache.VboToReload can be performed without
  actually recreating the Shape.Cache.Arrays (so no need to create TArraysGenerator
  and call TArraysGenerator.GenerateArrays).
  Detect and perform such optimized case.

  This is crucial for skinned animation, that only requires updating coords/normals/tangents
  each frame, no need for updating other attributes. }
function FastUpdateArrays(const Shape: TX3DRendererShape): Boolean;

  { If possible, update the coordinate/normal/tangent data in VBO fast.

    This is carefully implemented to do a specific case of TShapeCache.LoadArraysToVbo.
    It optimizes the case of animating coordinates/normals/tangents using CoordinateInterpolator,
    which is very important to optimize, since that's how glTF skinned animations
    are done.

    Pass non-nil Tangents if VBO should have tangent data,
    pass nil Tangents if VBO should not have it. }
  function FastCoordinateNormalUpdate(const Cache: TShapeCache;
    const Coords, Normals, Tangents: TVector3List): Boolean;
  var
    NewCoordinates, NewCoord: Pointer;
    Count, Size, ItemSize: Cardinal;
    I: Integer;
  begin
    Result := false;

    if Cache.VboCoordinatePreserveGeometryOrder and
       (Normals.Count = Coords.Count) and
       ( (Tangents = nil) or
         (Tangents.Count = Coords.Count) ) then
    begin
      Count := Coords.Count;

      ItemSize := SizeOf(TVector3) * 2;
      if Tangents <> nil then
        ItemSize := ItemSize + SizeOf(TVector3);
      Size := Count * ItemSize;

      if (Cache.VboAllocatedUsage = GL_STREAM_DRAW) and
         { Comparing the byte sizes also makes sure that previous and new coordinates
           count stayed the same.
           (Well, assuming we didn't change the Count, and simultaneously changed
           the Tangent existence, which in theory could cause Size to match
           for different structures.)
         }
         (Cache.VboAllocatedSize[vtCoordinate] = Size) then
      begin
        Result := true;
        Cache.VboToReload := Cache.VboToReload - [vtCoordinate]; // vtCoordinate are done
        if Count <> 0 then
        begin
          // calculate NewCoordinates
          NewCoordinates := GetMem(Size);
          try
            NewCoord := NewCoordinates;

            if Tangents <> nil then
            begin
              for I := 0 to Count - 1 do
              begin
                PVector3(NewCoord)^ := Coords.L[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Normals.L[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Tangents.L[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);
              end;
            end else
            begin
              for I := 0 to Count - 1 do
              begin
                PVector3(NewCoord)^ := Coords.L[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);

                PVector3(NewCoord)^ := Normals.L[I];
                PtrUInt(NewCoord) := PtrUInt(NewCoord) + SizeOf(TVector3);
              end;
            end;

            // load NewCoordinates to GPU
            glBindBuffer(GL_ARRAY_BUFFER, Cache.Vbo[vtCoordinate]);
            glBufferSubData(GL_ARRAY_BUFFER, 0, Size, NewCoordinates);
          finally FreeMemNiling(NewCoordinates) end;
        end;
      end;
    end;
  end;

var
  Cache: TShapeCache;
  Coords, Normals, Tangents: TVector3List;
begin
  Result := false;

  Cache := Shape.Cache;

  // this only optimizes the specific case of VboToReload = [vtCoordinate] now
  if Cache.VboToReload <> [vtCoordinate] then
    Exit;

  { When VBO of coordinates is 0 then
    - VBOs not supported (ancient GPU) -> we need to really update Arrays contents each time
    - or VBO of coordinates was not initialized yet }
  if Cache.Vbo[vtCoordinate] = 0 then
    Exit;
  if Cache.Arrays = nil then // not initialized yet
    Exit;

  if // Shape has coordinates expressed in simple way
     (Shape.Geometry.CoordField <> nil) and
     (Shape.Geometry.CoordField.Value is TCoordinateNode) then // checks also Value <> nil
  begin
    Coords := TCoordinateNode(Shape.Geometry.CoordField.Value).FdPoint.Items;

    if // Shape has normals expressed in simple way
       (Shape.Geometry.NormalField <> nil) and
       (Shape.Geometry.NormalField.Value is TNormalNode) then // checks also Value <> nil
    begin
      Normals := TNormalNode(Shape.Geometry.NormalField.Value).FdVector.Items;

      // Shape has normals expressed in tangents way, or doesn't use tangents (when no bump mapping used)
      if ( (not Cache.Arrays.HasTangent) or
           ( (Shape.Geometry.TangentField <> nil) and
             (Shape.Geometry.TangentField.Value is TTangentNode) ) ) then // checks also Value <> nil
      begin
        if Cache.Arrays.HasTangent then
          Tangents := TTangentNode(Shape.Geometry.TangentField.Value).FdVector.Items
        else
          Tangents := nil;

        Result := FastCoordinateNormalUpdate(Cache, Coords, Normals, Tangents);
      end;
    end;
  end;
end;

{ TX3DRendererShape --------------------------------------------------------- }

destructor TX3DRendererShape.Destroy;
begin
  FreeAndNil(MirrorPlaneUniforms);
  inherited;
end;

procedure TX3DRendererShape.LoadArraysToVbo;
begin
  Assert(Cache <> nil);
  Cache.LoadArraysToVbo(DynamicGeometry);
end;

{$endif}
