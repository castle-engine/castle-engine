{%MainUnit castleinternalrenderer.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
//type
  { Rendering resource for TScreenEffectNode. }
  TScreenEffectResource = class(TRendererResource)
  strict private
    { Did we try to initialize ShaderProgram, i.e. was PrepareCore called.

      We guarantee "ShaderLoaded=false => ShaderProgram=nil".

      So when ShaderLoaded=true, it may be be that ShaderProgram<>nil
      (if shader compiled OK) or ShaderProgram=nil (if shader compilation
      failed). }
    ShaderLoaded: Boolean;
    FNode: TScreenEffectNode;
    FRenderOptions: TCastleRenderOptions;
    FRHIVertexShader: TRHIShaderModule;
    FRHIFragmentShader: TRHIShaderModule;
    FRHIDescriptorLayout: TRHIDescriptorSetLayout;
    FRHIPipeline: TRHIPipeline;
    FRHIVertexGLSL: String;
    FRHIFragmentGLSL: String;
    FRHIUserEffectBuffer: TRHIBuffer;
    FRHIUserEffectDescriptorLayout: TRHIDescriptorSetLayout;
    FRHIUserEffectDescriptorSet: TRHIDescriptorSet;
    FRHIUserEffectLayout: TUserEffectShaderLayout;
    FRHIUserEffectFallbackUBO: TRHIBuffer;
    FRHIUserEffectTextures: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHITexture;
    FRHIUserEffectSamplers: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS - 1] of TRHISampler;
    FRHIUserEffectUniformCount: Cardinal;
    FRHIUserEffectUploadCount: Cardinal;
    function GetRHIUserEffectTexture(const AIndex: Cardinal): TRHITexture;
    function GetRHIUserEffectSampler(const AIndex: Cardinal): TRHISampler;
  protected
    procedure PrepareCore(const RenderOptions: TCastleRenderOptions); override;
    procedure UnprepareCore; override;
  public
    { GLSL shader for the ScreenEffect node.
      @nil if not prepared, or failed to be prepared (e.g. shader didn't compile). }
    ShaderProgram: TX3DGLSLProgram;

    constructor Create(const ANode: TX3DNode); override;

    { Extracts custom X3D uniform fields and texture nodes from the ScreenEffect node
      and its active TComposedShaderNode InterfaceDeclarations, uploads std140 uniform
      values to @link(RHIUserEffectBuffer) (Set 2, Binding 1), and binds custom texture
      samplers to Set 2, Bindings 2..5. Returns the number of custom uniforms/textures extracted. }
    function UpdateRHIUniforms: Cardinal;

    { Associated node.
      This obscures parent Node property,
      which is OK -- in this class Node is always guaranteed to be
      TScreenEffectNode. }
    property Node: TScreenEffectNode read FNode;
    property RHIPipeline: TRHIPipeline read FRHIPipeline;
    property RHIVertexShader: TRHIShaderModule read FRHIVertexShader;
    property RHIFragmentShader: TRHIShaderModule read FRHIFragmentShader;
    property RHIDescriptorLayout: TRHIDescriptorSetLayout read FRHIDescriptorLayout;
    property RHIVertexGLSL: String read FRHIVertexGLSL;
    property RHIFragmentGLSL: String read FRHIFragmentGLSL;
    property RHIUserEffectBuffer: TRHIBuffer read FRHIUserEffectBuffer;
    property RHIUserEffectDescriptorLayout: TRHIDescriptorSetLayout read FRHIUserEffectDescriptorLayout;
    property RHIUserEffectDescriptorSet: TRHIDescriptorSet read FRHIUserEffectDescriptorSet;
    property RHIUserEffectTextures[const AIndex: Cardinal]: TRHITexture read GetRHIUserEffectTexture;
    property RHIUserEffectSamplers[const AIndex: Cardinal]: TRHISampler read GetRHIUserEffectSampler;
    property RHIUserEffectUniformCount: Cardinal read FRHIUserEffectUniformCount;
    property RHIUserEffectUploadCount: Cardinal read FRHIUserEffectUploadCount;
  end;

  { Manage TScreenEffectResource connected to X3D nodes.

    Non-instantiable class (has only class methods).

    This should be the only code accessing
    TScreenEffectNode.InternalRendererResource field,
    everything else should go through this class. }
  TScreenEffectResources = class abstract
  public
    { Prepare renderer resources for given node. }
    class procedure Prepare(
      const RenderOptions: TCastleRenderOptions;
      const Node: TScreenEffectNode);

    { Renderer resource associated with given TScreenEffectNode.
      Returns @nil if no resource is associated yet (call Prepare to initialize it). }
    class function Get(const Node: TScreenEffectNode): TScreenEffectResource;
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TScreenEffectResources ---------------------------------------------------- }

class procedure TScreenEffectResources.Prepare(const RenderOptions: TCastleRenderOptions;
  const Node: TScreenEffectNode);
var
  RendererResource: TScreenEffectResource;
begin
  { Unlike TTextureResources.Prepare, we don't allow nil argument.
    This is more consistent in CGE, see
    https://castle-engine.io/coding_conventions#nil }
  Assert(Node <> nil);
  if Node.InternalRendererResource = nil then
  begin
    RendererResource := TScreenEffectResource.Create(Node);
    RendererResource.Prepare(RenderOptions);
    Node.InternalRendererResource := RendererResource;
  end else
  if Node.FdEnabled.Value then
  begin
    RendererResource := TScreenEffectResource(Node.InternalRendererResource);
    if (RendererResource.RHIPipeline = nil) and (RendererResource.ShaderProgram = nil) then
      RendererResource.Prepare(RenderOptions);
  end;
end;

class function TScreenEffectResources.Get(const Node: TScreenEffectNode):
  TScreenEffectResource;
begin
  Result := Node.InternalRendererResource as TScreenEffectResource;
end;

{ TScreenEffectResource ----------------------------------------------------- }

function TScreenEffectResource.GetRHIUserEffectTexture(const AIndex: Cardinal): TRHITexture;
begin
  if AIndex <= High(FRHIUserEffectTextures) then
    Result := FRHIUserEffectTextures[AIndex]
  else
    Result := nil;
end;

function TScreenEffectResource.GetRHIUserEffectSampler(const AIndex: Cardinal): TRHISampler;
begin
  if AIndex <= High(FRHIUserEffectSamplers) then
    Result := FRHIUserEffectSamplers[AIndex]
  else
    Result := nil;
end;

function TScreenEffectResource.UpdateRHIUniforms: Cardinal;
var
  UBOData: array[0..4095] of Byte;
  UBOOffset, UnreferencedOffset, MaxUploadEnd: PtrUInt;
  TexSlotCount: Cardinal;
  UseNamedLayout, CurrentFieldIsNamed: Boolean;

  procedure AlignOffset(const AAlignment: PtrUInt);
  begin
    if not CurrentFieldIsNamed then
      UBOOffset := (UBOOffset + AAlignment - 1) and not (AAlignment - 1);
  end;

  procedure WriteBytes(const ASource; const ASize, AAlignment: PtrUInt);
  begin
    AlignOffset(AAlignment);
    if UBOOffset + ASize <= SizeOf(UBOData) then
    begin
      Move(ASource, UBOData[UBOOffset], ASize);
      Inc(UBOOffset, ASize);
      if UBOOffset > MaxUploadEnd then
        MaxUploadEnd := UBOOffset;
      Inc(Result);
    end;
  end;

  procedure ResolveAndBindTextureNodeAtSlot(const ANode: TX3DNode; const AExplicitSlot: Integer);
  var
    Res: TTextureResource;
    Tex: TRHITexture;
    Samp: TRHISampler;
    TargetSlot: Cardinal;
  begin
    if (ANode = nil) or (not (ANode is TAbstractTextureNode)) then
      Exit;
    if AExplicitSlot >= 0 then
    begin
      if AExplicitSlot >= CASTLE_MAX_USER_EFFECT_SAMPLERS then
        Exit;
      TargetSlot := Cardinal(AExplicitSlot);
    end else
    begin
      if TexSlotCount >= CASTLE_MAX_USER_EFFECT_SAMPLERS then
        Exit;
      TargetSlot := TexSlotCount;
    end;
    Res := TTextureResources.Get(TAbstractTextureNode(ANode));
    if (Res = nil) and (RHIDevice <> nil) then
    begin
      TTextureResources.Prepare(FRenderOptions, TAbstractTextureNode(ANode));
      Res := TTextureResources.Get(TAbstractTextureNode(ANode));
    end;
    if Res = nil then
      Exit;
    Tex := nil;
    if Res is TImageTextureResource then
      Tex := TImageTextureResource(Res).RHITexture
    else if Res is TMovieTextureResource then
      Tex := TMovieTextureResource(Res).RHITexture
    else if Res is TRenderedTextureResource then
      Tex := TRenderedTextureResource(Res).RHITexture
    else if Res is TCubeMapTextureResource then
      Tex := TCubeMapTextureResource(Res).RHITexture
    else if Res is T3DTextureResource then
      Tex := T3DTextureResource(Res).RHITexture
    else if Res is TGeneratedShadowMapResource then
      Tex := TGeneratedShadowMapResource(Res).RHITexture;
    Samp := Res.RHISampler;
    if Tex <> nil then
    begin
      FRHIUserEffectTextures[TargetSlot] := Tex;
      FRHIUserEffectSamplers[TargetSlot] := Samp;
      if AExplicitSlot < 0 then
        Inc(TexSlotCount);
      Inc(Result);
    end;
  end;

  procedure ProcessInterfaceDeclarations(const AIDecls: TX3DInterfaceDeclarationList);
  var
    I, J, K: Integer;
    Field: TX3DField;
    FieldName: String;
    UDesc: TUserEffectUniformDesc;
    SDesc: TUserEffectSamplerDesc;
    FVal: Single;
    IVal: Int32;
    IVec4: array[0..3] of Int32;
    V2: TVector2;
    V3: TVector3;
    V4: TVector4;
    M3: TMatrix3;
    M4: TMatrix4;
  begin
    if AIDecls = nil then
      Exit;
    for I := 0 to AIDecls.Count - 1 do
    begin
      Field := AIDecls[I].FieldOrLastEventValue;
      if Field = nil then
        Continue;
      FieldName := Field.X3DName;
      if FieldName = '' then
        FieldName := AIDecls[I].FieldOrEvent.X3DName;
      if Field is TSFNode then
      begin
        if UseNamedLayout and FRHIUserEffectLayout.FindSampler(FieldName, SDesc) then
          ResolveAndBindTextureNodeAtSlot(TSFNode(Field).Value, SDesc.SlotIndex)
        else
          ResolveAndBindTextureNodeAtSlot(TSFNode(Field).Value, -1);
        Continue;
      end else if Field is TMFNode then
      begin
        for J := 0 to TMFNode(Field).Count - 1 do
          if UseNamedLayout and FRHIUserEffectLayout.FindSampler(FieldName, SDesc) then
            ResolveAndBindTextureNodeAtSlot(TMFNode(Field)[J], SDesc.SlotIndex + J)
          else
            ResolveAndBindTextureNodeAtSlot(TMFNode(Field)[J], -1);
        Continue;
      end;

      CurrentFieldIsNamed := UseNamedLayout and
        FRHIUserEffectLayout.FindUniform(FieldName, UDesc);
      if CurrentFieldIsNamed then
        UBOOffset := UDesc.ByteOffset
      else
        UBOOffset := UnreferencedOffset;

      if Field is TSFBool then
      begin
        IVal := Ord(TSFBool(Field).Value);
        WriteBytes(IVal, SizeOf(Int32), 4);
      end else if Field is TSFLong then
      begin
        IVal := TSFLong(Field).Value;
        WriteBytes(IVal, SizeOf(Int32), 4);
      end else if Field is TSFFloat then
      begin
        FVal := TSFFloat(Field).Value;
        WriteBytes(FVal, SizeOf(Single), 4);
      end else if Field is TSFDouble then
      begin
        FVal := TSFDouble(Field).Value;
        WriteBytes(FVal, SizeOf(Single), 4);
      end else if Field is TSFVec2f then
      begin
        V2 := TSFVec2f(Field).Value;
        WriteBytes(V2, SizeOf(TVector2), 8);
      end else if Field is TSFVec2d then
      begin
        V2 := Vector2(Single(TSFVec2d(Field).Value.X), Single(TSFVec2d(Field).Value.Y));
        WriteBytes(V2, SizeOf(TVector2), 8);
      end else if Field is TSFVec3f then
      begin
        V3 := TSFVec3f(Field).Value;
        WriteBytes(V3, SizeOf(TVector3), 16);
      end else if Field is TSFVec3d then
      begin
        V3 := Vector3(
          Single(TSFVec3d(Field).Value.X),
          Single(TSFVec3d(Field).Value.Y),
          Single(TSFVec3d(Field).Value.Z));
        WriteBytes(V3, SizeOf(TVector3), 16);
      end else if Field is TSFVec4f then
      begin
        V4 := TSFVec4f(Field).Value;
        WriteBytes(V4, SizeOf(TVector4), 16);
      end else if Field is TSFVec4d then
      begin
        V4 := Vector4(
          Single(TSFVec4d(Field).Value.X),
          Single(TSFVec4d(Field).Value.Y),
          Single(TSFVec4d(Field).Value.Z),
          Single(TSFVec4d(Field).Value.W));
        WriteBytes(V4, SizeOf(TVector4), 16);
      end else if Field is TSFRotation then
      begin
        V4 := TSFRotation(Field).Value;
        WriteBytes(V4, SizeOf(TVector4), 16);
      end else if Field is TSFMatrix3f then
      begin
        M3 := TSFMatrix3f(Field).Value;
        for J := 0 to 2 do
        begin
          V4 := Vector4(M3.Data[J][0], M3.Data[J][1], M3.Data[J][2], 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TSFMatrix3d then
      begin
        for J := 0 to 2 do
        begin
          V4 := Vector4(
            Single(TSFMatrix3d(Field).Value.Data[J][0]),
            Single(TSFMatrix3d(Field).Value.Data[J][1]),
            Single(TSFMatrix3d(Field).Value.Data[J][2]),
            0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TSFMatrix4f then
      begin
        M4 := TSFMatrix4f(Field).Value;
        WriteBytes(M4, SizeOf(TMatrix4), 16);
      end else if Field is TSFMatrix4d then
      begin
        for J := 0 to 3 do
        begin
          V4 := Vector4(
            Single(TSFMatrix4d(Field).Value.Data[J][0]),
            Single(TSFMatrix4d(Field).Value.Data[J][1]),
            Single(TSFMatrix4d(Field).Value.Data[J][2]),
            Single(TSFMatrix4d(Field).Value.Data[J][3]));
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFBool then
      begin
        for J := 0 to TMFBool(Field).Count - 1 do
        begin
          FillChar(IVec4, SizeOf(IVec4), 0);
          IVec4[0] := Ord(TMFBool(Field).Items[J]);
          WriteBytes(IVec4, SizeOf(IVec4), 16);
        end;
      end else if Field is TMFLong then
      begin
        for J := 0 to TMFLong(Field).Count - 1 do
        begin
          FillChar(IVec4, SizeOf(IVec4), 0);
          IVec4[0] := TMFLong(Field).Items[J];
          WriteBytes(IVec4, SizeOf(IVec4), 16);
        end;
      end else if Field is TMFFloat then
      begin
        for J := 0 to TMFFloat(Field).Count - 1 do
        begin
          V4 := Vector4(TMFFloat(Field).Items[J], 0.0, 0.0, 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFDouble then
      begin
        for J := 0 to TMFDouble(Field).Count - 1 do
        begin
          V4 := Vector4(Single(TMFDouble(Field).Items[J]), 0.0, 0.0, 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec2f then
      begin
        for J := 0 to TMFVec2f(Field).Count - 1 do
        begin
          V4 := Vector4(TMFVec2f(Field).Items[J].X, TMFVec2f(Field).Items[J].Y, 0.0, 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec2d then
      begin
        for J := 0 to TMFVec2d(Field).Count - 1 do
        begin
          V4 := Vector4(Single(TMFVec2d(Field).Items[J].X), Single(TMFVec2d(Field).Items[J].Y), 0.0, 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec3f then
      begin
        for J := 0 to TMFVec3f(Field).Count - 1 do
        begin
          V4 := Vector4(TMFVec3f(Field).Items[J], 0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec3d then
      begin
        for J := 0 to TMFVec3d(Field).Count - 1 do
        begin
          V4 := Vector4(
            Single(TMFVec3d(Field).Items[J].X),
            Single(TMFVec3d(Field).Items[J].Y),
            Single(TMFVec3d(Field).Items[J].Z),
            0.0);
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec4f then
      begin
        for J := 0 to TMFVec4f(Field).Count - 1 do
        begin
          V4 := TMFVec4f(Field).Items[J];
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFVec4d then
      begin
        for J := 0 to TMFVec4d(Field).Count - 1 do
        begin
          V4 := Vector4(
            Single(TMFVec4d(Field).Items[J].X),
            Single(TMFVec4d(Field).Items[J].Y),
            Single(TMFVec4d(Field).Items[J].Z),
            Single(TMFVec4d(Field).Items[J].W));
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFRotation then
      begin
        for J := 0 to TMFRotation(Field).Count - 1 do
        begin
          V4 := TMFRotation(Field).Items[J];
          WriteBytes(V4, SizeOf(TVector4), 16);
        end;
      end else if Field is TMFMatrix3f then
      begin
        for J := 0 to TMFMatrix3f(Field).Count - 1 do
        begin
          M3 := TMFMatrix3f(Field).Items[J];
          for K := 0 to 2 do
          begin
            V4 := Vector4(M3.Data[K][0], M3.Data[K][1], M3.Data[K][2], 0.0);
            WriteBytes(V4, SizeOf(TVector4), 16);
          end;
        end;
      end else if Field is TMFMatrix3d then
      begin
        for J := 0 to TMFMatrix3d(Field).Count - 1 do
        begin
          for K := 0 to 2 do
          begin
            V4 := Vector4(
              Single(TMFMatrix3d(Field).Items[J].Data[K][0]),
              Single(TMFMatrix3d(Field).Items[J].Data[K][1]),
              Single(TMFMatrix3d(Field).Items[J].Data[K][2]),
              0.0);
            WriteBytes(V4, SizeOf(TVector4), 16);
          end;
        end;
      end else if Field is TMFMatrix4f then
      begin
        for J := 0 to TMFMatrix4f(Field).Count - 1 do
        begin
          M4 := TMFMatrix4f(Field).Items[J];
          WriteBytes(M4, SizeOf(TMatrix4), 16);
        end;
      end else if Field is TMFMatrix4d then
      begin
        for J := 0 to TMFMatrix4d(Field).Count - 1 do
        begin
          for K := 0 to 3 do
          begin
            V4 := Vector4(
              Single(TMFMatrix4d(Field).Items[J].Data[K][0]),
              Single(TMFMatrix4d(Field).Items[J].Data[K][1]),
              Single(TMFMatrix4d(Field).Items[J].Data[K][2]),
              Single(TMFMatrix4d(Field).Items[J].Data[K][3]));
            WriteBytes(V4, SizeOf(TVector4), 16);
          end;
        end;
      end;
      if not CurrentFieldIsNamed then
        UnreferencedOffset := UBOOffset;
    end;
  end;

var
  I: Integer;
  AlignedSize: PtrUInt;
  BufDesc: TRHIBufferDesc;
  Mapped: Pointer;
  Set2Bindings: array[0..CASTLE_MAX_USER_EFFECT_SAMPLERS] of TRHIDescriptorBindingDesc;
  ZeroBytes: array[0..15] of Byte;
begin
  Result := 0;
  if (RHIDevice = nil) or (Node = nil) then
    Exit;

  UBOOffset := 0;
  UnreferencedOffset := 0;
  MaxUploadEnd := 0;
  TexSlotCount := 0;
  CurrentFieldIsNamed := False;
  FillChar(UBOData, SizeOf(UBOData), 0);
  UseNamedLayout := (FRHIUserEffectLayout.TotalUBOSize > 0) or
    (Length(FRHIUserEffectLayout.Samplers) > 0);
  if UseNamedLayout then
  begin
    TexSlotCount := Length(FRHIUserEffectLayout.Samplers);
    if Length(FRHIUserEffectLayout.Uniforms) > 0 then
      UnreferencedOffset :=
        FRHIUserEffectLayout.Uniforms[High(FRHIUserEffectLayout.Uniforms)].ByteOffset +
        FRHIUserEffectLayout.Uniforms[High(FRHIUserEffectLayout.Uniforms)].ByteSize;
  end;
  for I := 0 to High(FRHIUserEffectTextures) do
  begin
    FRHIUserEffectTextures[I] := nil;
    FRHIUserEffectSamplers[I] := nil;
  end;

  ProcessInterfaceDeclarations(Node.InterfaceDeclarations);
  for I := 0 to Node.FdShaders.Count - 1 do
    if (Node.FdShaders[I] is TComposedShaderNode) and
       (TComposedShaderNode(Node.FdShaders[I]).Language in [slDefault, slGLSL]) then
    begin
      ProcessInterfaceDeclarations(TComposedShaderNode(Node.FdShaders[I]).InterfaceDeclarations);
      Break;
    end;

  if UseNamedLayout and
     (FRHIUserEffectLayout.TotalUBOSize > MaxUploadEnd) and
     (FRHIUserEffectLayout.TotalUBOSize <= SizeOf(UBOData)) then
    MaxUploadEnd := FRHIUserEffectLayout.TotalUBOSize;

  FRHIUserEffectUniformCount := Result;
  if (Result = 0) and (not UseNamedLayout) then
    Exit;

  if MaxUploadEnd > 0 then
  begin
    AlignedSize := (MaxUploadEnd + 15) and not PtrUInt(15);
    if AlignedSize = 0 then
      AlignedSize := 16;
    if (FRHIUserEffectBuffer = nil) or (FRHIUserEffectBuffer.Desc.Size < AlignedSize) then
    begin
      FreeAndNil(FRHIUserEffectBuffer);
      BufDesc := TRHIBufferDesc.Create(AlignedSize, [buUniform], muCpuToGpu,
        'ScreenEffect.UserEffectUBO.' + Node.NiceName);
      FRHIUserEffectBuffer := RHIDevice.CreateBuffer(BufDesc, nil);
    end;
    Mapped := FRHIUserEffectBuffer.Map;
    if Mapped <> nil then
    try
      FillChar(Mapped^, AlignedSize, 0);
      Move(UBOData[0], Mapped^, MaxUploadEnd);
    finally
      FRHIUserEffectBuffer.Unmap;
    end;
    Inc(FRHIUserEffectUploadCount);
  end;

  if FRHIUserEffectDescriptorLayout = nil then
  begin
    FillChar(Set2Bindings, SizeOf(Set2Bindings), 0);
    Set2Bindings[0].Binding := 1;
    Set2Bindings[0].DescriptorType := dtUniformBuffer;
    Set2Bindings[0].Count := 1;
    Set2Bindings[0].StageFlags := [ssVertex, ssFragment];
    for I := 0 to CASTLE_MAX_USER_EFFECT_SAMPLERS - 1 do
    begin
      Set2Bindings[I + 1].Binding := 2 + Cardinal(I);
      Set2Bindings[I + 1].DescriptorType := dtCombinedImageSampler;
      Set2Bindings[I + 1].Count := 1;
      Set2Bindings[I + 1].StageFlags := [ssVertex, ssFragment];
    end;
    FRHIUserEffectDescriptorLayout := RHIDevice.CreateDescriptorSetLayout(Set2Bindings);
  end;

  if FRHIUserEffectDescriptorSet = nil then
    FRHIUserEffectDescriptorSet := RHIDevice.CreateDescriptorSet(FRHIUserEffectDescriptorLayout);

  if FRHIUserEffectBuffer <> nil then
    FRHIUserEffectDescriptorSet.BindUniformBuffer(1, FRHIUserEffectBuffer, 0, FRHIUserEffectBuffer.Desc.Size)
  else
  begin
    if FRHIUserEffectFallbackUBO = nil then
    begin
      FillChar(ZeroBytes, SizeOf(ZeroBytes), 0);
      BufDesc := TRHIBufferDesc.Create(SizeOf(ZeroBytes), [buUniform], muCpuToGpu,
        'ScreenEffect.FallbackUBO');
      FRHIUserEffectFallbackUBO := RHIDevice.CreateBuffer(BufDesc, @ZeroBytes[0]);
    end;
    if FRHIUserEffectFallbackUBO <> nil then
      FRHIUserEffectDescriptorSet.BindUniformBuffer(1, FRHIUserEffectFallbackUBO, 0, FRHIUserEffectFallbackUBO.Desc.Size);
  end;
  for I := 0 to CASTLE_MAX_USER_EFFECT_SAMPLERS - 1 do
    if FRHIUserEffectTextures[I] <> nil then
      FRHIUserEffectDescriptorSet.BindTexture(2 + Cardinal(I), FRHIUserEffectTextures[I], FRHIUserEffectSamplers[I])
    {$ifndef WASI}
    else if (RHIDevice is TVulkanRHIDevice) and (TVulkanRHIDevice(RHIDevice).Handle <> nil) then
      FRHIUserEffectDescriptorSet.BindTexture(2 + Cardinal(I), TVulkanRHIDevice(RHIDevice).GetDefaultWhiteTexture, nil)
    {$endif};
  FRHIUserEffectDescriptorSet.Update;
end;

procedure TScreenEffectResource.PrepareCore(const RenderOptions: TCastleRenderOptions);

  procedure PrepareIDecls(const IDecls: TX3DInterfaceDeclarationList);

    { If TextureNode <> @nil and is a texture node, prepare it. }
    procedure PrepareTexture(TextureNode: TX3DNode);
    begin
      if (TextureNode <> nil) and
         (TextureNode is TAbstractTextureNode) then
        TTextureResources.Prepare(RenderOptions, TAbstractTextureNode(TextureNode));
    end;

  var
    I, J: Integer;
    UniformField: TX3DField;
  begin
    if IDecls = nil then Exit; { ignore nodes without interface declararations }

    for I := 0 to IDecls.Count - 1 do
    begin
      UniformField := IDecls.Items[I].FieldOrLastEventValue;

      if UniformField <> nil then
      begin
        if UniformField is TSFNode then
        begin
          PrepareTexture(TSFNode(UniformField).Value);
        end else
        if UniformField is TMFNode then
        begin
          for J := 0 to TMFNode(UniformField).Count - 1 do
            PrepareTexture(TMFNode(UniformField)[J]);
        end;
      end;
    end;
  end;

  { Prepare all textures within X3D "interface declarations" of the given Nodes. }
  procedure PrepareIDeclsList(Nodes: TMFNode);
  var
    I: Integer;
  begin
    for I := 0 to Nodes.Count - 1 do
      PrepareIDecls(Nodes[I].InterfaceDeclarations);
  end;

  procedure PrepareRHIScreenEffect;
  var
    I, J: Integer;
    CompShader: TComposedShaderNode;
    Part: TShaderPartNode;
    UserVertSource, UserFragSource, UpgradedVertSource, ScreenLibFrag, DefaultMainVS: String;
    ShaderGen: TCastleShaderGeneratorRHI;
    Bindings: array of TRHIDescriptorBindingDesc;
    PipeDesc: TRHIGraphicsPipelineDesc;
    NeedsDepth, HasCustomVert: Boolean;
    ActiveStages: TRHIShaderStageFlags;
  begin
    CompShader := nil;
    UserVertSource := '';
    UserFragSource := '';
    for I := 0 to Node.FdShaders.Count - 1 do
      if (Node.FdShaders[I] is TComposedShaderNode) and
         (TComposedShaderNode(Node.FdShaders[I]).Language in [slDefault, slGLSL]) then
      begin
        CompShader := TComposedShaderNode(Node.FdShaders[I]);
        for J := 0 to CompShader.FdParts.Count - 1 do
          if CompShader.FdParts[J] is TShaderPartNode then
          begin
            Part := TShaderPartNode(CompShader.FdParts[J]);
            if Part.Contents <> '' then
              case Part.ShaderType of
                stVertex:
                  UserVertSource := UserVertSource + Part.Contents + LineEnding;
                stFragment:
                  UserFragSource := UserFragSource + Part.Contents + LineEnding;
                else ;
              end;
          end;
        Break;
      end;

    if (CompShader = nil) or (Trim(UserFragSource) = '') then
      Exit;

    NeedsDepth := Node.FdNeedsDepth.Value;
    HasCustomVert := Trim(UserVertSource) <> '';
    if HasCustomVert then
      ActiveStages := [ssVertex, ssFragment]
    else
      ActiveStages := [ssFragment];
    ShaderGen := TCastleShaderGeneratorRHI.Create(RHIDevice.Capabilities.Backend);
    try
      DefaultMainVS :=
        'void main(void)' + LineEnding +
        '{' + LineEnding +
        '  v_tex_coord = tex_coord;' + LineEnding +
        '  gl_Position = vec4(vertex, 0.0, 1.0);' + LineEnding +
        '}' + LineEnding;

      FRHIUserEffectLayout := ShaderGen.ExtractUserEffectLayout(
        UserVertSource, UserFragSource, '', 4);

      if HasCustomVert then
      begin
        UpgradedVertSource := ShaderGen.UpgradeUserEffectStageSource(
          ssVertex, UserVertSource, FRHIUserEffectLayout);
        if Pos('main(', UpgradedVertSource) = 0 then
          UpgradedVertSource := UpgradedVertSource + LineEnding + DefaultMainVS;
        FRHIVertexGLSL :=
          ShaderGen.GenerateScreenEffectPreamble(ssVertex, NeedsDepth, True) + LineEnding +
          UpgradedVertSource;
      end else
        FRHIVertexGLSL :=
          ShaderGen.GenerateScreenEffectPreamble(ssVertex, NeedsDepth, False) + LineEnding +
          DefaultMainVS;

      ScreenLibFrag :=
        'vec2 castle_effective_screen_size() { return (castle_Screen.screen_size.x > 0.5 && castle_Screen.screen_size.y > 0.5) ? castle_Screen.screen_size : vec2(max(textureSize(screen, 0), ivec2(1, 1))); }' + LineEnding +
        'ivec2 screen_position() { return ivec2(tex_coord * castle_effective_screen_size()); }' + LineEnding +
        'int screen_x() { return int(tex_coord.x * castle_effective_screen_size().x); }' + LineEnding +
        'int screen_y() { return int(tex_coord.y * castle_effective_screen_size().y); }' + LineEnding +
        'vec2 screenf_position() { return tex_coord * castle_effective_screen_size(); }' + LineEnding +
        'float screenf_x() { return tex_coord.x * castle_effective_screen_size().x; }' + LineEnding +
        'float screenf_y() { return tex_coord.y * castle_effective_screen_size().y; }' + LineEnding +
        'vec4 screenf_get_color(vec2 position) { return texture(screen, position / castle_effective_screen_size()); }' + LineEnding +
        'vec4 screenf_get_original_color() { return texture(screen, screenf_01_position); }' + LineEnding +
        'vec4 screenf_01_get_color(vec2 position) { return texture(screen, position); }' + LineEnding +
        'vec4 screen_get_color(ivec2 position) { return screenf_get_color(vec2(position) + vec2(0.5)); }' + LineEnding;
      if NeedsDepth then
        ScreenLibFrag := ScreenLibFrag +
          'float screenf_01_get_depth(vec2 position) { return texture(screen_depth, position).r; }' + LineEnding +
          'float screenf_get_depth(vec2 position) { return screenf_01_get_depth(position / castle_effective_screen_size()); }' + LineEnding +
          'float screenf_get_original_depth() { return screenf_01_get_depth(screenf_01_position); }' + LineEnding +
          'float screen_get_depth(ivec2 position) { return screenf_get_depth(vec2(position) + vec2(0.5)); }' + LineEnding +
          'float screenf_get_depth_fast(vec2 position) { return screenf_get_depth(position); }' + LineEnding +
          'float screen_get_depth_fast(ivec2 position) { return screen_get_depth(position); }' + LineEnding;

      FRHIFragmentGLSL :=
        ShaderGen.GenerateScreenEffectPreamble(ssFragment, NeedsDepth) + LineEnding +
        ScreenLibFrag + LineEnding +
        ShaderGen.UpgradeUserEffectStageSource(ssFragment, UserFragSource, FRHIUserEffectLayout);
    finally
      FreeAndNil(ShaderGen);
    end;

    UpdateRHIUniforms;
    CompShader.EventIsSelected.Send(True);
    CompShader.EventIsValid.Send(True);

    FRHIVertexShader := RHIDevice.CreateShaderModuleFromGLSL(ssVertex,
      FRHIVertexGLSL, 'ScreenEffectVS');
    FRHIFragmentShader := RHIDevice.CreateShaderModuleFromGLSL(ssFragment,
      FRHIFragmentGLSL, 'ScreenEffectFS');

    if NeedsDepth then
      SetLength(Bindings, 2)
    else
      SetLength(Bindings, 1);
    FillChar(Bindings[0], Length(Bindings) * SizeOf(TRHIDescriptorBindingDesc), 0);
    Bindings[0].Binding := 0;
    Bindings[0].DescriptorType := dtCombinedImageSampler;
    Bindings[0].Count := 1;
    Bindings[0].StageFlags := ActiveStages;
    if NeedsDepth then
    begin
      Bindings[1].Binding := 1;
      Bindings[1].DescriptorType := dtCombinedImageSampler;
      Bindings[1].Count := 1;
      Bindings[1].StageFlags := ActiveStages;
    end;
    FRHIDescriptorLayout := RHIDevice.CreateDescriptorSetLayout(Bindings);

    FillChar(PipeDesc, SizeOf(PipeDesc), 0);
    PipeDesc.VertexShader := FRHIVertexShader;
    PipeDesc.FragmentShader := FRHIFragmentShader;
    PipeDesc.Topology := ptTriangleList;
    PipeDesc.Rasterizer := TRHIRasterizerStateDesc.DefaultCullBack;
    PipeDesc.Rasterizer.CullMode := cmNone;
    PipeDesc.DepthStencil := TRHIDepthStencilStateDesc.DefaultDisabled;
    PipeDesc.BlendState := TRHIBlendStateDesc.DefaultOpaque;
    SetLength(PipeDesc.VertexInput.Bindings, 1);
    PipeDesc.VertexInput.Bindings[0].Binding := 0;
    PipeDesc.VertexInput.Bindings[0].Stride := SizeOf(TVector2) * 2;
    PipeDesc.VertexInput.Bindings[0].PerInstance := False;
    SetLength(PipeDesc.VertexInput.Attributes, 2);
    PipeDesc.VertexInput.Attributes[0].Location := 0;
    PipeDesc.VertexInput.Attributes[0].Binding := 0;
    PipeDesc.VertexInput.Attributes[0].Format := vafVector2;
    PipeDesc.VertexInput.Attributes[0].Offset := 0;
    PipeDesc.VertexInput.Attributes[1].Location := 1;
    PipeDesc.VertexInput.Attributes[1].Binding := 0;
    PipeDesc.VertexInput.Attributes[1].Format := vafVector2;
    PipeDesc.VertexInput.Attributes[1].Offset := SizeOf(TVector2);
    SetLength(PipeDesc.ColorAttachmentFormats, 1);
    PipeDesc.ColorAttachmentFormats[0] := pfRGBA8_UNorm;
    PipeDesc.SampleCount := 1;
    if FRHIUserEffectDescriptorLayout <> nil then
    begin
      SetLength(PipeDesc.DescriptorSetLayouts, 3);
      PipeDesc.DescriptorSetLayouts[0] := FRHIDescriptorLayout;
      PipeDesc.DescriptorSetLayouts[1] := nil;
      PipeDesc.DescriptorSetLayouts[2] := FRHIUserEffectDescriptorLayout;
    end else
    begin
      SetLength(PipeDesc.DescriptorSetLayouts, 1);
      PipeDesc.DescriptorSetLayouts[0] := FRHIDescriptorLayout;
    end;
    PipeDesc.PushConstantSize := 32; // SizeOf(TCastleScreenPushConstants)
    PipeDesc.PushConstantStages := ActiveStages;
    PipeDesc.DebugName := 'ScreenEffectPSO.' + Node.NiceName;
    FRHIPipeline := RHIDevice.GetOrCreatePipeline(PipeDesc);
  end;

var
  Shader: TShader;
  ShaderNode: TComposedShaderNode;
  DummyCamera: TRenderingCamera;
begin
  if not ShaderLoaded then
  begin
    Assert(ShaderProgram = nil);
    FRenderOptions := RenderOptions;
    if not Node.FdEnabled.Value then
      Exit;
    ShaderLoaded := true;

    if RHIDevice <> nil then
    begin
      PrepareIDecls(Node.InterfaceDeclarations);
      PrepareIDeclsList(Node.FdShaders);
      PrepareRHIScreenEffect;
      Exit;
    end;

    { Note: Strictly speaking, we should not need to check
      "not GLFeatures.EnableFixedFunction" below.
      Without this check, TX3DGLSLProgram.Create below will raise an exception
      which will cause to abort.

      But:
      - This is a bit inefficient way to abort.
      - It causes unknown issues: castle-model-viewer --debug-fixed-function,
        enable any (like "visualize depth") screen effect,
        and you may experience crash in TCastleScene.ScreenEffectsCount
        as if ScreenEffectNodes was nil.

        Not debugged to the end:
        situation seems impossible and in debugger mysteriously
        doesn't occur. And logging inconclusive:

          WritelnLog('TCastleScene.ScreenEffectsCount %s', [BoolToStr(ScreenEffectNodes <> nil, true)]);

        sometimes shown we indeed have nil right before crash,
        but sometimes not...
    }

    if Node.FdEnabled.Value and (GLFeatures <> nil) and (not GLFeatures.EnableFixedFunction) then
    begin
      { make sure that textures inside shaders are prepared }
      PrepareIDecls(Node.InterfaceDeclarations);
      PrepareIDeclsList(Node.FdShaders);

      Shader := TShader.Create;
      try
        DummyCamera := TRenderingCamera.Create;
        try
          Shader.RenderingCamera := DummyCamera;

          { for ScreenEffect, we require that some ComposedShader was present.
            Rendering with default TShader shader makes no sense. }
          if Shader.EnableCustomShaderCode(Node.FdShaders, ShaderNode) then
          try
            ShaderProgram := TX3DGLSLProgram.Create;
            Shader.AddScreenEffectCode(Node.FdNeedsDepth.Value);
            Shader.LinkProgram(ShaderProgram, Node.NiceName);

            { We have to ignore invalid uniforms, as it's normal that when
              rendering screen effect we will pass some screen_* variables
              that you will not use. }
            ShaderProgram.UniformMissing := umIgnore;
          except on E: EGLSLError do
            begin
              FreeAndNil(ShaderProgram);
              WritelnWarning('Renderer', Format('Cannot use GLSL shader for ScreenEffect: %s',
                [E.Message]));
            end;
          end;
        finally FreeAndNil(DummyCamera) end;
      finally FreeAndNil(Shader) end;
    end;
  end;
end;

procedure TScreenEffectResource.UnprepareCore;
var
  I: Integer;
begin
  FreeAndNil(ShaderProgram);
  FreeAndNil(FRHIUserEffectDescriptorSet);
  FreeAndNil(FRHIUserEffectDescriptorLayout);
  FreeAndNil(FRHIUserEffectBuffer);
  FreeAndNil(FRHIUserEffectFallbackUBO);
  FRHIUserEffectLayout := Default(TUserEffectShaderLayout);
  for I := 0 to High(FRHIUserEffectTextures) do
  begin
    FRHIUserEffectTextures[I] := nil;
    FRHIUserEffectSamplers[I] := nil;
  end;
  FRHIUserEffectUniformCount := 0;
  FRHIUserEffectUploadCount := 0;
  FreeAndNil(FRHIDescriptorLayout);
  FreeAndNil(FRHIFragmentShader);
  FreeAndNil(FRHIVertexShader);
  FRHIPipeline := nil; // Owned by RHIDevice pipeline cache
  FRHIVertexGLSL := '';
  FRHIFragmentGLSL := '';
  FRenderOptions := nil;
  ShaderLoaded := false;
end;

constructor TScreenEffectResource.Create(const ANode: TX3DNode);
begin
  inherited;
  if not (ANode is TScreenEffectNode) then
    raise EInternalError.Create('TScreenEffectResource.Create: ANode must be TScreenEffectNode');
  FNode := TScreenEffectNode(ANode);
end;

{$endif read_implementation}
