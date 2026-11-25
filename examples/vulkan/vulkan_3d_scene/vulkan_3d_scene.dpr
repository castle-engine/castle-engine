{
  Copyright 2026-2026 Michalis Kamburelis and Castle Game Engine developers.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{ Example demonstrating the Castle Game Engine Next-Gen Vulkan RHI pipeline:
  - Shader pre-warming (SPIR-V cache)
  - Per-Pass Scene Camera UBO (Set 0, Binding 0) & Multi-light UBO (Set 0, Binding 1)
  - Shadow Map depth-only dynamic rendering pass (TRHIRenderingInfo.CreateDepthOnly)
  - GPU Skinning joint matrix UBO (Set 2, Binding 0)
  - High-level TCastleRenderOptions & TCastleViewport Next-Gen subsystem orchestration
  - TCastleWindow cross-platform WSI VkSurfaceKHR descriptor resolution }
program vulkan_3d_scene;

{$ifdef FPC}{$mode objfpc}{$H+}{$endif}

uses
  {$if defined(FPC) and defined(UNIX) and (not defined(CASTLE_DISABLE_THREADS))}
  CThreads,
  {$endif}
  SysUtils, Classes,
  CastleVectors, CastleColors, CastleRectangles, CastleLog,
  CastleApplicationProperties, CastleRenderOptions, CastleViewport, CastleScene,
  CastleTransform, CastleUIControls, CastleControls, CastleComponentSerialize,
  CastleCameras, CastleWindow, CastleParameters, CastleRenderContext, X3DNodes,
  CastleInternalRHI, CastleInternalShaderRHI, CastleInternalShaderCompiler,
  CastleInternalVulkan, CastleInternalVulkanWSI, CastleInternalRHI_Vulkan;

type
  TVulkanDemoController = class(TCastleUserInterface)
  public
    Viewport: TCastleViewport;
    GoldenBox: TCastleBox;
    StatusLabel: TCastleLabel;
    Angle: Single;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

procedure TVulkanDemoController.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  Angle := Angle + SecondsPassed * 1.2;
  if GoldenBox <> nil then
    GoldenBox.Rotation := Vector4(0.2, 1.0, 0.15, Angle);
  if (StatusLabel <> nil) and (Viewport <> nil) then
    StatusLabel.Caption := Format(
      'Backend: Vulkan 1.3 RHI | FPS: %s | GI: Lumen SDF+SurfaceCache | TSR + VRS | EV100: %.1f',
      [Container.Fps.ToString, Viewport.EvaluateCurrentCameraEV100]);
end;

var
  ActiveBackend: TRenderBackend;
  PrewarmedHashes: TUInt64Array;
  SceneUBO: TCastleSceneUniformBlock;
  LightsUBO: TCastleLightsUniformBlock;
  SkinUBO: TCastleSkinningUniformBlock;
  ShadowPassInfo: TRHIRenderingInfo;
  Device: TVulkanRHIDevice;
  Viewport: TCastleViewport;
  Scene: TCastleScene;
  Win: TCastleWindow;
  DesignRoot: TCastleUserInterface;
  DesignOwner: TComponent;
  Navigation: TCastleExamineNavigation;
  Controller: TVulkanDemoController;
  SurfaceDesc: TVulkanNativeSurfaceDesc;
  WSIExts: TAnsiStringArray;
  ExecutedSubsystems: Cardinal;
  HeadlessMode: Boolean;
  RootNode: TX3DRootNode;
  BoxShape: TShapeNode;
  BoxGeom: TBoxNode;
  BoxApp: TAppearanceNode;
  BoxMat: TPhysicalMaterialNode;
  DirLight: TDirectionalLightNode;
  FogNode: TFogNode;
  SavedCtx, HeadlessCtx: TRenderContext;
  I: Integer;
begin
  ApplicationProperties._FileAccessSafe := true;
  InitializeLog;
  ApplicationProperties.RenderBackend := backendVulkan;
  ActiveBackend := SelectOptimalRenderBackend(ApplicationProperties.RenderBackend, True, 1, 3);
  HeadlessMode := Parameters.IndexOf('--headless') <> -1;

  WritelnLog('Vulkan3DScene', 'Castle Game Engine - Vulkan 3D Forward Pass & Next-Gen RHI Example');
  WritelnLog('Vulkan3DScene', 'Selected optimal render backend: %d (Vulkan=3)', [Ord(ActiveBackend)]);

  { 1. Pre-warm core engine shaders into SPIR-V cache }
  PrewarmedHashes := TCastleShaderCompiler.Instance.PrewarmCoreEngineShaders;
  WritelnLog('Vulkan3DScene', 'Pre-warmed %d core GLSL 450 shader stages into SPIR-V cache',
    [Length(PrewarmedHashes)]);

  { 2. Populate Set 0 Binding 0 Scene Camera UBO and Set 0 Binding 1 Multi-Light UBO }
  FillChar(SceneUBO, SizeOf(SceneUBO), 0);
  SceneUBO.ProjectionMatrix := TMatrix4.Identity;
  SceneUBO.ViewMatrix := TMatrix4.Identity;
  SceneUBO.ViewMatrixInverse := TMatrix4.Identity;
  SceneUBO.LightCount := 2;

  LightsUBO.Clear;
  LightsUBO.AddDirectionalLight(Vector3(0.0, -1.0, -0.5), Vector3(1.0, 0.98, 0.9), Vector3(0.2, 0.2, 0.2));
  LightsUBO.SetProjectorMatrix(0, TMatrix4.Identity);
  LightsUBO.AddPointLight(Vector3(4.0, 6.0, 2.0), Vector3(0.3, 0.6, 1.0), Vector3(0.05, 0.05, 0.05),
    Vector3(1.0, 0.0, 0.1), 25.0);

  { 3. Configure Depth-Only Dynamic Rendering Pass for Shadow Maps }
  ShadowPassInfo := TRHIRenderingInfo.CreateDepthOnly(Rectangle(0, 0, 2048, 2048), nil, loClear, soStore, 1.0);
  WritelnLog('Vulkan3DScene', 'Configured %dx%d depth-only shadow pass (SceneUBO=%d B, LightsUBO=%d B, Lights=%d)',
    [ShadowPassInfo.RenderArea.Width, ShadowPassInfo.RenderArea.Height,
     SizeOf(SceneUBO), SizeOf(LightsUBO), LightsUBO.Count]);

  { 4. Populate Set 2 Binding 0 GPU Skinning UBO }
  FillChar(SkinUBO, SizeOf(SkinUBO), 0);
  SkinUBO.JointCount := 16;
  for I := 0 to Integer(SkinUBO.JointCount) - 1 do
    SkinUBO.JointMatrices[I] := TMatrix4.Identity;
  WritelnLog('Vulkan3DScene', 'Initialized GPU skinning UBO for %d active bones (%d B)',
    [SkinUBO.JointCount, SizeOf(SkinUBO)]);

  { 5. Exercise High-Level TCastleRenderOptions, 3D X3D Scene Preparation & Next-Gen Subsystem Orchestration }
  SavedCtx := RenderContext;
  HeadlessCtx := TRenderContext.Create(nil);
  Device := TVulkanRHIDevice.CreateHeadless(VkPhysicalDevice(PtrUInt($7701)));
  Viewport := TCastleViewport.Create(nil);
  Scene := TCastleScene.Create(nil);
  Win := TCastleWindow.Create(nil);
  try
    SetRHIDevice(Device);
    RenderContext := HeadlessCtx;

    RootNode := TX3DRootNode.Create;
    DirLight := TDirectionalLightNode.Create;
    DirLight.Direction := Vector3(-0.3, -0.8, -0.5).Normalize;
    DirLight.Global := True;
    RootNode.AddChildren(DirLight);

    FogNode := TFogNode.Create;
    FogNode.Color := Vector3(0.5, 0.6, 0.75);
    FogNode.VisibilityRange := 60.0;
    RootNode.AddChildren(FogNode);

    BoxShape := TShapeNode.Create;
    BoxGeom := TBoxNode.Create;
    BoxGeom.Size := Vector3(2.0, 2.0, 2.0);
    BoxShape.Geometry := BoxGeom;
    BoxApp := TAppearanceNode.Create;
    BoxMat := TPhysicalMaterialNode.Create;
    BoxMat.BaseColor := Vector3(0.95, 0.75, 0.25);
    BoxMat.Metallic := 0.85;
    BoxMat.Roughness := 0.2;
    BoxApp.Material := BoxMat;
    BoxShape.Appearance := BoxApp;
    RootNode.AddChildren(BoxShape);
    Scene.Load(RootNode, True);

    Scene.RenderOptions.SpecularAntiAliasing := True;
    Scene.RenderOptions.DualQuaternionSkinning := True;
    Scene.RenderOptions.GpuDrivenMeshletCulling := True;
    Scene.RenderOptions.TransparencyMode := tmWeightedBlendedOIT;
    Scene.RenderOptions.ShadowMapSystemMode := smsVirtualShadowMaps;
    Viewport.Items.Add(Scene);
    Viewport.PrepareResources(Scene);

    Viewport.Width := 1280;
    Viewport.Height := 720;
    Viewport.ScreenSpaceAmbientOcclusion := True;
    Viewport.ScreenSpaceReflections := True;
    Viewport.VolumetricFogEnabled := True;
    Viewport.GlobalIlluminationMode := giLumenSDFAndSurfaceCache;
    Viewport.TemporalAntiAliasingMode := taaTSRUpscaler;
    Viewport.VariableRateShadingMode := vrsContrastAdaptive;
    Viewport.NextGenToneMappingMode := ntmAgXGamutCompressed;
    Viewport.AutoExposureEnabled := True;
    Viewport.TargetFrameRateHz := 60.0;

    Device.BeginFrame;
    try
      ExecutedSubsystems := Viewport.ExecuteVulkanViewportSubsystems(ActiveCommandList, 0.016) +
        Viewport.ExecuteAllScenesRenderOptionsSubsystems(ActiveCommandList);
    finally
      Device.EndFrameAndPresent;
    end;
    WritelnLog('Vulkan3DScene',
      'Executed %d Next-Gen Vulkan viewport & scene subsystems (EV100=%.2f, ExposureMultiplier=%.4f, RenderBackend=%s, Frames=%d)',
      [ExecutedSubsystems,
       Viewport.EvaluateCurrentCameraEV100,
       Viewport.EvaluateCurrentCameraExposureMultiplier,
       Scene.RenderOptions.ActiveRenderBackendName,
       Device.TotalFramesPresented]);

    { 6. Verify TCastleWindow WSI Native Surface Descriptor & Extensions }
    SurfaceDesc := Win.BuildVulkanNativeSurfaceDesc;
    WSIExts := GetRequiredVulkanInstanceExtensionsForPlatform(SurfaceDesc.Platform);
    WritelnLog('Vulkan3DScene',
      'TCastleWindow WSI Platform=%d, IsVulkanBackendRequested=%s, InstanceExtensions=%d',
      [Ord(SurfaceDesc.Platform),
       BoolToStr(Win.IsVulkanBackendRequested, True),
       Length(WSIExts)]);

    FreeAndNil(Scene);
    FreeAndNil(Viewport);
    RenderContext := SavedCtx;
    SetRHIDevice(nil);
    if not HeadlessMode then
    begin
      Application.MainWindow := Win;
      Win.Caption := 'Castle Game Engine - Vulkan 1.3 RHI 3D Scene & Lighting';
      Win.Width := 1280;
      Win.Height := 720;
      DesignOwner := TComponent.Create(Win);
      DesignRoot := UserInterfaceLoad('castle-data:/gameviewmain.castle-user-interface', DesignOwner);
      Win.Controls.InsertFront(DesignRoot);

      Controller := TVulkanDemoController.Create(Win);
      Controller.Viewport := DesignOwner.FindComponent('VulkanViewport3D') as TCastleViewport;
      Controller.GoldenBox := DesignOwner.FindComponent('VulkanGoldenBox') as TCastleBox;
      Controller.StatusLabel := DesignOwner.FindComponent('LabelVulkanStatus') as TCastleLabel;
      if Controller.Viewport <> nil then
      begin
        Controller.Viewport.GlobalIlluminationMode := giLumenSDFAndSurfaceCache;
        Controller.Viewport.TemporalAntiAliasingMode := taaTSRUpscaler;
        Controller.Viewport.VariableRateShadingMode := vrsContrastAdaptive;
        Controller.Viewport.NextGenToneMappingMode := ntmAgXGamutCompressed;
        Controller.Viewport.VolumetricFogEnabled := True;
        Controller.Viewport.AutoExposureEnabled := True;
        Navigation := TCastleExamineNavigation.Create(DesignOwner);
        Controller.Viewport.InsertBack(Navigation);
      end;
      Win.Controls.InsertBack(Controller);
      Win.OpenAndRun;
    end;
  finally
    FreeAndNil(Scene);
    FreeAndNil(Viewport);
    RenderContext := SavedCtx;
    SetRHIDevice(nil);
    FreeAndNil(Win);
    FreeAndNil(HeadlessCtx);
    FreeAndNil(Device);
  end;
end.
