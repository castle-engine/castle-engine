{%MainUnit castlecontrols.pas}
{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { Effect of mask over rendering, used by @link(TCastleMask.MaskRendering). }
  TMaskRendering = (
    mrChildrenWhereNoMask,
    mrChildrenWhereMask,
    mrChildrenIgnoreMask,
    mrMask
  );

  { Limit the visibility of children by an arbitrary (defined using an arbitrary UI) mask.

    The children controls are visible only where the @link(Mask) is rendered,
    or where the mask is not rendered.
    You can customize the mask effect using @link(MaskRendering).

    Limitation of the current implementation:
    You cannot have other controls
    underneath TCastleMask (inside @link(Mask) or inside other children) that
    deal with stencil buffer.
    In practice, it means that you cannot use TCastleMask within another TCastleMask,
    and you cannot use TCastleViewport with shadow volumes within TCastleMask.
    Set @link(TCastleViewport.ShadowVolumes) to @false to make @link(TCastleViewport)
    reliable inside a TCastleMask.
    TODO: This limitation may be removed one day, it's possible by using distinct
    stencil buffer bits (masks). }
  TCastleMask = class(TCastleUserInterface)
  strict private
    FMask: TCastleUserInterface;
    FMaskRendering: TMaskRendering;
    procedure SetMaskRendering(const Value: TMaskRendering);
  protected
    procedure InternalRenderLoop(const ViewportRect: TRectangle); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Place in @link(Mask) any UI control (TCastleUserInterface) to limit
      where the children are rendered.

      The colors of the mask UI don't matter, but the pixels that are touched by the mask matter.
      So you can e.g. define a mask using an image (TCastleImageControl),
      make sure to set @link(TCastleImageControl.AlphaChannel) to acTest to make the mask apply
      where the image is opaque.
      Or you can define a mask using a viewport with 2D or 3D scenes -- TCastleViewport,
      with @link(TCastleViewport.Transparent).
      Or you can define a mask using a shape, like TCastleRectangleControl or TCastleShape.

      The @link(Mask) is a subcomponent with @link(TCastleUserInterface.FullSize) = @true by default,
      so it fills the entire TCastleMask by default.

      The children in @link(Mask) are never rendered to the color or depth buffers
      (unless MaskRendering is mrMask).
      They are only rendered to set the stencil buffer. }
    property Mask: TCastleUserInterface read FMask;

    { How children of this control are limited by the mask.

      @unorderedList(
        @item(mrChildrenWhereNoMask (default):

          Children are rendered only where mask is @italic(not) visible.

          This is the default, and it means that when mask is empty
          (also the default state) then the children are rendered as usual.
        )

        @item(mrChildrenWhereMask:

          Children are rendered only where mask is visible.)

        @item(mrChildrenIgnoreMask:

          Turns off mask usage, effectively making TCastleMask a regular UI control
          and the contents of @link(Mask) meaningless.)

        @item(mrMask:

          Render only mask, as a regular UI control.
          Other children are not rendered.
          This mode is useful to design mask at design-time, where it may be easier
          to just see mask UI explicitly.)
      )

      Note that mrChildrenWhereNoMask and mrChildrenWhereMask values only work when there
      is a stencil buffer available.
      If the stencil buffer is not available, they behave as if
      @link(MaskRendering) = mrChildrenIgnoreMask, so the mask will not do anything.
      All modern GPUs provide a stencil buffer, and CGE requests stencil buffer by default.
      Check @code(GLFeatures.StencilBits <> 0) if you want to react to lack of stencil buffer
      in some particular way in your code. }
    property MaskRendering: TMaskRendering read FMaskRendering write SetMaskRendering
      default mrChildrenWhereNoMask;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleMask -------------------------------------------------------------- }

constructor TCastleMask.Create(AOwner: TComponent);
begin
  inherited;

  FMask := TCastleUserInterface.Create(Self);
  FMask.SetSubComponent(true);
  FMask.FullSize := true;
  FMask.Name := 'Mask';
  InsertFront(FMask);
end;

destructor TCastleMask.Destroy;
begin
  inherited;
end;

function TCastleMask.PropertySections(const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Mask', 'MaskRendering'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleMask.SetMaskRendering(const Value: TMaskRendering);
begin
  if FMaskRendering <> Value then
  begin
    FMaskRendering := Value;
    VisibleChange([chRender]);
  end;
end;

procedure TCastleMask.InternalRenderLoop(const ViewportRect: TRectangle);
var
  SavedDepthBufferUpdate, SavedInternalClearColorsByDraw: Boolean;
  SavedColorChannels: TColorChannels;
  I: Integer;
begin
  { Early test for special case when we render Mask for debug.
    We do this regardless of GLFeatures.StencilBits, so check this special case first. }
  if MaskRendering = mrMask then
  begin
    Mask.InternalRecursiveRender(ViewportRect);
    Exit;
  end;

  { Early test for special case when we don't use Mask. }
  if (MaskRendering = mrChildrenIgnoreMask) or (GLFeatures.StencilBits = 0) then
  begin
    for I := 0 to ControlsCount - 1 do
      if Controls[I] <> Mask then
        Controls[I].InternalRecursiveRender(ViewportRect);
    Exit;
  end;

  { The usual rendering mode: mrChildrenWhereMask or mrChildrenWhereNoMask.
    Render mask into stencil buffer, then render the rest with stencil test.

    Note that we don't try to save/restore the OpenGL state of
    glStencilFunc, glStencilOp.
    All CGE operations using stencil buffer must set this state
    before rendering: this applies to TCastleMask and TGLShadowVolumeRenderer. }

  RenderContext.Clear([cbStencil], { ClearColor is meaningless for clearing [cbStencil] } TVector4.Zero);

  glEnable(GL_STENCIL_TEST);

    { render Mask to set stencil buffer }

    SavedDepthBufferUpdate := RenderContext.DepthBufferUpdate;
    SavedColorChannels := RenderContext.ColorChannels;

    { Don't write anything to depth or color buffers. }
    RenderContext.DepthBufferUpdate := false;
    RenderContext.ColorChannels := [];

    glStencilFunc(GL_ALWAYS, 1, 0);
    { GL_REPLACE means to set stencil to 1 ("ref" argument passed to glStencilFunc)
      where the stencil and depth test pass }
    glStencilOp(GL_KEEP, GL_KEEP, GL_REPLACE);
    Mask.InternalRecursiveRender(ViewportRect);

    RenderContext.DepthBufferUpdate := SavedDepthBufferUpdate;
    RenderContext.ColorChannels := SavedColorChannels;

    { render other children, honoring the stencil buffer }

    glStencilOp(GL_KEEP, GL_KEEP, GL_KEEP); // do not modify stencil anymore
    case MaskRendering of
      mrChildrenWhereMask  : glStencilFunc(GL_EQUAL, 1, 1);
      mrChildrenWhereNoMask: glStencilFunc(GL_NOTEQUAL, 1, 1);
      else raise EInternalError.Create('MaskRendering?');
    end;

    { Using InternalClearColorsByDraw = true means that solid-color background done
      by TCastleViewport (in case it is in TCastleMask children) is properly masked by stencil. }
    SavedInternalClearColorsByDraw := RenderContext.InternalClearColorsByDraw;
    RenderContext.InternalClearColorsByDraw := true;

    for I := 0 to ControlsCount - 1 do
      if Controls[I] <> Mask then
        Controls[I].InternalRecursiveRender(ViewportRect);

    RenderContext.InternalClearColorsByDraw := SavedInternalClearColorsByDraw;

  glDisable(GL_STENCIL_TEST);
end;

{$endif read_implementation}
