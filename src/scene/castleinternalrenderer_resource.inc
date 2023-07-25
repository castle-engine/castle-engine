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

{$ifdef read_interface}
  { Resource connected to rendering (so connected to OpenGL context)
    of a given X3D node.

    Prepare it to increase reference count,
    Unprepare it (also done at destruction) to decrease reference count.

    Descendants implementors: Be careful when implementing this.
    The lifetime of this instance is connected to the lifetime of the X3D node.
    It should not assume anything else, e.g. that some TRenderer
    or TCastleRenderOptions instance continues existing. }
  TRendererResource = class(TInternalRendererResource)
  strict private
    FNode: TX3DNode;

    { Release stuff allocated by Prepare.
      For now this just calls virtual UnprepareCore. }
    procedure Unprepare;
  protected
    { Do the time-consuming preparations before using this node with renderer.

      Called always from our Prepare method. Our Prepare method takes care to catch
      common exceptions from this (EFramebufferError, ETextureLoadError),
      convert them to WritelnWarning and make Unprepare to undo the preparations.
      So Prepare-Unprepare work like constructor-destructor, with Unprepare
      having to be prepared to eventually finalize even incomplete instance. }
    procedure PrepareCore(const RenderOptions: TCastleRenderOptions); virtual; abstract;

    { Release all resources allocated by Prepare method.
      This is exposed so that you can override it. }
    procedure UnprepareCore; virtual; abstract;
  public
    constructor Create(const ANode: TX3DNode); virtual;
    destructor Destroy; override;

    { Reference to handled X3D node.
      Never @nil. }
    property Node: TX3DNode read FNode;

    { Do the time-consuming preparations before using this node with renderer.
      Calls PrepareCore. }
    procedure Prepare(const RenderOptions: TCastleRenderOptions);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TRendererResource ---------------------------------------------------------- }

constructor TRendererResource.Create(const ANode: TX3DNode);
begin
  inherited Create;
  FNode := ANode;
end;

destructor TRendererResource.Destroy;
begin
  Unprepare;
  inherited;
end;

procedure TRendererResource.Prepare(const RenderOptions: TCastleRenderOptions);
begin
  try
    PrepareCore(RenderOptions);
  except
    on E: EFramebufferError do
    begin
      Unprepare;
      WritelnWarning('Renderer', 'Framebuffer error, generated texture not possible: '
        + E.Message);
    end;

    on E: ETextureLoadError do
    begin
      Unprepare;
      WritelnWarning('Renderer', 'Cannot load texture to OpenGL: ' + E.Message);
    end;
  end;
end;

procedure TRendererResource.Unprepare;
begin
  UnprepareCore;
end;

{$endif}
