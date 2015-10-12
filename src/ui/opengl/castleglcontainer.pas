{
  Copyright 2009-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container for 2D controls able to render using OpenGL (TGLContainer). }
unit CastleGLContainer;

{$I castleconf.inc}

interface

uses CastleUIControls;

type
  { Container for controls providing an OpenGL rendering.
    This class is internally used by TCastleWindowCustom and TCastleControlCustom.
    It is not useful from the outside, unless you want to implement
    your own container provider similar to TCastleWindowCustom / TCastleControlCustom. }
  TGLContainer = class abstract(TUIContainer)
  public
    procedure EventRender; override;
  end;

implementation

uses CastleVectors, CastleGL, CastleGLUtils;

procedure TGLContainer.EventRender;

  procedure ControlRenderBegin;
  begin
    { Set state that is guaranteed for Render2D calls,
      but TUIControl.Render cannot change it carelessly. }
    {$ifndef OpenGLES}
    glDisable(GL_LIGHTING);
    glDisable(GL_FOG);
    {$endif}
    glDisable(GL_DEPTH_TEST);
    ScissorDisable;
    GLEnableTexture(CastleGLUtils.etNone);
    glViewport(Rect);
    OrthoProjection(0, Width, 0, Height);

    { Set OpenGL state that may be changed carelessly, and has some
      guaranteed value, for Render2d calls. }
    {$ifndef OpenGLES} glLoadIdentity; {$endif}
    CastleGLUtils.WindowPos := Vector2LongInt(0, 0);
  end;

  procedure RenderWithChildren(const C: TUIControl;
    var SomeControlHasRenderStyle2D: boolean;
    const FilterRenderStyle: TRenderStyle);
  var
    I: Integer;
  begin
    if C.GetExists then
    begin
      { We check C.GLInitialized, because it may happen that a control
        did not receive GLContextOpen yet, in case we cause some rendering
        during TUIContainer.EventOpen (e.g. because some TUIControl.GLContextOpen
        calls Window.Screenshot, so everything is rendered
        before even the rest of controls received TUIControl.GLContextOpen).
        See castle_game_engine/tests/testcontainer.pas . }

      if C.GLInitialized then
      begin
        if C.RenderStyle = FilterRenderStyle then
        begin
          ControlRenderBegin;
          C.Render;
        end;
        if C.RenderStyle = rs2D then
          SomeControlHasRenderStyle2D := true;
      end;

      for I := 0 to C.ControlsCount - 1 do
        RenderWithChildren(C.Controls[I], SomeControlHasRenderStyle2D, FilterRenderStyle);
    end;
  end;

  procedure RenderEverything(const FilterRenderStyle: TRenderStyle; out SomeControlHasRenderStyle2D: boolean);
  var
    I: Integer;
  begin
    SomeControlHasRenderStyle2D := false;

    { draw controls in "to" order, back to front }
    for I := 0 to Controls.Count - 1 do
      RenderWithChildren(Controls[I], SomeControlHasRenderStyle2D, FilterRenderStyle);

    if TooltipVisible and (Focus <> nil) then
    begin
      if Focus.TooltipStyle = FilterRenderStyle then
      begin
        ControlRenderBegin;
        Focus.TooltipRender;
      end;
      if Focus.TooltipStyle = rs2D then
        SomeControlHasRenderStyle2D := true;
    end;

    if RenderStyle = FilterRenderStyle then
    begin
      ControlRenderBegin;
      if Assigned(OnRender) then OnRender(Self);
    end;
    if RenderStyle = rs2D then
      SomeControlHasRenderStyle2D := true;
  end;

var
  SomeControlHasRenderStyle2D, Dummy: boolean;
begin
  { Required to make DrawRectangle and TGLImage.Draw correct. }
  Viewport2DSize[0] := Width;
  Viewport2DSize[1] := Height;

  RenderEverything(rs3D, SomeControlHasRenderStyle2D);
  if SomeControlHasRenderStyle2D then
    RenderEverything(rs2D, Dummy);
end;

end.
