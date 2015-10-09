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

  { Call Render for all controls having RenderStyle = rs3D.

    Also (since we call RenderStyle for everything anyway)
    calculates AnythingWants2D = if any control returned RenderStyle = rs2D.
    If not, you can later avoid even changing projection to 2D. }
  procedure Render3D(out AnythingWants2D: boolean);
  var
    I: Integer;
    C: TUIControl;
  begin
    AnythingWants2D := false;

    { draw controls in "to" order, back to front }
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      { We check C.GLInitialized, because it may happen that a control
        did not receive GLContextOpen yet, in case we initialize some rendering
        during TUIContainer.EventOpen.
        See castle_game_engine/tests/testcontainer.pas for cases
        when this is really needed. Although right now the container OnOpen
        is always after all TUIControl.GLContextOpen, but the problem may
        still occur if another control does SaveScreen during it's
        own GLContextOpen. }
      if C.GetExists and C.GLInitialized then
        case C.RenderStyle of
          rs2D: AnythingWants2D := true;
          { Set OpenGL state that may be changed carelessly, and has some
            guanteed value, for TUIControl.Render calls.
            For now, just glLoadIdentity. }
          rs3D: begin ControlRenderBegin; C.RenderWithChildren; end;
        end;
    end;

    if TooltipVisible and (Focus <> nil) then
      case Focus.TooltipStyle of
        rs2D: AnythingWants2D := true;
        rs3D: begin ControlRenderBegin; Focus.TooltipRender; end;
      end;

    case RenderStyle of
      rs2D: AnythingWants2D := true;
      rs3D: begin ControlRenderBegin; if Assigned(OnRender) then OnRender(Self); end;
    end;
  end;

  procedure Render2D;
  var
    C: TUIControl;
    I: Integer;
  begin
    { draw controls in "to" order, back to front }
    for I := 0 to Controls.Count - 1 do
    begin
      C := Controls[I];
      if C.GetExists and C.GLInitialized and (C.RenderStyle = rs2D) then
      begin
        ControlRenderBegin;
        C.RenderWithChildren;
      end;
    end;

    if TooltipVisible and (Focus <> nil) and (Focus.TooltipStyle = rs2D) then
    begin
      ControlRenderBegin;
      Focus.TooltipRender;
    end;

    if RenderStyle = rs2D then
    begin
      ControlRenderBegin;
      if Assigned(OnRender) then OnRender(Self);
    end;
  end;

var
  AnythingWants2D: boolean;
begin
  { Required to make DrawRectangle and TGLImage.Draw correct. }
  Viewport2DSize[0] := Width;
  Viewport2DSize[1] := Height;

  Render3D(AnythingWants2D);

  if AnythingWants2D then
    Render2D;
end;

end.
