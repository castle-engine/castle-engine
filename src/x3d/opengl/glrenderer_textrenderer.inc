{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

type
  { Our text renderer, for Text and Text3D geometry.

    TODO: our text rendering completely omits the whole TGeometryArrays
    and all features coming from it. So multi-texturing, texture generation,
    vertex arrays and VBOs, and many other features simply don't work.
    In the future, this should be changed: text rendering should simply
    follow other nodes, and create triangles arrays in
    GenerateCoordinate, and let normal rendering through TGeometryArrays
    handle it. }
  TAbstractTextRenderer = class(TVRMLMeshRenderer)
  protected
    procedure RenderText(
      const Size, Spacing: Single;
      const Justify: TX3DFontJustify;
      const Strings: TCastleStringList;
      CurrentFont: TGLOutlineFont;
      const Depth: Single;
      const CullBackFaces: boolean;
      const EventLineBounds, EventOrigin, EventTextBounds: TX3DEvent);
  end;

  TAsciiTextRenderer = class(TAbstractTextRenderer)
  public
    procedure Render; override;
  end;

  TTextRenderer = class(TAbstractTextRenderer)
  public
    procedure Render; override;
  end;

  TText3DRenderer = class(TAbstractTextRenderer)
  public
    procedure Render; override;
  end;

{ Text ----------------------------------------------------------------------- }

procedure TAbstractTextRenderer.RenderText(
  const Size, Spacing: Single;
  const Justify: TX3DFontJustify;
  const Strings: TCastleStringList;
  CurrentFont: TGLOutlineFont;
  const Depth: Single;
  const CullBackFaces: boolean;
  const EventLineBounds, EventOrigin, EventTextBounds: TX3DEvent);

var
  TextWidths: TSingleList;

  { StringPos* ignore Size, just assume that CurrentFont has right size. }
  function StringPosX(I: Integer): TGLfloat;
  begin
    case Justify of
      fjBegin : Result := 0;
      fjMiddle: Result := - TextWidths.Items[I] / 2;
      fjEnd   : Result := - TextWidths.Items[I];
      else raise EInternalError.Create('Invalid font justify value');
    end;
  end;

  function StringPosY(I: Integer): TGLfloat;
  begin
    Result := - I * CurrentFont.RowHeight * Spacing;
  end;

var
  YScale, XScale: TGLfloat;
  MaxTextWidth: Single;

  procedure SendEvents;
  var
    LineBounds: TMFVec2f;
    Time: TX3DTime;
    FinalRowHeight: Single;
    I: Integer;
  begin
    if (EventOrigin <> nil) and
       (Geometry.Scene <> nil) then
    begin
      { This is all simple, since we ignore topToBottom, leftToRight.
        Also, we don't honour the rule that the upper line of the text
        is exactly on Y = 0 (instead, our *row* (which is slightly higher
        than the upper text line) is on Y = 0, this is documented
        on [http://castle-engine.sourceforge.net/x3d_implementation_status.php]
        by failed NIST test about it.

        So the lacks of current Text rendering implementation
        make this somewhat simple :) }

      Time := Geometry.Scene.GetTime;

      FinalRowHeight := CurrentFont.RowHeight * Spacing * YScale;

      case Justify of
        fjBegin : EventOrigin.Send(Vector3Single(0                         , -FinalRowHeight, 0), Time);
        fjMiddle: EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale / 2, -FinalRowHeight, 0), Time);
        fjEnd   : EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale    , -FinalRowHeight, 0), Time);
        else raise EInternalError.Create('Invalid font justify value');
      end;

      if EventLineBounds.SendNeeded then
      begin
        LineBounds := TMFVec2f.CreateUndefined(Geometry, EventLineBounds.Name, false);
        try
          LineBounds.Items.Count := TextWidths.Count;

          case Justify of
            fjBegin :
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.L[I] := Vector2Single(
                  TextWidths[I] * XScale, FinalRowHeight);
            fjMiddle:
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.L[I] := Vector2Single(
                  (MaxTextWidth + TextWidths[I]) * XScale / 2, FinalRowHeight);
            fjEnd   :
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.L[I] := Vector2Single(
                  MaxTextWidth * XScale, FinalRowHeight);
            else raise EInternalError.Create('Invalid font justify value');
          end;

          EventLineBounds.Send(LineBounds, Time);
        finally FreeAndNil(LineBounds) end;
      end;

      EventTextBounds.Send(Vector2Single(
        MaxTextWidth * XScale, FinalRowHeight * Strings.Count), Time);
    end;
  end;

var
  I: Integer;
begin
  if PrepareRenderShape <> 0 then Exit;

  { Make sure CurrentProgram is correct }
  CurrentProgram := nil;

  YScale := Size / CurrentFont.RowHeight;
  { TODO: Use maxEntent, length for VRML 2.0.
    Use width for VRML 1.0. }
  XScale := YScale;

  TextWidths := TSingleList.Create;
  try
    TextWidths.Count := Strings.Count;
    MaxTextWidth := 0;
    for I := 0 to TextWidths.Count - 1 do
    begin
      TextWidths.L[I] := CurrentFont.TextWidth(Strings[I]);
      MaxTo1st(MaxTextWidth, TextWidths.L[I]);
    end;

    glPushMatrix;
      glScalef(1, YScale, 1);

      { Make sure Renderer.CullFace is suitable }
      if CullBackFaces then
        { Outside face is CW now, so cull CCW. }
        Renderer.CullFace := cfCCW else
        Renderer.CullFace := cfNone;

      { Make sure Renderer.SmoothShading is suitable }
      if Renderer.Attributes.Mode = rmFull then
        Renderer.SmoothShading := true;
      { TODO: for rmDepth, set SmoothShading := false always }

      { Normal pointing from CCW. }
      glNormal3f(0, 0, -1);

      for I := 0 to Strings.Count - 1 do
      begin
        glPushMatrix;
          glScalef(XScale, 1, 1);

          { TODO: when using Justify <> fjBegin I should also
            change texOriginX here, at least for VRML 1.0 --- check
            for VRML 2.0. }
          glTranslatef(StringPosX(I), StringPosY(I), 0);

          { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
            popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
          if Renderer.TexCoordsNeeded > 0 then
            CurrentFont.PrintTexturedAndMove(Strings[i], 0, 0) else
            CurrentFont.PrintAndMove(Strings[i]);
        glPopMatrix;
      end;

      if Depth <> 0 then
      begin
        { Make sure Renderer.CullFace is suitable }
        if CullBackFaces then
          Renderer.CullFace := cfCW else
          Renderer.CullFace := cfNone;

        { Normal pointing from CCW. }
        glNormal3f(0, 0, -1);

        for I := 0 to Strings.Count - 1 do
        begin
          glPushMatrix;
            glScalef(XScale, 1, 1);

            { TODO: when using Justify <> fjBegin I should also
              change texOriginX here, at least for VRML 1.0 --- check
              for VRML 2.0. }
            glTranslatef(StringPosX(I), StringPosY(I), -Depth);

            { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
              popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
            if Renderer.TexCoordsNeeded > 0 then
              CurrentFont.PrintTexturedAndMove(Strings[i], 0, 0) else
              CurrentFont.PrintAndMove(Strings[i]);
          glPopMatrix;
        end;

        { Normals will change during rendering of character extrusions,
          that's why I render extrusions after rendering all front and back caps.
          For now, normals for extrusions are suitable only for flat shading,
          so we set flat shading. }
        if Renderer.Attributes.Mode = rmFull then
          Renderer.SmoothShading := false;

        for I := 0 to Strings.Count - 1 do
        begin
          glPushMatrix;
            glScalef(XScale, 1, 1);

            { TODO: when using Justify <> fjBegin I should also
              change texOriginX here, at least for VRML 1.0 --- check
              for VRML 2.0. }
            glTranslatef(StringPosX(I), StringPosY(I), -Depth);

            { uzywamy ponizej print*AndMove bo przeciez za chwile i tak zrobimy
              popMatrix (wiec po co marnowac jedno miejsce na matrix stack ?) }
            if Renderer.TexCoordsNeeded > 0 then
              CurrentFont.PrintTexturedExtrusionAndMove(Strings[i], Depth, 0, 0) else
              CurrentFont.PrintExtrusionAndMove(Strings[i], Depth);
          glPopMatrix;
        end;
      end;

    glPopMatrix;

    { These events should be generated only when
      the default values of length and maxExtent are used.
      For now, we ignore length and maxExtent, so these events are
      simply always generated. }
    SendEvents;

  finally FreeAndNil(TextWidths) end;
end;

procedure TAsciiTextRenderer.Render;
var
  Node: TAsciiTextNode_1;
begin
  Node := Geometry as TAsciiTextNode_1;

  RenderText(
    State.LastNodes.FontStyle.FdSize.Value,
    Node.FdSpacing.Value,
    Node.Justify,
    Node.FdString.Items,
    Renderer.Cache.Fonts[
      State.LastNodes.FontStyle.Family,
      State.LastNodes.FontStyle.Bold,
      State.LastNodes.FontStyle.Italic].Instance, 0, false,
    nil, nil, nil);
end;

procedure TTextRenderer.Render;
var
  Node: TTextNode;
  Size, Spacing: Single;
  Justify: TX3DFontJustify;
  CurrentFont: TGLOutlineFont;
begin
  Node := Geometry as TTextNode;

  if Node.FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    CurrentFont := Renderer.Cache.Fonts[
      TFontStyleNode.DefaultFamily,
      TFontStyleNode.DefaultBold,
      TFontStyleNode.DefaultItalic].Instance;
  end else
  begin
    Size := Node.FontStyle.FdSize.Value;
    Spacing := Node.FontStyle.FdSpacing.Value;
    Justify := Node.FontStyle.Justify;
    CurrentFont := Renderer.Cache.Fonts[
      Node.FontStyle.Family,
      Node.FontStyle.Bold,
      Node.FontStyle.Italic].Instance;
  end;

  { TODO: this also has "solid" field, use! }

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    0, false,
    Node.EventLineBounds, Node.EventOrigin, Node.EventTextBounds);
end;

procedure TText3DRenderer.Render;
var
  Size, Spacing: Single;
  Justify: TX3DFontJustify;
  CurrentFont: TGLOutlineFont;
  Node: TText3DNode;
begin
  Node := Geometry as TText3DNode;

  if Node.FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    CurrentFont := Renderer.Cache.Fonts[
      TFontStyleNode.DefaultFamily,
      TFontStyleNode.DefaultBold,
      TFontStyleNode.DefaultItalic].Instance;
  end else
  begin
    Size := Node.FontStyle.FdSize.Value;
    Spacing := Node.FontStyle.FdSpacing.Value;
    Justify := Node.FontStyle.Justify;
    CurrentFont := Renderer.Cache.Fonts[
      Node.FontStyle.Family,
      Node.FontStyle.Bold,
      Node.FontStyle.Italic].Instance;
  end;

  RenderText(Size, Spacing, Justify, Node.FdString.Items, CurrentFont,
    Node.FdDepth.Value, Node.FdSolid.Value,
    nil, nil, nil);
end;
