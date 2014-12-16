{
  Copyright 2002-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TAbstractFontStyleNode = class(TAbstractNode)
  public
    procedure CreateNode; override;
  end;

  { Font family that can be specified by FontStyle node in family
    field. First three fields are equal (after casting by Ord) to
    three values of FSFAMILY_* constants. }
  TX3DFontFamily = (ffSerif, ffSans, ffTypeWriter);

  { Font justification that can be specified by FontStyle in
    justify/justification field. First three fields are equal
    (after casting by Ord) to JUSTIFICATION_* constants. }
  TX3DFontJustify = (fjBegin, fjMiddle, fjEnd);

  TFontStyleNode = class(TAbstractFontStyleNode)
  private
    JustifyWarningUppercaseDone, JustifyWarningObsoleteDone,
      JustifyWarningNotSupportedDone: boolean;
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFamily: TMFString;
    public property FdFamily: TMFString read FFdFamily;

    private FFdHorizontal: TSFBool;
    public property FdHorizontal: TSFBool read FFdHorizontal;

    private FFdJustify: TMFString;
    public property FdJustify: TMFString read FFdJustify;

    private FFdLanguage: TSFString;
    public property FdLanguage: TSFString read FFdLanguage;

    private FFdLeftToRight: TSFBool;
    public property FdLeftToRight: TSFBool read FFdLeftToRight;

    private FFdSize: TSFFloat;
    public property FdSize: TSFFloat read FFdSize;

    private FFdSpacing: TSFFloat;
    public property FdSpacing: TSFFloat read FFdSpacing;

    private FFdStyle: TSFString;
    public property FdStyle: TSFString read FFdStyle;

    private FFdTopToBottom: TSFBool;
    public property FdTopToBottom: TSFBool read FFdTopToBottom;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    function Family: TX3DFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function Justify: TX3DFontJustify;

    class function DefaultSize: Single;
    class function DefaultSpacing: Single;
    class function DefaultFamily: TX3DFontFamily;
    class function DefaultBold: boolean;
    class function DefaultItalic: boolean;
    class function DefaultJustify: TX3DFontJustify;
  end;
  TFontStyleNode_2 = TFontStyleNode;

  TPixelTextureNode = class;

  TTextNode = class(TAbstractX3DGeometryNode)
  strict private
    FFontTextureNode: TPixelTextureNode;
    function Font: TTextureFontData;
  public
    procedure CreateNode; override;
    destructor Destroy; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdFontStyle: TSFNode;
    public property FdFontStyle: TSFNode read FFdFontStyle;

    private FFdLength: TMFFloat;
    public property FdLength: TMFFloat read FFdLength;

    private FFdMaxExtent: TSFFloat;
    public property FdMaxExtent: TSFFloat read FFdMaxExtent;

    private FFdString: TMFString;
    public property FdString: TMFString read FFdString;

    { Event out } { }
    private FEventLineBounds: TMFVec2fEvent;
    public property EventLineBounds: TMFVec2fEvent read FEventLineBounds;

    { Event out } { }
    private FEventOrigin: TSFVec3fEvent;
    public property EventOrigin: TSFVec3fEvent read FEventOrigin;

    { Event out } { }
    private FEventTextBounds: TSFVec2fEvent;
    public property EventTextBounds: TSFVec2fEvent read FEventTextBounds;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function Proxy(var State: TX3DGraphTraverseState;
      const OverTriangulate: boolean): TAbstractGeometryNode; override;
    function ProxyUsesOverTriangulate: boolean; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TFontStyleNode. }
    function FontStyle: TFontStyleNode;
    function SolidField: TSFBool; override;
    function FontTextureNode: TAbstractTexture2DNode; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TAbstractFontStyleNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'fontStyle';
end;

procedure TFontStyleNode.CreateNode;
begin
  inherited;

  FFdFamily := TMFString.Create(Self, 'family', ['SERIF']);
   FdFamily.Exposed := false;
   FdFamily.ChangesAlways := [chFontStyle];
  Fields.Add(FFdFamily);

  FFdHorizontal := TSFBool.Create(Self, 'horizontal', true);
   FdHorizontal.Exposed := false;
   FdHorizontal.ChangesAlways := [chFontStyle];
  Fields.Add(FFdHorizontal);

  FFdJustify := TMFString.Create(Self, 'justify', ['BEGIN']);
   FdJustify.Exposed := false;
   FdJustify.ChangesAlways := [chFontStyle];
  Fields.Add(FFdJustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdLanguage := TSFString.Create(Self, 'language', '');
   FdLanguage.Exposed := false;
   FdLanguage.ChangesAlways := [chFontStyle];
  Fields.Add(FFdLanguage);

  FFdLeftToRight := TSFBool.Create(Self, 'leftToRight', true);
   FdLeftToRight.Exposed := false;
   FdLeftToRight.ChangesAlways := [chFontStyle];
  Fields.Add(FFdLeftToRight);

  FFdSize := TSFFloat.Create(Self, 'size', DefaultSize);
   FdSize.Exposed := false;
   FdSize.ChangesAlways := [chFontStyle];
  Fields.Add(FFdSize);
  { X3D specification comment: (0,Inf) }

  FFdSpacing := TSFFloat.Create(Self, 'spacing', DefaultSpacing);
   FdSpacing.Exposed := false;
   FdSpacing.ChangesAlways := [chFontStyle];
  Fields.Add(FFdSpacing);
  { X3D specification comment: [0,Inf) }

  FFdStyle := TSFString.Create(Self, 'style', 'PLAIN');
   FdStyle.Exposed := false;
   FdStyle.ChangesAlways := [chFontStyle];
  Fields.Add(FFdStyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdTopToBottom := TSFBool.Create(Self, 'topToBottom', true);
   FdTopToBottom.Exposed := false;
   FdTopToBottom.ChangesAlways := [chFontStyle];
  Fields.Add(FFdTopToBottom);
end;

class function TFontStyleNode.ClassNodeTypeName: string;
begin
  Result := 'FontStyle';
end;

class function TFontStyleNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TFontStyleNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TFontStyleNode.Family: TX3DFontFamily;
var
  I: Integer;
begin
  for I := 0 to FdFamily.Items.Count - 1 do
    if FdFamily.Items[I] = 'SERIF' then
      Exit(ffSerif) else
    if FdFamily.Items[I] = 'SANS' then
      Exit(ffSans) else
    if FdFamily.Items[I] = 'TYPEWRITER' then
      Exit(ffTypeWriter) else
      OnWarning(wtMajor, 'VRML/X3D', 'Font family "' + FdFamily.Items[I] + '" not supported');

  { If no supported values on FdFamily.Items then fall back to serif }
  Result := ffSerif;
end;

const
  StyleBold = 'BOLD';
  StyleBoldItalic = 'BOLDITALIC';
  StyleItalic = 'ITALIC';
  StylePlain = 'PLAIN';

function TFontStyleNode.Bold: boolean;
begin
  Result :=
    (FdStyle.Value = StyleBold) or
    (FdStyle.Value = StyleBoldItalic);

  { This is the end of calculating Result.
    But we would like to make a warning in case of invalid FdStyle
    value, so we do check below. }

  if not Result then
  begin
    if not (
      (FdStyle.Value = StyleItalic) or
      (FdStyle.Value = StylePlain) or
      (FdStyle.Value = '')) then
      OnWarning(wtMajor, 'VRML/X3D', 'Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TFontStyleNode.Italic: boolean;
begin
  Result :=
    (FdStyle.Value = StyleItalic) or
    (FdStyle.Value = StyleBoldItalic);

  { This is the end of calculating Result.
    But we would like to make a warning in case of invalid FdStyle
    value, so we do check below. }

  if not Result then
  begin
    if not (
      (FdStyle.Value = StyleBold) or
      (FdStyle.Value = StylePlain) or
      (FdStyle.Value = '')) then
      OnWarning(wtMajor, 'VRML/X3D', 'Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TFontStyleNode.Justify: TX3DFontJustify;
const
  SJustifyObsolete = 'Font justify "%s" should not be used in VRML >= 2.0, use "%s" instead';
var
  J: string;
begin
  if FdJustify.Items.Count = 0 then
    Result := fjBegin else
  begin
    { Some X3D models use lowercase names, like [http://instant-reality.com/]
      test models. }
    J := UpperCase(FdJustify.Items[0]);
    if (J <> FdJustify.Items[0]) and not JustifyWarningUppercaseDone then
    begin
      OnWarning(wtMajor, 'VRML/X3D', Format('Font justify value "%s" should be specified in uppercase',
        [ FdJustify.Items[0] ]));
      JustifyWarningUppercaseDone := true;
    end;

    if (J = 'BEGIN') or
       (J = 'FIRST') then
      Result := fjBegin else
    if J = 'MIDDLE' then
      Result := fjMiddle else
    if J = 'END' then
      Result := fjEnd else

    { Some X3D models use old justify names, like cic.nist.gov X3D demos. }
    if J = 'LEFT' then
    begin
      if not JustifyWarningObsoleteDone then
      begin
        OnWarning(wtMajor, 'VRML/X3D', Format(SJustifyObsolete, ['LEFT', 'BEGIN']));
        JustifyWarningObsoleteDone := true;
      end;
      Result := fjBegin;
    end else
    if J = 'CENTER' then
    begin
      if not JustifyWarningObsoleteDone then
      begin
        OnWarning(wtMajor, 'VRML/X3D', Format(SJustifyObsolete, ['CENTER', 'MIDDLE']));
        JustifyWarningObsoleteDone := true;
      end;
      Result := fjMiddle;
    end else
    if J = 'RIGHT' then
    begin
      if not JustifyWarningObsoleteDone then
      begin
        OnWarning(wtMajor, 'VRML/X3D', Format(SJustifyObsolete, ['RIGHT', 'END']));
        JustifyWarningObsoleteDone := true;
      end;
      Result := fjEnd;
    end else

    begin
      Result := fjBegin;
      if not JustifyWarningNotSupportedDone then
      begin
        OnWarning(wtMajor, 'VRML/X3D', 'Font justify "' + FdJustify.Items[0] + '" not supported');
        JustifyWarningNotSupportedDone := true;
      end;
    end;
  end;
end;

class function TFontStyleNode.DefaultSize: Single;
begin
  Result := 1;
end;

class function TFontStyleNode.DefaultSpacing: Single;
begin
  Result := 1;
end;

class function TFontStyleNode.DefaultFamily: TX3DFontFamily;
begin
  Result := ffSerif;
end;

class function TFontStyleNode.DefaultBold: boolean;
begin
  Result := false;
end;

class function TFontStyleNode.DefaultItalic: boolean;
begin
  Result := false;
end;

class function TFontStyleNode.DefaultJustify: TX3DFontJustify;
begin
  Result := fjBegin;
end;

procedure TTextNode.CreateNode;
begin
  inherited;

  FFdFontStyle := TSFNode.Create(Self, 'fontStyle', [TAbstractFontStyleNode]);
   FdFontStyle.ChangesAlways := [chGeometry];
  Fields.Add(FFdFontStyle);

  FFdLength := TMFFloat.Create(Self, 'length', []);
   FdLength.ChangesAlways := [chGeometry];
  Fields.Add(FFdLength);
  { X3D specification comment: [0,Inf) }

  FFdMaxExtent := TSFFloat.Create(Self, 'maxExtent', 0.0);
   FdMaxExtent.ChangesAlways := [chGeometry];
  Fields.Add(FFdMaxExtent);
  { X3D specification comment: [0,Inf) }

  FFdString := TMFString.Create(Self, 'string', []);
   FdString.ChangesAlways := [chGeometry];
  Fields.Add(FFdString);

  FEventLineBounds := TMFVec2fEvent.Create(Self, 'lineBounds', false);
  Events.Add(FEventLineBounds);

  FEventOrigin := TSFVec3fEvent.Create(Self, 'origin', false);
  Events.Add(FEventOrigin);

  FEventTextBounds := TSFVec2fEvent.Create(Self, 'textBounds', false);
  Events.Add(FEventTextBounds);

  FFdSolid := TSFBool.Create(Self, 'solid', false);
   FdSolid.Exposed := false;
   FdSolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdSolid);
end;

destructor TTextNode.Destroy;
begin
  { actually FreeAndNil would also be OK, as this is never
    a child of another node. But for safety... }
  FreeIfUnusedAndNil(FFontTextureNode);
  inherited;
end;

class function TTextNode.ClassNodeTypeName: string;
begin
  Result := 'Text';
end;

class function TTextNode.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TTextNode.FontStyle: TFontStyleNode;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TFontStyleNode) then
    Result := TFontStyleNode(FdFontStyle.Value) else
    Result := nil;
end;

function TTextNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TextProxy(const OriginalNode: TX3DNode;
  const Size, Spacing: Single;
  const Justify: TX3DFontJustify;
  const Strings: TCastleStringList;
  const Font: TTextureFontData;
  const Solid: boolean;
  const EventLineBounds: TMFVec2fEvent;
  const EventOrigin: TSFVec3fEvent;
  const EventTextBounds: TSFVec2fEvent): TQuadSetNode;
var
  Coord: TCoordinateNode;
  TexCoordNode: TTextureCoordinateNode;
  TextWidths: TSingleList;

  { StringPos* calculate position to start the given line of text.
    They ignore Size parameter, just assume that Font has right size. }
  function StringPosX(I: Integer): Single;
  begin
    case Justify of
      fjBegin : Result := 0;
      fjMiddle: Result := - TextWidths.Items[I] / 2;
      fjEnd   : Result := - TextWidths.Items[I];
      else raise EInternalError.Create('Invalid font justify value');
    end;
  end;

  function StringPosY(I: Integer): Single;
  begin
    Result := - I * Font.RowHeight * Spacing;
  end;

var
  YScale, XScale, MaxTextWidth: Single;

  procedure SendEvents;
  var
    LineBounds: TMFVec2f;
    Time: TX3DTime;
    FinalRowHeight: Single;
    I: Integer;
  begin
    if (EventOrigin <> nil) and
       (OriginalNode.Scene <> nil) then
    begin
      { This is all simple, since we ignore topToBottom, leftToRight.
        Also, we don't honour the rule that the upper line of the text
        is exactly on Y = 0 (instead, our *row* (which is slightly higher
        than the upper text line) is on Y = 0, this is documented
        on [http://castle-engine.sourceforge.net/x3d_implementation_status.php]
        by failed NIST test about it.

        So the lacks of current Text rendering implementation
        make this somewhat simple :) }

      Time := OriginalNode.Scene.GetTime;

      FinalRowHeight := Font.RowHeight * Spacing * YScale;

      case Justify of
        fjBegin : EventOrigin.Send(Vector3Single(0                         , -FinalRowHeight, 0), Time);
        fjMiddle: EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale / 2, -FinalRowHeight, 0), Time);
        fjEnd   : EventOrigin.Send(Vector3Single(-MaxTextWidth * XScale    , -FinalRowHeight, 0), Time);
        else raise EInternalError.Create('Invalid font justify value');
      end;

      if EventLineBounds.SendNeeded then
      begin
        LineBounds := TMFVec2f.CreateUndefined(OriginalNode, EventLineBounds.Name, false);
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

  { Print the string, by adding rectangles to current QuadSet.
    This is somewhat analogous to printing straight in 2D done
    by TCastleFont.Print and TGLImage.Draw. }
  procedure Print(X, Y: Single; const S: string; const XScale, YScale: Single);
  var
    C: TUnicodeChar;
    TextPtr: PChar;
    CharLen: Integer;
    G: TTextureFontData.TGlyph;
    CoordX0, CoordX1, CoordY0, CoordY1, TexX0, TexX1, TexY0, TexY1: Single;
  begin
    TextPtr := PChar(S);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    while (C > 0) and (CharLen > 0) do
    begin
      Inc(TextPtr, CharLen);

      G := Font.Glyph(C);
      if G <> nil then
      begin
        if (G.Width <> 0) and (G.Height <> 0) then
        begin
          { Assumes 1 pixel transparent border around every glyph
            (as it happens, TTextureFontData.Create creates such).
            Makes the letter shape more smooth,
            visibly better in the case of blending.

            TODO: needs 1 pixel border to the left / right too. }
          { $define EXTRA_GLYPH_SPACE}

          CoordX0 := X - G.X {$ifdef EXTRA_GLYPH_SPACE} - 1 {$endif};
          CoordY0 := Y - G.Y {$ifdef EXTRA_GLYPH_SPACE} - 1 {$endif};
          CoordX1 := CoordX0 + G.Width  {$ifdef EXTRA_GLYPH_SPACE} + 2 {$endif};
          CoordY1 := CoordY0 + G.Height {$ifdef EXTRA_GLYPH_SPACE} + 2 {$endif};

          CoordX0 *= XScale;
          CoordX1 *= XScale;
          CoordY0 *= YScale;
          CoordY1 *= YScale;

          Coord.FdPoint.Items.Add(Vector3Single(CoordX0, CoordY0, 0));
          Coord.FdPoint.Items.Add(Vector3Single(CoordX1, CoordY0, 0));
          Coord.FdPoint.Items.Add(Vector3Single(CoordX1, CoordY1, 0));
          Coord.FdPoint.Items.Add(Vector3Single(CoordX0, CoordY1, 0));

          TexX0 := (G.ImageX {$ifdef EXTRA_GLYPH_SPACE} - 1 {$endif}) / Font.Image.Width;
          TexY0 := (G.ImageY {$ifdef EXTRA_GLYPH_SPACE} - 1 {$endif}) / Font.Image.Height;
          TexX1 := (G.ImageX + G.Width  {$ifdef EXTRA_GLYPH_SPACE} + 2 {$endif}) / Font.Image.Width;
          TexY1 := (G.ImageY + G.Height {$ifdef EXTRA_GLYPH_SPACE} + 2 {$endif}) / Font.Image.Height;

          TexCoordNode.FdPoint.Items.Add(Vector2Single(TexX0, TexY0));
          TexCoordNode.FdPoint.Items.Add(Vector2Single(TexX1, TexY0));
          TexCoordNode.FdPoint.Items.Add(Vector2Single(TexX1, TexY1));
          TexCoordNode.FdPoint.Items.Add(Vector2Single(TexX0, TexY1));
        end;
        X += G.AdvanceX;
        Y += G.AdvanceY;
      end;

      C := UTF8CharacterToUnicode(TextPtr, CharLen);
    end;
  end;

var
  I: Integer;
begin
  Result := TQuadSetNode.Create(OriginalNode.NodeName, OriginalNode.BaseUrl);
  try
    { make Coordinate node }
    Coord := TCoordinateNode.Create('', OriginalNode.BaseUrl);
    Result.FdCoord.Value := Coord;

    TexCoordNode := TTextureCoordinateNode.Create('', OriginalNode.BaseUrl);
    Result.FdTexCoord.Value := TexCoordNode;
    Result.Solid := Solid;

    YScale := Size / Font.RowHeight;
    { TODO: Use maxEntent, length for VRML 2.0.
      Use width for VRML 1.0. }
    XScale := YScale;

    TextWidths := TSingleList.Create;
    try
      TextWidths.Count := Strings.Count;
      MaxTextWidth := 0;
      for I := 0 to TextWidths.Count - 1 do
      begin
        TextWidths.L[I] := Font.TextWidth(Strings[I]);
        MaxTo1st(MaxTextWidth, TextWidths.L[I]);
      end;

      for I := 0 to Strings.Count - 1 do
      begin
        { TODO: when using Justify <> fjBegin I should also
          change texOriginX here, at least for VRML 1.0 --- check
          for VRML 2.0. }
        Print(StringPosX(I), StringPosY(I), Strings[I], XScale, YScale);
      end;

      { These events should be generated only when
        the default values of length and maxExtent are used.
        For now, we ignore length and maxExtent, so these events are
        simply always generated. }
      SendEvents;
    finally FreeAndNil(TextWidths) end;
  except FreeAndNil(Result); raise end;
end;

function TTextNode.Proxy(var State: TX3DGraphTraverseState;
  const OverTriangulate: boolean): TAbstractGeometryNode;
var
  Size, Spacing: Single;
  Justify: TX3DFontJustify;
begin
  { TODO: recreate this when font parameters changed }
  if FontStyle = nil then
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
  end else
  begin
    Size := FontStyle.FdSize.Value;
    Spacing := FontStyle.FdSpacing.Value;
    Justify := FontStyle.Justify;
  end;

  { TODO: test "solid" field working }
  { TODO: allow Text node to have texCoord, for shadow maps etc.
    TODO: use the Text node texCoord as extra texCoord when generating proxy }

  Result := TextProxy(Self, Size, Spacing, Justify, FdString.Items, Font,
    Solid, EventLineBounds, EventOrigin, EventTextBounds);
end;

function TTextNode.ProxyUsesOverTriangulate: boolean;
begin
  Result := false;
end;

function TTextNode.Font: TTextureFontData;

  function GetFont(const Family: TX3DFontFamily; const Bold, Italic: boolean): TTextureFontData;
  begin
    case Family of
      ffSerif:
        if Bold and Italic then
          Result := TextureFont_DejaVuSerifBoldItalic_20 else
        if Bold then
          Result := TextureFont_DejaVuSerifBold_20 else
        if Italic then
          Result := TextureFont_DejaVuSerifItalic_20 else
          Result := TextureFont_DejaVuSerif_20;
      ffSans:
        if Bold and Italic then
          Result := TextureFont_DejaVuSansBoldOblique_20 else
        if Bold then
          Result := TextureFont_DejaVuSansBold_20 else
        if Italic then
          Result := TextureFont_DejaVuSansOblique_20 else
          Result := TextureFont_DejaVuSans_20;
      ffTypeWriter:
        if Bold and Italic then
          Result := TextureFont_DejaVuSansMonoBoldOblique_20 else
        if Bold then
          Result := TextureFont_DejaVuSansMonoBold_20 else
        if Italic then
          Result := TextureFont_DejaVuSansMonoOblique_20 else
          Result := TextureFont_DejaVuSansMono_20;
      else raise EInternalError.Create('GetFont:Family?');
    end;
  end;

begin
  if FontStyle <> nil then
    Result := GetFont(
      FontStyle.Family,
      FontStyle.Bold,
      FontStyle.Italic) else
    Result := GetFont(
      TFontStyleNode.DefaultFamily,
      TFontStyleNode.DefaultBold,
      TFontStyleNode.DefaultItalic);
end;

function TTextNode.FontTextureNode: TAbstractTexture2DNode;
begin
  { TODO: recreate this when font parameters changed }
  if FFontTextureNode = nil then
  begin
    FFontTextureNode := TPixelTextureNode.Create('', BaseUrl);
    FreeAndNil(FFontTextureNode.FdImage.Value);
    FFontTextureNode.FdRepeatS.Value := false;
    FFontTextureNode.FdRepeatT.Value := false;
    { convert grayscale image with TreatAsAlpha to TGrayscaleAlphaImage }
    FFontTextureNode.FdImage.Value := Font.Image.ToGrayscaleAlphaImage;
    { TODO: allow configuring this on Text node, whether we use blendinf or not. }
    //FFontTextureNode.FdAlphaChannel.Value := 'SIMPLE_YES_NO';
  end;
  Result := FFontTextureNode;
end;

procedure RegisterTextNodes;
begin
  NodesManager.RegisterNodeClasses([
    TFontStyleNode,
    TTextNode
  ]);
end;

{$endif read_implementation}
