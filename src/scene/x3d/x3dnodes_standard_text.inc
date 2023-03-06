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
  { Base node type for all font style nodes. }
  TAbstractFontStyleNode = class(TAbstractNode)
    {$I auto_generated_node_helpers/x3dnodes_x3dfontstylenode.inc}
  end;

  { Font family that can be specified by a TFontStyleNode. }
  TX3DFontFamily = (ffSerif, ffSans, ffTypeWriter);

  { Font justification that can be specified by a TFontStyleNode. }
  TX3DFontJustify = (fjFirst, fjBegin, fjMiddle, fjEnd);

  { Font style that can be specified by a @link(TFontStyleNode.Style). }
  TX3DFontStyle = (fsPlain, fsBold, fsItalic, fsBoldItalic);

  { Defines the size, family, style and other properties used for @link(TTextNode). }
  TFontStyleNode = class(TAbstractFontStyleNode)
  strict private
    JustifyWarningUppercaseDone, JustifyWarningObsoleteDone,
      JustifyWarningNotSupportedDone: boolean;
    FCustomFont: TCastleFont;
    FCustomFontObserver: TFreeNotificationObserver;
    function StringToJustify(const S: string;
      const DefaultValue: TX3DFontJustify): TX3DFontJustify;
    function GetFamily: TX3DFontFamily;
    procedure SetFamily(const Value: TX3DFontFamily);
    function GetBold: boolean;
    procedure SetBold(const Value: boolean);
    function GetItalic: boolean;
    procedure SetItalic(const Value: boolean);
    function GetJustify: TX3DFontJustify;
    procedure SetJustify(const Value: TX3DFontJustify);
    function GetJustifyMinor: TX3DFontJustify;
    procedure SetJustifyMinor(const Value: TX3DFontJustify);
    procedure SetCustomFont(const Value: TCastleFont);
    procedure CustomFontFreeNotification(const Sender: TFreeNotificationObserver);
  private
    class function GetDefaultFont(const Family: TX3DFontFamily; const Bold, Italic: Boolean): TCastleFont;
  public
    const
      DefaultSize = 1.0;
      DefaultSpacing = 1.0;
      DefaultFamily = ffSerif;
      DefaultBold = false;
      DefaultItalic = false;
      DefaultJustify = fjBegin;
      DefaultJustifyMinor = fjFirst;
      DefaultBlending = true;

    constructor Create(const AX3DName: string = ''; const ABaseUrl: string = ''); override;
    destructor Destroy; override;

    property Family: TX3DFontFamily read GetFamily write SetFamily;
    property Justify: TX3DFontJustify read GetJustify write SetJustify;
    property JustifyMinor: TX3DFontJustify read GetJustifyMinor write SetJustifyMinor;
    property Bold: boolean read GetBold write SetBold;
    property Italic: boolean read GetItalic write SetItalic;

    class function ForVRMLVersion(const Version: TX3DVersion): boolean;
      override;

    { Font used by this node, determined by our fields (like @link(Bold))
      and @link(TFontStyleNode.CustomFont). }
    function Font: TCastleFont;

    { Adjust the font.

      TODO: Currently this only allows TCastleFont.
      In the future it should allow any TCastleAbstractFont,
      like TCastleFontFamily (thus have different options for bold/italic variants)
      or TCastleBitmapFont. }
    property CustomFont: TCastleFont read FCustomFont write SetCustomFont;

    {$I auto_generated_node_helpers/x3dnodes_fontstyle.inc}
  end;
  TFontStyleNode_2 = TFontStyleNode deprecated 'use TFontStyleNode';

  { Multiline text, visualized as a set of flat and textured polygons. }
  TTextNode = class(TAbstractGeometryNode)
  strict private
    FFontTextureNode: TPixelTextureNode;
    function Font: TCastleFont;
  public
    destructor Destroy; override;
    function TexCoordField: TSFNode; override;

    function Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode; override;
    function LocalBoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;
    function BoundingBox(State: TX3DGraphTraverseState;
      ProxyGeometry: TAbstractGeometryNode; ProxyState: TX3DGraphTraverseState): TBox3D; override;

    function SolidField: TSFBool; override;
    function FontTextureNode: TAbstractTexture2DNode; override;

    { Force recalculating the shape and font texture.
      Call this when TFontStyleNode.CustomFont returns something different. }
    procedure FontChanged;

    {  }
    procedure SetString(const Value: array of string); overload; deprecated 'use SetText';
    {  }
    procedure SetString(const Value: TCastleStringList); overload; deprecated 'use SetText';

    {$I auto_generated_node_helpers/x3dnodes_text.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ internal routine to create proxy ------------------------------------------- }

{ Create a simple mesh (filling OutCoords, OutTexCoords fields
  to initialize X3D node like QuadSet) that represents input Text node.
  The interface is suitable for both VRML 2.0 / X3D Text node,
  as well as VRML 1.0 AsciiText node. }
procedure TextProxy(const OriginalNode: TX3DNode;
  const Size, Spacing: Single;
  const Justify, JustifyMinor: TX3DFontJustify;
  const Strings: TCastleStringList;
  const MaxDisplayChars: Integer;
  const Font: TCastleFont;
  const Solid: boolean;
  const EventLineBounds: TMFVec2fEvent;
  const EventOrigin: TSFVec3fEvent;
  const EventTextBounds: TSFVec2fEvent;
  const OutCoord: TMFVec3f; const OutTexCoord, OutExtraTexCoord: TMFVec2f);
var
  TextWidths: TSingleList;
  FontData: TTextureFontData;

  { StringPos* calculate position to start the given line of text.
    They ignore the Size parameter. }

  function StringPosX(I: Integer): Single;
  begin
    case Justify of
      fjBegin, fjFirst :
                Result := 0;
      fjMiddle: Result := - TextWidths.Items[I] / 2;
      fjEnd   : Result := - TextWidths.Items[I];
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('StringPosX: Invalid font Justify value');
      {$endif}
    end;
  end;

  function StringPosY(I: Integer): Single;
  begin
    case JustifyMinor of
      fjFirst : Result := - I;
      fjBegin : Result := - (I + 1);
      fjMiddle: Result := Strings.Count / 2 - (I + 1);
      fjEnd   : Result := Strings.Count     - (I + 1);
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('StringPosY: Invalid font JustifyMinor value');
      {$endif}
    end;
    Result := Result * (Font.Height * Spacing);

    { For JustifyMinor <> fjFirst add font descend,
      to account for the fact that baseline is a bit higher than 0.

      This means that (for JustifyMinor <> fjFirst) vertical level = 0 is not "baseline".
      Vertical level = 0 is "where bounding box starts". This has advantages:

      - It is similar behavior to TCastleLabel. So user has consistent behavior,
        and you can even easily match TCastleLabel coordinates with TCastleText coordinates
        in the viewport.
      - It is more comfortable when you want to fit the text somewhere.
        No text geometry goes underneath vertical level = 0, ever.
      - This makes the fjBegin and fjEnd behave consistently,
        and for both horizontal and vertical cases.
        They all mean "this is the edge of the bounding box".

      In contrast, fjFirst logic is a bit special.
      It means that vertical level = 0 is "baseline".
      This makes the bottom of the font special:

      - Font may have some geometry below 0.
      - This is unlike the horizontal fjFirst (that determines just "left edge of bbox"
        and is equal to fjBegin).

      Of course fjFirst (vertical level = 0 is "baseline") has one big advantage:
      making larger/smaller Descend of the font doesn't shift where the baseline is rendered.

      It matches Text_LocalBoundingBox logic, that also accounts for
      JustifyMinor=fjFirst as a special case. }
    if JustifyMinor <> fjFirst then
      Result := Result + Font.DescenderHeight * Spacing;
  end;

var
  YScale, XScale, MaxTextWidth: Single;
  ExtraTexOrigin: TVector2;

  procedure SendEvents;
  var
    Origin: TVector3;
    LineBounds: TMFVec2f;
    FinalHeight: Single;
    I: Integer;
  begin
    if (EventOrigin <> nil) and
       (OriginalNode.Scene <> nil) then
    begin
      { TODO: We ignore topToBottom, leftToRight. }

      FinalHeight := Font.Height * Spacing * YScale;

      { calculate and send Origin }
      Origin := TVector3.Zero;
      case Justify of
        fjBegin, fjFirst :
                  Origin.X := 0;
        fjMiddle: Origin.X := -MaxTextWidth * XScale / 2;
        fjEnd   : Origin.X := -MaxTextWidth * XScale    ;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('SendEvents: Invalid font justify value');
        {$endif}
      end;
      case JustifyMinor of
        fjFirst : Origin.Y := -FinalHeight;
        fjBegin : Origin.Y := 0;
        fjMiddle: Origin.Y := -FinalHeight * Strings.Count / 2;
        fjEnd   : Origin.Y := -FinalHeight * Strings.Count;
        {$ifndef COMPILER_CASE_ANALYSIS}
        else raise EInternalError.Create('SendEvents: Invalid font justify value');
        {$endif}
      end;
      EventOrigin.Send(Origin, OriginalNode.Scene.NextEventTime);

      if EventLineBounds.SendNeeded then
      begin
        LineBounds := TMFVec2f.CreateUndefined(OriginalNode, false, EventLineBounds.X3DName);
        try
          LineBounds.Items.Count := TextWidths.Count;

          case Justify of
            fjBegin, fjFirst:
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.List^[I] := Vector2(
                  TextWidths[I] * XScale, FinalHeight);
            fjMiddle:
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.List^[I] := Vector2(
                  (MaxTextWidth + TextWidths[I]) * XScale / 2, FinalHeight);
            fjEnd   :
              for I := 0 to TextWidths.Count - 1 do
                LineBounds.Items.List^[I] := Vector2(
                  MaxTextWidth * XScale, FinalHeight);
            {$ifndef COMPILER_CASE_ANALYSIS}
            else raise EInternalError.Create('Invalid font justify value');
            {$endif}
          end;

          EventLineBounds.Send(LineBounds, OriginalNode.Scene.NextEventTime);
        finally FreeAndNil(LineBounds) end;
      end;

      EventTextBounds.Send(Vector2(
        MaxTextWidth * XScale, FinalHeight * Strings.Count), OriginalNode.Scene.NextEventTime);
    end;
  end;

  { Print the string, by adding rectangles to current QuadSet.
    This is analogous to printing straight in 2D done by TCastleFont.Print.
    Some pieces of code below are deliberately similar, and we prepare to make outlines
    too. }
  procedure Print(const X, Y: Single; const S: string; const XScale, YScale: Single);

    function ConvertCoordToExtraTex(const CoordIndex: Integer; const Coord: Single): Single;
    begin
      Result := MapRange(Coord,
        ExtraTexOrigin[CoordIndex],
        ExtraTexOrigin[CoordIndex] + Font.Height * Spacing * YScale,
        0, 1);
    end;

  // TODO: Outline not done yet.
  // Outline requires a different X3D shape for different color, or using ColorRGBA.
  const
    Outline = 0;
    OutlineHighQuality = true;
  var
    ScreenX, ScreenY: Single;
    G: TTextureFontData.TGlyph;

    procedure GlyphDraw(const OutlineMoveX, OutlineMoveY: Integer);
    var
      ScreenRect, ImageRect, ExtraTexRect: TFloatRectangle;
    begin
      { Use a small margin around every glyph to allow bilinear
        filtering to smoothly go from opaque to fully transparent
        at glyph border. This prevents glyph border from ending suddenly,
        it looks much better in the case of blending.

        This cooperates with TTextureFontData.Create (used by
        texture-font-to-pascal) that makes sure that each letter is
        surrounded with a padding that allows such border,
        see GlyphPadding in castletexturefontdata.pas. }
      {$define EXTRA_GLYPH_SPACE}

      ScreenRect := FloatRectangle(
        XScale * (ScreenX - G.X + OutlineMoveX * Outline {$ifdef EXTRA_GLYPH_SPACE} - 0.5 {$endif}),
        YScale * (ScreenY - G.Y + OutlineMoveY * Outline {$ifdef EXTRA_GLYPH_SPACE} - 0.5 {$endif}),
        XScale * (G.Width  {$ifdef EXTRA_GLYPH_SPACE} + 1 {$endif}),
        YScale * (G.Height {$ifdef EXTRA_GLYPH_SPACE} + 1 {$endif}));

      ImageRect := FloatRectangle(
        (G.ImageX {$ifdef EXTRA_GLYPH_SPACE} - 0.5 {$endif}) / FontData.Image.Width,
        (G.ImageY {$ifdef EXTRA_GLYPH_SPACE} - 0.5 {$endif}) / FontData.Image.Height,
        (G.Width  {$ifdef EXTRA_GLYPH_SPACE} + 1 {$endif}) / FontData.Image.Width,
        (G.Height {$ifdef EXTRA_GLYPH_SPACE} + 1 {$endif}) / FontData.Image.Height);

      ScreenRect.AddToCoords(OutCoord.Items, 0);
      ImageRect.AddToCoords(OutTexCoord.Items);

      { if we want extra texture coordinates, generate them knowing
        ExtraTexOrigin and current Coord* values. This is enough,
        see the specification:

          The texture origin is at the origin of the first string,
          as determined by the justification. The texture is scaled
          equally in both S and T dimensions, with the font height
          representing 1 unit. S increases to the right, and T increases up. }

      if OutExtraTexCoord <> nil then
      begin
        ExtraTexRect := FloatRectangle(
          ConvertCoordToExtraTex(0, ScreenRect.Left  ),
          ConvertCoordToExtraTex(1, ScreenRect.Bottom),
          ConvertCoordToExtraTex(0, ScreenRect.Right ) - ConvertCoordToExtraTex(0, ScreenRect.Left),
          ConvertCoordToExtraTex(1, ScreenRect.Top   ) - ConvertCoordToExtraTex(1, ScreenRect.Bottom)
        );
        ExtraTexRect.AddToCoords(OutExtraTexCoord.Items);
      end;
    end;

  var
    C: TUnicodeChar;
    {$ifdef FPC}
    TextPtr: PChar;
    CharLen: Integer;
    {$else}
    TextIndex: Integer;
    NextTextIndex: Integer;
    TextLength: Integer;
    {$endif}
  begin
    { first pass, to render Outline. }

    // TODO: Outline not done yet.
    // Outline requires a different X3D shape for different color, or using ColorRGBA.
    (*
    if Outline <> 0 then
    begin
      ScreenX := X;
      ScreenY := Y;

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
            GlyphDraw(0, 0);
            GlyphDraw(0, 2);
            GlyphDraw(2, 2);
            GlyphDraw(2, 0);

            if OutlineHighQuality then
            begin
              GlyphDraw(1, 0);
              GlyphDraw(1, 2);
              GlyphDraw(0, 1);
              GlyphDraw(2, 1);
            end;
          end;
          ScreenX += G.AdvanceX + Outline * 2;
          ScreenY += G.AdvanceY;
        end;

        C := UTF8CharacterToUnicode(TextPtr, CharLen);
      end;
    end;
    *)

    ScreenX := X;
    ScreenY := Y;

    {$ifdef FPC}
    TextPtr := PChar(S);
    C := UTF8CharacterToUnicode(TextPtr, CharLen);
    while (C > 0) and (CharLen > 0) do
    {$else}
    TextIndex := 1;
    TextLength := Length(S);
    while (TextIndex <= TextLength) do
    {$endif}
    begin
      {$ifdef FPC}
      Inc(TextPtr, CharLen);
      {$else}
      C := GetUTF32Char(S, TextIndex, NextTextIndex);
      TextIndex := NextTextIndex;
      {$endif}

      G := FontData.Glyph(C);
      if G <> nil then
      begin
        if (G.Width <> 0) and (G.Height <> 0) then
          // TODO: Outline not done yet.
          // Outline requires a different X3D shape for different color, or using ColorRGBA.
          (*
          if Outline <> 0 then
            GlyphDraw(1, 1)
          else
          *)
            GlyphDraw(0, 0);
        ScreenX := ScreenX + G.AdvanceX + Outline * 2;
        ScreenY := ScreenY + G.AdvanceY;
      end;

      {$ifdef FPC}
      C := UTF8CharacterToUnicode(TextPtr, CharLen);
      {$endif}
    end;
  end;

var
  I: Integer;
  DisplayChars: Integer;
begin
  {$warnings off} // using FontData, will be internal in the future
  FontData := Font.FontData;
  {$warnings on}
  if FontData = nil then
  begin
    WritelnWarning('Cannot render font that is not loaded (has FontData = nil)');
    Exit;
  end;

  YScale := Size / Font.Height;
  { TODO: Use maxEntent, length for VRML 2.0. Use width for VRML 1.0. }
  XScale := YScale;

  TextWidths := TSingleList.Create;
  try
    TextWidths.Count := Strings.Count;
    MaxTextWidth := 0;
    for I := 0 to TextWidths.Count - 1 do
    begin
      TextWidths.List^[I] := Font.TextWidth(Strings[I]);
      MaxVar(MaxTextWidth, TextWidths.List^[I]);
    end;

    if Strings.Count <> 0 then
    begin
      ExtraTexOrigin := Vector2(StringPosX(0) * XScale, StringPosY(0) * YScale);
      if MaxDisplayChars < 0 then
        for I := 0 to Strings.Count - 1 do
          Print(StringPosX(I), StringPosY(I), Strings[I], XScale, YScale) //optimize in case MaxDisplayChars = -1
      else
      begin
        DisplayChars := MaxDisplayChars;
        for I := 0 to Strings.Count - 1 do
        begin
          if {$ifdef FPC}UTF8Length{$else}GetUTF32Length{$endif}(Strings[I]) < DisplayChars then
            Print(StringPosX(I), StringPosY(I), Strings[I], XScale, YScale)
          else
            Print(StringPosX(I), StringPosY(I),
              {$ifdef FPC}UTF8Copy{$else}UTF32Copy{$endif}(Strings[I],
              1, DisplayChars), XScale, YScale);
          Dec(DisplayChars,
            {$ifdef FPC}UTF8Length{$else}GetUTF32Length{$endif}(Strings[I]));
          if DisplayChars <= 0 then
            Break;
        end;
      end;
    end;

    { These events should be generated only when
      the default values of length and maxExtent are used.
      For now, we ignore length and maxExtent, so these events are
      simply always generated. }
    SendEvents;
  finally FreeAndNil(TextWidths) end;
end;

function TTextNode.Proxy(var State: TX3DGraphTraverseState): TAbstractGeometryNode;
var
  Size, Spacing: Single;
  Justify, JustifyMinor: TX3DFontJustify;
  FaceSet: TQuadSetNode;
  CoordNode: TCoordinateNode;
  MultiTexCoordNode: TMultiTextureCoordinateNode;
  TexCoordNode, ExtraTexCoordNode: TTextureCoordinateNode;
  I: Integer;
  ExtraTexCoordField: TMFVec2f;
begin
  if FontStyle is TFontStyleNode then // also checks FontStyle <> nil
  begin
    Size := TFontStyleNode(FontStyle).FdSize.Value;
    Spacing := TFontStyleNode(FontStyle).FdSpacing.Value;
    Justify := TFontStyleNode(FontStyle).Justify;
    JustifyMinor := TFontStyleNode(FontStyle).JustifyMinor;
  end else
  begin
    Size := TFontStyleNode.DefaultSize;
    Spacing := TFontStyleNode.DefaultSpacing;
    Justify := TFontStyleNode.DefaultJustify;
    JustifyMinor := TFontStyleNode.DefaultJustifyMinor;
  end;

  FaceSet := TQuadSetNode.Create(X3DName, BaseUrl);
  try
    FaceSet.Solid := Solid;

    CoordNode := TCoordinateNode.Create('', BaseUrl);
    FaceSet.Coord := CoordNode;

    MultiTexCoordNode := TMultiTextureCoordinateNode.Create('', BaseUrl);
    FaceSet.TexCoord := MultiTexCoordNode;

    TexCoordNode := TTextureCoordinateNode.Create('', BaseUrl);
    MultiTexCoordNode.FdTexCoord.Add(TexCoordNode);

    if (FdTexCoord.Value <> nil) and FdTexCoord.CurrentChildAllowed then
    begin
      { If you specify explicit Text.texCoord value, we assume you want
        to use it for Appearance.texture, instead of automatic texture
        coordinates that would be placed inside ExtraTexCoordNode.
        So we don't use ExtraTexCoordNode then.

        Note: don't worry about CastleInternalShadowMaps processing here.
        CastleInternalShadowMaps processing happens on the resulting geometry node,
        it *does not* modify source FdTexCoord field. So shadow maps
        will work on both textured and untextured text,
        because CastleInternalShadowMaps processing will actually cut off ExtraTexCoordNode
        in case they are unused (because text is not textured),
        before adding ProjectedTextureCoordinate. }
      ExtraTexCoordNode := nil;
      if FdTexCoord.Value is TMultiTextureCoordinateNode then
      begin
        for I := 0 to TMultiTextureCoordinateNode(FdTexCoord.Value).FdTexCoord.Count - 1 do
          MultiTexCoordNode.FdTexCoord.Add(
            TMultiTextureCoordinateNode(FdTexCoord.Value).FdTexCoord[I]);
      end else
        MultiTexCoordNode.FdTexCoord.Add(FdTexCoord.Value);
    end else
    begin
      ExtraTexCoordNode := TTextureCoordinateNode.Create('', BaseUrl);
      MultiTexCoordNode.FdTexCoord.Add(ExtraTexCoordNode);
    end;
    if ExtraTexCoordNode <> nil then
      ExtraTexCoordField := ExtraTexCoordNode.FdPoint else
      ExtraTexCoordField := nil;

    TextProxy(Self, Size, Spacing, Justify, JustifyMinor, FdString.Items,
      FdMaxDisplayChars.Value, Font,
      Solid, EventLineBounds, EventOrigin, EventTextBounds,
      CoordNode.FdPoint, TexCoordNode.FdPoint, ExtraTexCoordField);

    Result := FaceSet;
  except FreeAndNil(FaceSet); raise end;
end;

{ constants ------------------------------------------------------------------ }

const
  FontStyleNames: array [TX3DFontStyle] of String =
  ('PLAIN', 'BOLD', 'ITALIC', 'BOLDITALIC');

  FontFamilyNames: array [TX3DFontFamily] of string =
  ('SERIF', 'SANS', 'TYPEWRITER');

  JustifyToString: array[TX3DFontJustify] of string =
  ('FIRST', 'BEGIN', 'MIDDLE', 'END');

{ TFontStyleNode ------------------------------------------------------------- }

constructor TFontStyleNode.Create(const AX3DName: string = ''; const ABaseUrl: string = '');
begin
  inherited;
  FCustomFontObserver := TFreeNotificationObserver.Create(nil);
  FCustomFontObserver.OnFreeNotification := {$ifdef FPC}@{$endif}CustomFontFreeNotification;
end;

destructor TFontStyleNode.Destroy;
begin
  FreeAndNil(FCustomFontObserver);
  inherited;
end;

procedure TFontStyleNode.SetCustomFont(const Value: TCastleFont);
begin
  if FCustomFont <> Value then
  begin
    FCustomFont := Value;
    FCustomFontObserver.Observed := Value;
  end;
end;

procedure TFontStyleNode.CustomFontFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FCustomFont := nil;
end;

class function TFontStyleNode.ForVRMLVersion(const Version: TX3DVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TFontStyleNode.GetFamily: TX3DFontFamily;
var
  I: Integer;
begin
  for I := 0 to FdFamily.Items.Count - 1 do
    if FdFamily.Items[I] = 'SERIF' then
      Exit(ffSerif)
    else
    if FdFamily.Items[I] = 'SANS' then
      Exit(ffSans)
    else
    if FdFamily.Items[I] = 'TYPEWRITER' then
      Exit(ffTypeWriter)
    else
      WritelnWarning('VRML/X3D', 'Font family "' + FdFamily.Items[I] + '" not supported');

  { If no supported values on FdFamily.Items then fall back to serif }
  Result := ffSerif;
end;

procedure TFontStyleNode.SetFamily(const Value: TX3DFontFamily);
begin
  FdFamily.Send(FontFamilyNames[Value]);
end;

const
  StyleEnum: array[boolean { bold ? }, boolean { italic ? }] of TX3DFontStyle =
  ( (fsPlain, fsItalic),
    (fsBold, fsBoldItalic) );

function TFontStyleNode.GetBold: boolean;
begin
  Result := Style in [fsBold, fsBoldItalic];
end;

procedure TFontStyleNode.SetBold(const Value: boolean);
begin
  Style := StyleEnum[Value, Italic];
end;

function TFontStyleNode.GetItalic: boolean;
begin
  Result := Style in [fsItalic, fsBoldItalic];
end;

procedure TFontStyleNode.SetItalic(const Value: boolean);
begin
  Style := StyleEnum[Bold, Value];
end;

function TFontStyleNode.StringToJustify(const S: string;
  const DefaultValue: TX3DFontJustify): TX3DFontJustify;
const
  SJustifyObsolete = 'Font justify "%s" should not be used in VRML >= 2.0, use "%s" instead';
var
  J: string;
begin
  { Some X3D models use lowercase names, like [http://instant-reality.com/]
    test models. }
  J := UpperCase(S);
  if (J <> S) and not JustifyWarningUppercaseDone then
  begin
    WritelnWarning('VRML/X3D', Format('Font justify value "%s" should be specified in uppercase',
      [S]));
    JustifyWarningUppercaseDone := true;
  end;

  if J = ''       then Result := DefaultValue else
  if J = 'BEGIN'  then Result := fjBegin else
  if J = 'FIRST'  then Result := fjFirst else
  if J = 'MIDDLE' then Result := fjMiddle else
  if J = 'END'    then Result := fjEnd else

  { Some X3D models use old justify names, like cic.nist.gov X3D demos. }
  if J = 'LEFT' then
  begin
    if not JustifyWarningObsoleteDone then
    begin
      WritelnWarning('VRML/X3D', Format(SJustifyObsolete, ['LEFT', 'BEGIN']));
      JustifyWarningObsoleteDone := true;
    end;
    Result := fjBegin;
  end else
  if J = 'CENTER' then
  begin
    if not JustifyWarningObsoleteDone then
    begin
      WritelnWarning('VRML/X3D', Format(SJustifyObsolete, ['CENTER', 'MIDDLE']));
      JustifyWarningObsoleteDone := true;
    end;
    Result := fjMiddle;
  end else
  if J = 'RIGHT' then
  begin
    if not JustifyWarningObsoleteDone then
    begin
      WritelnWarning('VRML/X3D', Format(SJustifyObsolete, ['RIGHT', 'END']));
      JustifyWarningObsoleteDone := true;
    end;
    Result := fjEnd;
  end else

  begin
    Result := DefaultValue;
    if not JustifyWarningNotSupportedDone then
    begin
      WritelnWarning('VRML/X3D', 'Font justify "' + S + '" not supported');
      JustifyWarningNotSupportedDone := true;
    end;
  end;
end;

function TFontStyleNode.GetJustify: TX3DFontJustify;
begin
  if FdJustify.Items.Count > 0 then
    Result := StringToJustify(FdJustify.Items[0], DefaultJustify)
  else
    Result := DefaultJustify;
end;

function TFontStyleNode.GetJustifyMinor: TX3DFontJustify;
begin
  if FdJustify.Items.Count > 1 then
    Result := StringToJustify(FdJustify.Items[1], DefaultJustifyMinor)
  else
    Result := DefaultJustifyMinor;
end;

procedure TFontStyleNode.SetJustify(const Value: TX3DFontJustify);
begin
  FdJustify.Send([JustifyToString[Value], JustifyToString[JustifyMinor]]);
end;

procedure TFontStyleNode.SetJustifyMinor(const Value: TX3DFontJustify);
begin
  FdJustify.Send([JustifyToString[Justify], JustifyToString[Value]]);
end;

function TFontStyleNode.Font: TCastleFont;
begin
  { Check CustomFont.FontLoaded below, because this checks CustomFont.Data <> nil,
    and TX3DFontTexturesCache.LoadFontTexture requires Data <> nil. }
  if (CustomFont <> nil) and CustomFont.FontLoaded then
    Result := CustomFont
  else
    Result := TFontStyleNode.GetDefaultFont(Family, Bold, Italic);
end;

var
  DefaultFontData: array [
    TX3DFontFamily,
    { bold } Boolean,
    { italic } Boolean
  ] of TTextureFontData;

  DefaultFont: array [
    TX3DFontFamily,
    { bold } Boolean,
    { italic } Boolean
  ] of TCastleFont;

class function TFontStyleNode.GetDefaultFont(const Family: TX3DFontFamily; const Bold, Italic: Boolean): TCastleFont;
begin
  { Create TCastleFont on-demand, as it creates a texture,
    and we don't want to create all 3 * 2 * 2 font textures always,
    when most CGE applications will not use all X3D font versions. }
  if DefaultFont[Family, Bold, Italic] = nil then
  begin
    DefaultFont[Family, Bold, Italic] := TCastleFont.Create(nil);
    DefaultFont[Family, Bold, Italic].Load(DefaultFontData[Family, Bold, Italic]);
  end;
  Result := DefaultFont[Family, Bold, Italic];
end;

{ TTextNode ------------------------------------------------------------------ }

destructor TTextNode.Destroy;
begin
  if X3DCache <> nil then
    X3DCache.FreeFontTexture(FFontTextureNode);
  inherited;
end;

procedure TTextNode.FontChanged;
begin
  if X3DCache <> nil then
    X3DCache.FreeFontTexture(FFontTextureNode);
end;

function TTextNode.TexCoordField: TSFNode;
begin
  Result := FdTexCoord;
end;

function TTextNode.SolidField: TSFBool;
begin
  Result := FdSolid;
end;

function TTextNode.Font: TCastleFont;
begin
  if FontStyle is TFontStyleNode then // also checks FontStyle <> nil
    Result := TFontStyleNode(FontStyle).Font
  else
    Result := TFontStyleNode.GetDefaultFont(
      TFontStyleNode.DefaultFamily,
      TFontStyleNode.DefaultBold,
      TFontStyleNode.DefaultItalic);
end;

function TTextNode.FontTextureNode: TAbstractTexture2DNode;
var
  Blending: boolean;
begin
  if FFontTextureNode = nil then
  begin
    if FontStyle is TFontStyleNode then // also checks FontStyle <> nil
      Blending := TFontStyleNode(FontStyle).Blending
    else
      Blending := TFontStyleNode.DefaultBlending;
    FFontTextureNode := X3DCache.LoadFontTexture(Font, Blending);
  end;
  Result := FFontTextureNode;
end;

procedure TTextNode.SetString(const Value: array of string);
begin
  SetText(Value);
end;

procedure TTextNode.SetString(const Value: TCastleStringList);
begin
  SetText(Value);
end;

{ registration --------------------------------------------------------------- }

procedure RegisterTextNodes;
begin
  NodesManager.RegisterNodeClasses([
    TFontStyleNode,
    TTextNode
  ]);
end;

procedure FontsInitialization;
begin
  {$ifdef CASTLE_EMBED_ALL_3D_FONT_VARIATIONS}
  DefaultFontData[ffTypeWriter, true , true ] := TextureFont_DejaVuSansMonoBoldOblique_20;
  DefaultFontData[ffTypeWriter, true , false] := TextureFont_DejaVuSansMonoBold_20;
  DefaultFontData[ffTypeWriter, false, true ] := TextureFont_DejaVuSansMonoOblique_20;
  DefaultFontData[ffTypeWriter, false, false] := TextureFont_DejaVuSansMono_20;

  DefaultFontData[ffSerif, true , true ] := TextureFont_DejaVuSerifBoldItalic_20;
  DefaultFontData[ffSerif, true , false] := TextureFont_DejaVuSerifBold_20;
  DefaultFontData[ffSerif, false, true ] := TextureFont_DejaVuSerifItalic_20;
  DefaultFontData[ffSerif, false, false] := TextureFont_DejaVuSerif_20;

  DefaultFontData[ffSans, true , true ] := TextureFont_DejaVuSansBoldOblique_20;
  DefaultFontData[ffSans, true , false] := TextureFont_DejaVuSansBold_20;
  DefaultFontData[ffSans, false, true ] := TextureFont_DejaVuSansOblique_20;
  DefaultFontData[ffSans, false, false] := TextureFont_DejaVuSans_20;
  {$else}
  DefaultFontData[ffTypeWriter, true , true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffTypeWriter, true , false] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffTypeWriter, false, true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffTypeWriter, false, false] := TextureFont_DejaVuSans_20;

  DefaultFontData[ffSerif, true , true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSerif, true , false] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSerif, false, true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSerif, false, false] := TextureFont_DejaVuSans_20;

  DefaultFontData[ffSans, true , true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSans, true , false] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSans, false, true ] := TextureFont_DejaVuSans_20;
  DefaultFontData[ffSans, false, false] := TextureFont_DejaVuSans_20;
  {$endif}
end;

procedure FontsFinalization;
var
  Family: TX3DFontFamily;
  Bold, Italic: Boolean;
begin
  for Family := Low(Family) to High(Family) do
    for Bold := Low(Bold) to High(Bold) do
      for Italic := Low(Italic) to High(Italic) do
        FreeAndNil(DefaultFont[Family, Bold, Italic]);
end;

{$endif read_implementation}
