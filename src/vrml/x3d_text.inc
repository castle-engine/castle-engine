{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TNodeX3DFontStyleNode = class(TNodeX3DNode)
  public
    procedure CreateNode; override;
  end;

  { Font family that can be specified by FontStyle node in family
    field. First three fields are equal (after casting by Ord) to
    three values of FSFAMILY_* constants. }
  TVRMLFontFamily = (ffSerif, ffSans, ffTypeWriter);

  { Font justification that can be specified by FontStyle in
    justify/justification field. First three fields are equal
    (after casting by Ord) to JUSTIFICATION_* constants. }
  TVRMLFontJustify = (fjBegin, fjMiddle, fjEnd);

  TNodeFontStyle = class(TNodeX3DFontStyleNode)
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

    class function ForVRMLVersion(const Version: TVRMLVersion): boolean;
      override;

    function Family: TVRMLFontFamily;
    function Bold: boolean;
    function Italic: boolean;
    function Justify: TVRMLFontJustify;
    function TTF_Font: PTrueTypeFont;

    class function DefaultSize: Single;
    class function DefaultSpacing: Single;
    class function DefaultFamily: TVRMLFontFamily;
    class function DefaultBold: boolean;
    class function DefaultItalic: boolean;
    class function DefaultJustify: TVRMLFontJustify;
    class function DefaultTTF_Font: PTrueTypeFont;

    class function ClassTTF_Font(AFamily: TVRMLFontFamily;
      const ABold, AItalic: boolean): PTrueTypeFont;
  end;
  TNodeFontStyle_2 = TNodeFontStyle;

  TNodeText = class(TNodeX3DGeometryNode)
  public
    procedure CreateNode; override;
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

    { Event: MFVec2f, out } { }
    private FEventLineBounds: TVRMLEvent;
    public property EventLineBounds: TVRMLEvent read FEventLineBounds;

    { Event: SFVec3f, out } { }
    private FEventOrigin: TVRMLEvent;
    public property EventOrigin: TVRMLEvent read FEventOrigin;

    { Event: SFVec2f, out } { }
    private FEventTextBounds: TVRMLEvent;
    public property EventTextBounds: TVRMLEvent read FEventTextBounds;

    private FFdSolid: TSFBool;
    public property FdSolid: TSFBool read FFdSolid;

    function LocalBoundingBox(State: TVRMLGraphTraverseState;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): TBox3D; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean;
      ProxyGeometry: TVRMLGeometryNode; ProxyState: TVRMLGraphTraverseState): Cardinal; override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TNodeFontStyle. }
    function FontStyle: TNodeFontStyle;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DFontStyleNode.CreateNode;
begin
  inherited;

  DefaultContainerField := 'fontStyle';
end;

const
  TTF_Font_Results: array[TVRMLFontFamily, boolean, boolean]of PTrueTypeFont =
  (              {   [],                          [italic],                            [bold],                      [italic, bold] }
    {serif}      ( ((@TTF_BitstreamVeraSerif),   (@TTF_BitstreamVeraSerif_Italic)),    ((@TTF_BitstreamVeraSerif_Bold),    (@TTF_BitstreamVeraSerif_Bold_Italic)) ),
    {sans}       ( ((@TTF_BitstreamVeraSans),    (@TTF_BitstreamVeraSans_Italic)),     ((@TTF_BitstreamVeraSans_Bold),     (@TTF_BitstreamVeraSans_Bold_Italic)) ),
    {typewriter} ( ((@TTF_BitstreamVeraSansMono),(@TTF_BitstreamVeraSansMono_Italic)), ((@TTF_BitstreamVeraSansMono_Bold), (@TTF_BitstreamVeraSansMono_Bold_Italic)) )
  );

procedure TNodeFontStyle.CreateNode;
begin
  inherited;

  FFdfamily := TMFString.Create(Self, 'family', ['SERIF']);
   Fdfamily.Exposed := false;
   Fdfamily.ChangesAlways := [chFontStyle];
  Fields.Add(FFdfamily);

  FFdhorizontal := TSFBool.Create(Self, 'horizontal', true);
   Fdhorizontal.Exposed := false;
   Fdhorizontal.ChangesAlways := [chFontStyle];
  Fields.Add(FFdhorizontal);

  FFdjustify := TMFString.Create(Self, 'justify', ['BEGIN']);
   Fdjustify.Exposed := false;
   Fdjustify.ChangesAlways := [chFontStyle];
  Fields.Add(FFdjustify);
  { X3D specification comment: ["BEGIN","END","FIRST","MIDDLE",""] }

  FFdlanguage := TSFString.Create(Self, 'language', '');
   Fdlanguage.Exposed := false;
   Fdlanguage.ChangesAlways := [chFontStyle];
  Fields.Add(FFdlanguage);

  FFdleftToRight := TSFBool.Create(Self, 'leftToRight', true);
   FdleftToRight.Exposed := false;
   FdleftToRight.ChangesAlways := [chFontStyle];
  Fields.Add(FFdleftToRight);

  FFdsize := TSFFloat.Create(Self, 'size', DefaultSize);
   Fdsize.Exposed := false;
   Fdsize.ChangesAlways := [chFontStyle];
  Fields.Add(FFdsize);
  { X3D specification comment: (0,Inf) }

  FFdspacing := TSFFloat.Create(Self, 'spacing', DefaultSpacing);
   Fdspacing.Exposed := false;
   Fdspacing.ChangesAlways := [chFontStyle];
  Fields.Add(FFdspacing);
  { X3D specification comment: [0,Inf) }

  FFdstyle := TSFString.Create(Self, 'style', 'PLAIN');
   Fdstyle.Exposed := false;
   Fdstyle.ChangesAlways := [chFontStyle];
  Fields.Add(FFdstyle);
  { X3D specification comment: ["PLAIN"|"BOLD"|"ITALIC"|"BOLDITALIC"|""] }

  FFdtopToBottom := TSFBool.Create(Self, 'topToBottom', true);
   FdtopToBottom.Exposed := false;
   FdtopToBottom.ChangesAlways := [chFontStyle];
  Fields.Add(FFdtopToBottom);
end;

class function TNodeFontStyle.ClassNodeTypeName: string;
begin
  Result := 'FontStyle';
end;

class function TNodeFontStyle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

class function TNodeFontStyle.ForVRMLVersion(const Version: TVRMLVersion): boolean;
begin
  Result := Version.Major >= 2;
end;

function TNodeFontStyle.TTF_Font: PTrueTypeFont;
begin
  Result := TTF_Font_Results[Family, Bold, Italic];
end;

class function TNodeFontStyle.ClassTTF_Font(
  AFamily: TVRMLFontFamily; const ABold, AItalic: boolean): PTrueTypeFont;
begin
  Result := TTF_Font_Results[AFamily, ABold, AItalic];
end;

function TNodeFontStyle.Family: TVRMLFontFamily;
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
      VRMLWarning(vwSerious, 'Font family "' + FdFamily.Items[I] + '" not supported');

  { If no supported values on FdFamily.Items then fall back to serif }
  Result := ffSerif;
end;

const
  StyleBold = 'BOLD';
  StyleBoldItalic = 'BOLDITALIC';
  StyleItalic = 'ITALIC';
  StylePlain = 'PLAIN';

function TNodeFontStyle.Bold: boolean;
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
      VRMLWarning(vwSerious, 'Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TNodeFontStyle.Italic: boolean;
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
      VRMLWarning(vwSerious, 'Font style "' + FdStyle.Value + '" not supported');
  end;
end;

function TNodeFontStyle.Justify: TVRMLFontJustify;
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
    if J <> FdJustify.Items[0] then
      VRMLWarning(vwSerious, Format(
        'Font justify value "%s" should be specified in uppercase',
        [ FdJustify.Items[0] ]));

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
      VRMLWarning(vwSerious, Format(SJustifyObsolete, ['LEFT', 'BEGIN']));
      Result := fjBegin;
    end else
    if J = 'CENTER' then
    begin
      VRMLWarning(vwSerious, Format(SJustifyObsolete, ['CENTER', 'MIDDLE']));
      Result := fjMiddle;
    end else
    if J = 'RIGHT' then
    begin
      VRMLWarning(vwSerious, Format(SJustifyObsolete, ['RIGHT', 'END']));
      Result := fjEnd;
    end else

    begin
      Result := fjBegin;
      VRMLWarning(vwSerious, 'Font justify "' + FdJustify.Items[0] +
        '" not supported');
    end;
  end;
end;

class function TNodeFontStyle.DefaultSize: Single;
begin
  Result := 1;
end;

class function TNodeFontStyle.DefaultSpacing: Single;
begin
  Result := 1;
end;

class function TNodeFontStyle.DefaultFamily: TVRMLFontFamily;
begin
  Result := ffSerif;
end;

class function TNodeFontStyle.DefaultBold: boolean;
begin
  Result := false;
end;

class function TNodeFontStyle.DefaultItalic: boolean;
begin
  Result := false;
end;

class function TNodeFontStyle.DefaultJustify: TVRMLFontJustify;
begin
  Result := fjBegin;
end;

class function TNodeFontStyle.DefaultTTF_Font: PTrueTypeFont;
begin
  Result := TTF_Font_Results[DefaultFamily, DefaultBold, DefaultItalic];
end;

procedure TNodeText.CreateNode;
begin
  inherited;

  FFdfontStyle := TSFNode.Create(Self, 'fontStyle', [TNodeX3DFontStyleNode]);
   FdfontStyle.ChangesAlways := [chGeometry];
  Fields.Add(FFdfontStyle);

  FFdlength := TMFFloat.Create(Self, 'length', []);
   Fdlength.ChangesAlways := [chGeometry];
  Fields.Add(FFdlength);
  { X3D specification comment: [0,Inf) }

  FFdmaxExtent := TSFFloat.Create(Self, 'maxExtent', 0.0);
   FdmaxExtent.ChangesAlways := [chGeometry];
  Fields.Add(FFdmaxExtent);
  { X3D specification comment: [0,Inf) }

  FFdstring := TMFString.Create(Self, 'string', []);
   Fdstring.ChangesAlways := [chGeometry];
  Fields.Add(FFdstring);

  FEventlineBounds := TVRMLEvent.Create(Self, 'lineBounds', TMFVec2f, false);
  Events.Add(FEventlineBounds);

  FEventorigin := TVRMLEvent.Create(Self, 'origin', TSFVec3f, false);
  Events.Add(FEventorigin);

  FEventtextBounds := TVRMLEvent.Create(Self, 'textBounds', TSFVec2f, false);
  Events.Add(FEventtextBounds);

  FFdsolid := TSFBool.Create(Self, 'solid', false);
   Fdsolid.Exposed := false;
   Fdsolid.ChangesAlways := [chGeometry];
  Fields.Add(FFdsolid);
end;

class function TNodeText.ClassNodeTypeName: string;
begin
  Result := 'Text';
end;

class function TNodeText.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

function TNodeText.FontStyle: TNodeFontStyle;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TNodeFontStyle) then
    Result := TNodeFontStyle(FdFontStyle.Value) else
    Result := nil;
end;

procedure RegisterTextNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeFontStyle,
    TNodeText
  ]);
end;

{$endif read_implementation}
