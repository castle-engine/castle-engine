{
  Copyright 2002-2007 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ Nodes that are defined specifically by Kambi VRML game engine.
  See [http://vrmlengine.sourceforge.net/kambi_vrml_extensions.php]. } { }

{$ifdef read_interface}
  TNodeKambiTriangulation = class(TVRMLNode)
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property FdQuadricSlices: TSFLong index 0 read GetFieldAsSFLong;
    property FdQuadricStacks: TSFLong index 1 read GetFieldAsSFLong;
    property FdRectDivisions: TSFLong index 2 read GetFieldAsSFLong;

    { zwracaja wartosc z odpowiedniego pola FdXxx lub,
      jesli ta wartosc jest -1, zwracaja Detail_Xxx.

      (Jesli ta wartosc jest nieprawidlowa to wywoluja VRMLNonFatalError
      a potem "po cichu" zmieniaja ta wartosc na wartosc wzieta z Detail_Xxx.
      A wiec poprawiaja blednego VRMLa.) }
    function QuadricStacks: Cardinal;
    function QuadricSlices: Cardinal;
    function RectDivisions: Cardinal;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeKambiHeadLight = class(TVRMLNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdAmbientIntensity: TSFFloat index 0 read GetFieldAsSFFloat;
    property FdAttenuation: TSFVec3f index 1 read GetFieldAsSFVec3f;
    property FdColor: TSFColor index 2 read GetFieldAsSFColor;
    property FdIntensity: TSFFloat index 3 read GetFieldAsSFFloat;
    property FdSpot: TSFBool index 4 read GetFieldAsSFBool;
    property FdSpotCutOffAngle: TSFFloat index 5 read GetFieldAsSFFloat;
    property FdSpotDropOffRate: TSFFloat index 6 read GetFieldAsSFFloat;

    class function URNMatching(const URN: string): boolean; override;
  end;

  TNodeText3D = class(TNodeGeneralShape)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;

    property Fdstring: TMFString index 0 read GetFieldAsMFString;
    property FdfontStyle: TSFNode index 1 read GetFieldAsSFNode;
    property Fdlength: TMFFloat index 2 read GetFieldAsMFFloat;
    property FdmaxExtent: TSFFloat index 3 read GetFieldAsSFFloat;

    { Fields added compared to normal Text node: }

    { }
    property FdDepth: TSFFloat index 4 read GetFieldAsSFFloat;
    property FdSolid: TSFBool index 5 read GetFieldAsSFBool;

    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    function BoundingBox(State: TVRMLGraphTraverseState): TBox3d; override;
    function VerticesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    function TrianglesCount(State: TVRMLGraphTraverseState; OverTriangulate: boolean): Cardinal; override;
    procedure LocalTriangulate(State: TVRMLGraphTraverseState; OverTriangulate: boolean; NewTriangleProc: TNewTriangleProc); override;

    { This returns FdFontStyle.Value. Returns nil if FdFontStyle.Value
      is nil or if it's not TNodeFontStyle_2. }
    function FontStyle: TNodeFontStyle_2;
  end;

  TNodeKambiAppearance = class(TNodeAppearance)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    property FdNormalMap: TSFNode index 3 read GetFieldAsSFNode;
    class function URNMatching(const URN: string): boolean; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
const
  TriangulationUseDef = -1;

constructor TNodeKambiTriangulation.Create(const ANodeName: string; const AWWWBasePath: string);
begin
 inherited;
 Fields.Add(TSFLong.Create('quadricSlices', TriangulationUseDef));
 Fields.Add(TSFLong.Create('quadricStacks', TriangulationUseDef));
 Fields.Add(TSFLong.Create('rectDivisions', TriangulationUseDef));
end;

class function TNodeKambiTriangulation.ClassNodeTypeName: string;
begin
 result := 'KambiTriangulation';
end;

{$define TRIANGULATION_DETAIL_FUNC:=
function TNodeKambiTriangulation.TRIANGULATION_DETAIL_FUNC_NAME: Cardinal;
begin
 if TRIANGULATION_DETAIL_FIELD.Value = TriangulationUseDef then
  result := TRIANGULATION_DETAIL_GLOBAL_VALUE else
 begin
  if Int64(TRIANGULATION_DETAIL_FIELD.Value) < Int64(TRIANGULATION_DETAIL_MIN) then
  begin
   VRMLNonFatalError(Format('Node "KambiTriangulation" '+
     'field "%s" value is %d but must be >= %d (or = -1)',
     [TRIANGULATION_DETAIL_FIELD_STRING,
      TRIANGULATION_DETAIL_FIELD.Value,
      TRIANGULATION_DETAIL_MIN]));
   TRIANGULATION_DETAIL_FIELD.Value := TRIANGULATION_DETAIL_GLOBAL_VALUE;
  end;

  result := TRIANGULATION_DETAIL_FIELD.Value;
 end;
end;}

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricSlices}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricSlices}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricSlices}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricSlices'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := QuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD := FdQuadricStacks}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_QuadricStacks}
  {$define TRIANGULATION_DETAIL_MIN := MinQuadricStacks}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'quadricStacks'}
  TRIANGULATION_DETAIL_FUNC

  {$define TRIANGULATION_DETAIL_FUNC_NAME := RectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD := FdRectDivisions}
  {$define TRIANGULATION_DETAIL_GLOBAL_VALUE := Detail_RectDivisions}
  {$define TRIANGULATION_DETAIL_MIN := MinRectDivisions}
  {$define TRIANGULATION_DETAIL_FIELD_STRING := 'rectDivisions'}
  TRIANGULATION_DETAIL_FUNC

{$undef TRIANGULATION_DETAIL_FUNC_NAME}
{$undef TRIANGULATION_DETAIL_FIELD}
{$undef TRIANGULATION_DETAIL_GLOBAL_VALUE}
{$undef TRIANGULATION_DETAIL_MIN}
{$undef TRIANGULATION_DETAIL_FIELD_STRING}
{$undef TRIANGULATION_DETAIL_FUNC}

class function TNodeKambiTriangulation.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeKambiHeadLight.ClassNodeTypeName: string;
begin
  Result := 'KambiHeadLight';
end;

constructor TNodeKambiHeadLight.Create(const ANodeName: string; const AWWWBasePath: string);
const
  HeadLightDefaultAmbientIntensity = 0;
  HeadLightDefaultAttenuation: TVector3Single = (1, 0, 0);
  HeadLightDefaultColor: TVector3Single = (1, 1, 1);
  HeadLightDefaultIntensity = 1.0;
  HeadLightDefaultSpot = false;
  HeadLightDefaultSpotCutOffAngle = 0.785398;
  HeadLightDefaultSpotDropOffRate = 0.0;
begin
  inherited;
  Fields.Add(TSFFloat.Create('ambientIntensity', HeadLightDefaultAmbientIntensity));
  Fields.Add(TSFVec3f.Create('attenuation', HeadLightDefaultAttenuation));
  Fields.Add(TSFColor.Create('color', HeadLightDefaultColor));
  Fields.Add(TSFFloat.Create('intensity', HeadLightDefaultIntensity));
  Fields.Add(TSFBool.Create('spot', HeadLightDefaultSpot));
  Fields.Add(TSFFloat.Create('spotCutOffAngle', HeadLightDefaultSpotCutOffAngle));
  Fields.Add(TSFFloat.Create('spotDropOffRate', HeadLightDefaultSpotDropOffRate));
end;

class function TNodeKambiHeadLight.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

class function TNodeText3D.ClassNodeTypeName: string;
begin
  Result := 'Text3D';
end;

class function TNodeText3D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNKambiNodes + ClassNodeTypeName);
end;

constructor TNodeText3D.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TMFString.Create('string', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFNode.Create(Self, 'fontStyle', [TNodeFontStyle_2])); Fields.Last.Exposed := true;
  Fields.Add(TMFFloat.Create('length', [])); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('maxExtent', 0.0)); Fields.Last.Exposed := true;
  Fields.Add(TSFFloat.Create('depth', 0.1, true)); Fields.Last.Exposed := true;
  Fields.Add(TSFBool.Create('solid', true)); Fields.Last.Exposed := true;
end;

function TNodeText3D.FontStyle: TNodeFontStyle_2;
begin
  if (FdFontStyle.Value <> nil) and
     (FdFontStyle.Value is TNodeFontStyle_2) then
    Result := TNodeFontStyle_2(FdFontStyle.Value) else
    Result := nil;
end;

class function TNodeKambiAppearance.ClassNodeTypeName: string;
begin
  Result := 'KambiAppearance';
end;

class function TNodeKambiAppearance.URNMatching(const URN: string): boolean;
begin
  { Don't call "inherited URNMatching(URN)", since we don't want to
    "intercept" URNs that should indicate standard Appearance node. }
  Result := (URN = URNKambiNodes + ClassNodeTypeName);
end;

constructor TNodeKambiAppearance.Create(const ANodeName: string; const AWWWBasePath: string);
begin
  inherited;
  Fields.Add(TSFNode.Create(Self, 'normalMap',
    [TNodeImageTexture, TNodeMovieTexture, TNodePixelTexture]));
  Fields.Last.Exposed := true;
end;

{$endif read_implementation}
