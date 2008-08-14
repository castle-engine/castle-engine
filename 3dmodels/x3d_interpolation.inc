{
  Copyright 2002-2008 Michalis Kamburelis.

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

{$ifdef read_interface}
  TNodeX3DInterpolatorNode = class(TNodeX3DChildNode)
  protected
    { Find a range within "key" field corresponding to given Fraction.
      Returns the index of @bold(right) range delimiter.
      So for normal ranges (between two values of "key" field) it's
      always between 1 and FdKey.Count - 1. Result 0 indicates we're
      before the left limit, and result equal FdKey.Count indicates
      we're after right limit.

      Result is always between 0 and FdKey.Count.

      Output T is the value between 0..1 indicating where within
      the range we are. It's undefined when Result is 0 or Key.Count
      (indicating we're outside limits).

      Call this only when FdKey.Count > 0. }
    function KeyRange(const Fraction: Single; out T: Single): Integer;
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;
  end;

  TNodeColorInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFColor;
    public property FdkeyValue: TMFColor read FFdkeyValue;

    { Event: SFColor, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeCoordinateInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeCoordinateInterpolator2D = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    { Event: MFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeEaseInEaseOut = class(TNodeX3DNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdeaseInEaseOut: TMFVec2f;
    public property FdeaseInEaseOut: TMFVec2f read FFdeaseInEaseOut;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;

    { Event: SFFloat, out } { }
    private FEventmodifiedFraction_changed: TVRMLEvent;
    public property EventmodifiedFraction_changed: TVRMLEvent read FEventmodifiedFraction_changed;
  end;

  TNodeNormalInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: MFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeOrientationInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFRotation;
    public property FdkeyValue: TMFRotation read FFdkeyValue;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodePositionInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodePositionInterpolator2D = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeScalarInterpolator = class(TNodeX3DInterpolatorNode)
  private
    procedure EventSet_FractionReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFFloat;
    public property FdkeyValue: TMFFloat read FFdkeyValue;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplinePositionInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFVec3f;
    public property FdkeyValue: TMFVec3f read FFdkeyValue;

    private FFdkeyVelocity: TMFVec3f;
    public property FdkeyVelocity: TMFVec3f read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFVec3f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplinePositionInterpolator2D = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFVec2f;
    public property FdkeyValue: TMFVec2f read FFdkeyValue;

    private FFdkeyVelocity: TMFVec2f;
    public property FdkeyVelocity: TMFVec2f read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFVec2f, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSplineScalarInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdclosed: TSFBool;
    public property Fdclosed: TSFBool read FFdclosed;

    private FFdkeyValue: TMFFloat;
    public property FdkeyValue: TMFFloat read FFdkeyValue;

    private FFdkeyVelocity: TMFFloat;
    public property FdkeyVelocity: TMFFloat read FFdkeyVelocity;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFFloat, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeSquadOrientationInterpolator = class(TNodeX3DInterpolatorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFRotation;
    public property FdkeyValue: TMFRotation read FFdkeyValue;

    private FFdnormalizeVelocity: TSFBool;
    public property FdnormalizeVelocity: TSFBool read FFdnormalizeVelocity;

    { Event: SFRotation, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{$define SingleInterpolator :=

  function KeyValue(Index: Integer): SingleInterpolatorType;
  begin
    if Index < FdKeyValue.Items.Count then
      Result := FdKeyValue.Items.Items[Index] else
      Result := SingleInterpolatorDefault;
  end;

var
  T: Single;
  OutputValue: SingleInterpolatorType;
  KeyCount, Range: Integer;
begin
  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    OutputValue := KeyValue(0);
  end else
  begin
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
      OutputValue := Lerp(T, KeyValue(Range - 1), KeyValue(Range));
  end;
  EventValue_Changed.Send(OutputValue);
end;}

{$define MultipleInterpolator :=
var
  KeyCount: Integer;

  function KeyValue(Index: Integer): MultipleInterpolatorFieldType;
  var
    FrameValues: Integer;
  begin
    FrameValues := FdKeyValue.Items.Count div KeyCount;
    Result := MultipleInterpolatorFieldType.CreateUndefined('');

    if (Index + 1) * FrameValues <= FdKeyValue.Items.Count then
    begin
      Result.Items.AppendDynArray(FdKeyValue.Items, Index * FrameValues, FrameValues);
    end else
      VRMLNonFatalError(Format('Not enough keyValue items for %s interpolator',
        [NodeTypeName]));
  end;

  // This actually knows that Index2 = Index1 + 1, but assumes only
  // Index2 > Index1
  function KeyValueLerp(const T: Single;
    Index1, Index2: Integer): MultipleInterpolatorFieldType;
  var
    FrameValues: Integer;
  begin
    FrameValues := FdKeyValue.Items.Count div KeyCount;
    Result := MultipleInterpolatorFieldType.CreateUndefined('');

    if (Index2 + 1) * FrameValues <= FdKeyValue.Items.Count then
    begin
      Result.Items.AssignLerp(T, FdKeyValue.Items, FdKeyValue.Items,
        Index1 * FrameValues, Index2 * FrameValues, FrameValues);
    end else
      VRMLNonFatalError(Format('Not enough keyValue items for %s interpolator',
        [NodeTypeName]));
  end;

var
  T: Single;
  OutputValue: MultipleInterpolatorFieldType;
  Range: Integer;
begin
  KeyCount := FdKey.Items.Count;
  if KeyCount = 0 then
  begin
    OutputValue := KeyValue(0);
  end else
  begin
    Range := KeyRange((Value as TSFFloat).Value, T);
    if Range = 0 then
      OutputValue := KeyValue(0) else
    if Range = KeyCount then
      OutputValue := KeyValue(KeyCount - 1) else
    begin
      OutputValue := KeyValueLerp(T, Range - 1, Range);
      MultipleInterpolatorFieldTypePostprocess
    end;
  end;
  try
    EventValue_Changed.Send(OutputValue);
  finally
    FreeAndNil(OutputValue);
  end;
end;}

constructor TNodeX3DInterpolatorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

function TNodeX3DInterpolatorNode.KeyRange(
  const Fraction: Single; out T: Single): Integer;
var
  Key: TDynSingleArray;
  A, B: Integer;
begin
  Key := FdKey.Items;
  Assert(Key.Count > 0);

  if Fraction <= Key.Items[0] then
    Result := 0 else
  if Fraction >= Key.Items[Key.High] then
    Result := Key.Count else
  begin
    { Then for sure we're between two Key values.
      Note that we know that Key.Count > 1 (otherwise, Key.First = Key.Last
      so one of <= or >= comparisons above would occur). }
    Assert(Key.Count > 1);

    { Always A < B.
      We're considering range from Key[A] to Key[B].

      Remember that we cannot actually depend on the fact that
      Key values are non-decreasing. They should be non-decreasing,
      and we have to output correct result only when they are non-decreasing,
      but we also have to terminate (with any result) in any situation.
      Reason: Key values are supplied in X3D file, so they may be broken
      in every possible way. }

    A := 0;
    B := Key.High;
    while B - A > 1 do
    begin
      Result := (A + B) div 2;
      { A < B => (A + B) < 2B => (A + B) div 2 < B => Result < B.
        Also, Result > A (the only way how Result could be = A
        would be when B = A + 1, but we eliminated this case by "while"
        condition".

        This is good, it means A < Result < B, so Result is good candidate
        for next A or B, it will for sure shorten the distance
        between A and B. }
      Assert(A < Result);
      Assert(Result < B);
      if Fraction <= Key[Result] then
        B := Result else
        A := Result;
    end;
    Result := B;

    if Key[B] > Key[A] then
      T := (Fraction - Key[A]) / (Key[B] - Key[A]) else
      T := 0;
  end;
end;

constructor TNodeColorInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFColor.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: [0,1] }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFColor, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeColorInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ColorInterpolator';
end;

function TNodeColorInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeColorInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeColorInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
SingleInterpolator

constructor TNodeCoordinateInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeCoordinateInterpolator.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator';
end;

function TNodeCoordinateInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeCoordinateInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCoordinateInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorFieldTypePostprocess := }
MultipleInterpolator

constructor TNodeCoordinateInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeCoordinateInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'CoordinateInterpolator2D';
end;

function TNodeCoordinateInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeCoordinateInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeCoordinateInterpolator2D.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define MultipleInterpolatorFieldType := TMFVec2f}
{$define MultipleInterpolatorFieldTypePostprocess := }
MultipleInterpolator

constructor TNodeEaseInEaseOut.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdeaseInEaseOut := TMFVec2f.Create('easeInEaseOut', []);
  Fields.Add(FFdeaseInEaseOut);
  { X3D specification comment: (-Inf,Inf) }

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  FEventmodifiedFraction_changed := TVRMLEvent.Create('modifiedFraction_changed', TSFFloat, false);
  Events.Add(FEventmodifiedFraction_changed);

  DefaultContainerField := 'children';
end;

class function TNodeEaseInEaseOut.ClassNodeTypeName: string;
begin
  Result := 'EaseInEaseOut';
end;

function TNodeEaseInEaseOut.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeEaseInEaseOut.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeNormalInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TMFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeNormalInterpolator.ClassNodeTypeName: string;
begin
  Result := 'NormalInterpolator';
end;

function TNodeNormalInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeNormalInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeNormalInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define MultipleInterpolatorFieldType := TMFVec3f}
{$define MultipleInterpolatorFieldTypePostprocess := OutputValue.Items.Normalize;}
MultipleInterpolator

constructor TNodeOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFRotation.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: [-1,1] or (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'OrientationInterpolator';
end;

function TNodeOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeOrientationInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define SingleInterpolatorType := TVector4Single}
{$define SingleInterpolatorDefault := ZeroVector4Single}
SingleInterpolator

constructor TNodePositionInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator';
end;

function TNodePositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define SingleInterpolatorType := TVector3Single}
{$define SingleInterpolatorDefault := ZeroVector3Single}
SingleInterpolator

constructor TNodePositionInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodePositionInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'PositionInterpolator2D';
end;

function TNodePositionInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodePositionInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodePositionInterpolator2D.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define SingleInterpolatorType := TVector2Single}
{$define SingleInterpolatorDefault := ZeroVector2Single}
SingleInterpolator

constructor TNodeScalarInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFFloat.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';

  EventSet_Fraction.OnReceive.AppendItem(@EventSet_FractionReceive);
end;

class function TNodeScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'ScalarInterpolator';
end;

function TNodeScalarInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeScalarInterpolator.EventSet_FractionReceive(
  Event: TVRMLEvent; Value: TVRMLField);
{$define SingleInterpolatorType := Single}
{$define SingleInterpolatorDefault := 0}
SingleInterpolator

constructor TNodeSplinePositionInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFVec3f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFVec3f.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec3f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplinePositionInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator';
end;

function TNodeSplinePositionInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplinePositionInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSplinePositionInterpolator2D.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFVec2f.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFVec2f.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFVec2f, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplinePositionInterpolator2D.ClassNodeTypeName: string;
begin
  Result := 'SplinePositionInterpolator2D';
end;

function TNodeSplinePositionInterpolator2D.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplinePositionInterpolator2D.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSplineScalarInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdclosed := TSFBool.Create('closed', false);
  Fields.Add(FFdclosed);

  FFdkeyValue := TMFFloat.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdkeyVelocity := TMFFloat.Create('keyVelocity', []);
  Fields.Add(FFdkeyVelocity);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFFloat, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSplineScalarInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SplineScalarInterpolator';
end;

function TNodeSplineScalarInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSplineScalarInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeSquadOrientationInterpolator.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFRotation.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FFdnormalizeVelocity := TSFBool.Create('normalizeVelocity', false);
  Fields.Add(FFdnormalizeVelocity);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFRotation, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeSquadOrientationInterpolator.ClassNodeTypeName: string;
begin
  Result := 'SquadOrientationInterpolator';
end;

function TNodeSquadOrientationInterpolator.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeSquadOrientationInterpolator.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterInterpolationNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeColorInterpolator,
    TNodeCoordinateInterpolator,
    TNodeCoordinateInterpolator2D,
    TNodeEaseInEaseOut,
    TNodeNormalInterpolator,
    TNodeOrientationInterpolator,
    TNodePositionInterpolator,
    TNodePositionInterpolator2D,
    TNodeScalarInterpolator,
    TNodeSplinePositionInterpolator,
    TNodeSplinePositionInterpolator2D,
    TNodeSplineScalarInterpolator,
    TNodeSquadOrientationInterpolator
  ]);
end;

{$endif read_implementation}
