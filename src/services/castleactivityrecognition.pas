{
  Copyright 2020-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Activity recognition (detects user stationary, walking, running and so on) (TActivityRecognition). }
unit CastleActivityRecognition;

{$I castleconf.inc}

interface

uses Classes,
  CastleStringUtils, CastleTimeUtils;

type
  { Current user activity, used by @link(TActivityRecognition.Activity).
    See https://developer.apple.com/documentation/coremotion/cmmotionactivity
    for the underlying iOS docs. }
  TUserActivity = (
    uaStationary,
    uaWalking,
    uaRunning,
    uaAutomotive,
    uaCycling,
    uaUnknown
  );

  TUserActivities = set of TUserActivity;

  { Current user activity, used by @link(TActivityRecognition.ActivityConfidence).
    See https://developer.apple.com/documentation/coremotion/cmmotionactivity
    for the underlying iOS docs. }
  TUserActivityConfidence = (
    uacLow,
    uacMedium,
    uacHigh
  );

  { Activity recognition (detects user stationary, walking, running and so on) (TActivityRecognition).

    Usage:

    @orderedList(
      @item(Include the necessary integration code in your Android / iOS project.

        For iOS, add the "activity_regonition" service inside CastleEngineManifest.xml.
        See https://castle-engine.io/ios-Services
        and https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/activity_recognition/README.md .

        Build your project with the Castle Game Engine build tool:
        https://castle-engine.io/build_tool .)

      @item(Create an instance of this class. Only a single instance of this class is allowed.)

      @item(Call @link(TActivityRecognition.Start).)

      @item(Read @link(TActivityRecognition.Activity), @link(TActivityRecognition.PossibleActivities)
        at any point.
        You can assign a callback to @link(TActivityRecognition.OnChange)
        to be notified about changes.)
    )
  }
  TActivityRecognition = class(TComponent)
  private
    FStarted: Boolean;
    FActivityValid: Boolean;
    FActivity: TUserActivity;
    FPossibleActivities: TUserActivities;
    FActivityTime: TDateTime;
    FActivityConfidence: TUserActivityConfidence;
    FOnChange: TNotifyEvent;
    function MessageReceived(const Received: TCastleStringList;
      const ReceivedStream: TMemoryStream): boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Start recognizing activity. }
    procedure Start;

    { Stop recognizing activity. }
    procedure Stop;

    { Was @link(Start) called (not followed by @link(Stop)). }
    property Started: Boolean read FStarted;

    { Called when current activity changed.
      This is called right after setting these properties:

      @unorderedList(
        @itemSpacing Compact
        @item @link(ActivityValid)
        @item @link(Activity)
        @item @link(PossibleActivities)
        @item @link(ActivityTime)
        @item @link(ActivityConfidence)
      ) }
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    { Is some activity detected.
      Note that the detected activity may still be uaUnknown. }
    property ActivityValid: Boolean read FActivityValid;

    { Last detected most possible activity.
      Valid only if @link(ActivityValid). }
    property Activity: TUserActivity read FActivity;

    { Last detected possible activities.
      Valid only if @link(ActivityValid). }
    property PossibleActivities: TUserActivities read FPossibleActivities;

    { The time activity was updated last.
      Valid only if @link(ActivityValid). }
    property ActivityTime: TDateTime read FActivityTime;

    { Confidence in @link(Activity) and @link(PossibleActivities) values.
      Valid only if @link(ActivityValid). }
    property ActivityConfidence: TUserActivityConfidence read FActivityConfidence;
  end;

{ Convert between lowercase activity name and TUserActivity type. }
function ActivityToStr(const Value: TUserActivity): String;
{ Convert between lowercase activity name and TUserActivity type.
  @raises Exception In case of invalid String. }
function StrToActivity(const Value: String): TUserActivity;
{ Convert between lowercase activity confidence name and TUserActivityConfidence type. }
function ActivityConfidenceToStr(const Value: TUserActivityConfidence): String;
{ Convert between lowercase activity confidence name and TUserActivityConfidence type.
  @raises Exception In case of invalid String. }
function StrToActivityConfidence(const Value: String): TUserActivityConfidence;
{ Convert TUserActivities to a String, using Delimiter to separate items. }
function PossibleActivitiesToStr(const Value: TUserActivities; const Delimiter: String): String;

implementation

uses SysUtils,
  CastleMessaging;

{ global routines ------------------------------------------------------------ }

function ActivityToStr(const Value: TUserActivity): String;
const
  Names: array [TUserActivity] of String = (
    'stationary',
    'walking',
    'running',
    'automotive',
    'cycling',
    'unknown'
  );
begin
  Result := Names[Value];
end;

function StrToActivity(const Value: String): TUserActivity;
begin
  for Result := Low(TUserActivity) to High(TUserActivity) do
    if ActivityToStr(Result) = Value then
      Exit;
  raise Exception.CreateFmt('Activity name invalid: "%s"', [Value]);
end;

function ActivityConfidenceToStr(const Value: TUserActivityConfidence): String;
const
  Names: array [TUserActivityConfidence] of String = (
    'low',
    'medium',
    'high'
  );
begin
  Result := Names[Value];
end;

function StrToActivityConfidence(const Value: String): TUserActivityConfidence;
begin
  for Result := Low(TUserActivityConfidence) to High(TUserActivityConfidence) do
    if ActivityConfidenceToStr(Result) = Value then
      Exit;
  raise Exception.CreateFmt('Activity confidence name invalid: "%s"', [Value]);
end;

{ Internal, uses delimiter same as iOS/Android service. }
function StrToPossibleActivities(const Value: String): TUserActivities;
const
  { This is a nice separator, as it has really low chance of occuring in non-binary data.
    - It's also not 0, so it will not be confused with "end of string" (Pascal is invulnerable
      to this, and can have #0 in the middle of AnsiString freely
      but I'm not so sure about Java or Objective-C NSString).
    - It's also within ASCII range, so it will not occur within any UTF-8 multibyte sequence
      (UTF-8 treats specially only stuff > 128, and you can search for ASCII substrings disregaring
      the UTF-8 multibyte stuff, as far as I know). }
  PossibleActivitiesDelimiter = #2;
var
  Values: TCastleStringList;
  S: String;
begin
  if Value = '' then // otherwise SplitString('', ...) would return one value, equal ''
    Exit([]);

  Values := SplitString(Value, PossibleActivitiesDelimiter);
  Result := [];
  for S in Values do
    Include(Result, StrToActivity(S));
end;

{ Public, uses configurable delimiter. }
function PossibleActivitiesToStr(const Value: TUserActivities; const Delimiter: String): String;
var
  A: TUserActivity;
begin
  Result := '';
  for A in Value do
    Result := SAppendPart(Result, Delimiter, ActivityToStr(A));
end;

{ TActivityRecognition ------------------------------------------------------- }

constructor TActivityRecognition.Create(AOwner: TComponent);
begin
  inherited;
  Messaging.OnReceive.Add({$ifdef FPC}@{$endif} MessageReceived);
end;

destructor TActivityRecognition.Destroy;
begin
  if Messaging <> nil then
    Messaging.OnReceive.Remove({$ifdef FPC}@{$endif} MessageReceived);
  inherited;
end;

function TActivityRecognition.MessageReceived(const Received: TCastleStringList;
  const ReceivedStream: TMemoryStream): boolean;

  { Try to get most useful activity from the set of possible activities.
    If Activities contain only one item, return it,
    If Activities are empty, return uaUnknown.
    If Activities contain uaUnknown and another item, return another item.
    Otherwise return uaUnknown. }
  function MostUsefulActivity(Activities: TUserActivities): TUserActivity;
  var
    A, LastA: TUserActivity;
    C: Cardinal;
  begin
    Exclude(Activities, uaUnknown);

    LastA := uaStationary; // just to silence Delphi warning

    C := 0;
    for A in Activities do
    begin
      LastA := A;
      Inc(C);
    end;

    if C = 1 then
      Result := LastA
    else
      Result := uaUnknown;
  end;

begin
  Result := false;

  { Just a quick auto-test of MostUsefulActivity }
  (*
  Assert(MostUsefulActivity([uaUnknown]) = uaUnknown);
  Assert(MostUsefulActivity([]) = uaUnknown);
  Assert(MostUsefulActivity([uaUnknown, uaRunning]) = uaRunning);
  Assert(MostUsefulActivity([uaUnknown, uaWalking]) = uaWalking);
  Assert(MostUsefulActivity([uaRunning]) = uaRunning);
  Assert(MostUsefulActivity([uaWalking]) = uaWalking);
  Assert(MostUsefulActivity([uaUnknown, uaWalking, uaRunning]) = uaUnknown);
  *)

  if (Received.Count = 3) and
     (Received[0] = 'activity-recognition-change') then
  begin
    FActivityValid := true;
    FPossibleActivities := StrToPossibleActivities(Received[1]);
    FActivity := MostUsefulActivity(FPossibleActivities);
    FActivityTime := CastleNow;
    FActivityConfidence := StrToActivityConfidence(Received[2]);
    if Assigned(OnChange) then
      OnChange(Self);
    Result := true;
  end;
end;

procedure TActivityRecognition.Start;
begin
  Messaging.Send(['activity-recognition', 'start']);
  FStarted := true;
end;

procedure TActivityRecognition.Stop;
begin
  Messaging.Send(['activity-recognition', 'stop']);
  FStarted := false;
end;

end.
