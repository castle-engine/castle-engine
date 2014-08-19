{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine bone timelines. }

{$ifdef read_interface}
  TBoneTimeline = class abstract
    Bone: TBone;
    Time: TSingleList;
    // TODO: curve not supported yet
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const Json: TJSONArray); virtual;
    procedure ParseSingleValue(const Json: TJSONObject); virtual; abstract;
  end;

  TBoneTimelineVector2 = class(TBoneTimeline)
    { Position or scale values on the timeline.
      This always has the same length as @link(Time) list. }
    Vectors: TVector2SingleList;
    constructor Create;
    destructor Destroy; override;
  end;

  TBoneTimelineTranslate = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
  end;

  TBoneTimelineScale = class(TBoneTimelineVector2)
    procedure ParseSingleValue(const Json: TJSONObject); override;
  end;

  TBoneTimelineRotate = class(TBoneTimeline)
    { Angle values on the timeline.
      This always has the same length as @link(Time) list. }
    Angles: TSingleList;
    constructor Create;
    destructor Destroy; override;
    procedure ParseSingleValue(const Json: TJSONObject); override;
  end;

  TBoneTimelineList = class(specialize TFPGObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}

{ TBoneTimeline -------------------------------------------------------------- }

constructor TBoneTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
end;

destructor TBoneTimeline.Destroy;
begin
  FreeAndNil(Time);
  inherited;
end;

procedure TBoneTimeline.Parse(const Json: TJSONArray);
var
  I: Integer;
  O: TJSONObject;
begin
  for I := 0 to Json.Count - 1 do
    if Json[I] is TJSONObject then
    begin
      O := TJSONObject(Json[I]);
      Time.Add(O.Get('time', 0.0));
      ParseSingleValue(O);
    end;
  //Writeln('got timeline for bone ', Bone.Name, ' with ', Time.Count, ' items');
end;

{ TBoneTimelineVector2 ------------------------------------------------------- }

constructor TBoneTimelineVector2.Create;
begin
  inherited Create;
  Vectors := TVector2SingleList.Create;
end;

destructor TBoneTimelineVector2.Destroy;
begin
  FreeAndNil(Vectors);
  inherited;
end;

{ TBoneTimelineTranslate ----------------------------------------------------- }

procedure TBoneTimelineTranslate.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2Single(
    Json.Get('x', 0),
    Json.Get('y', 0)));
end;

{ TBoneTimelineScale --------------------------------------------------------- }

procedure TBoneTimelineScale.ParseSingleValue(const Json: TJSONObject);
begin
  Vectors.Add(Vector2Single(
    Json.Get('x', 1),
    Json.Get('y', 1)));
end;

{ TBoneTimelineRotate -------------------------------------------------------- }

constructor TBoneTimelineRotate.Create;
begin
  inherited;
  Angles := TSingleList.Create;
end;

destructor TBoneTimelineRotate.Destroy;
begin
  FreeAndNil(Angles);
  inherited;
end;

procedure TBoneTimelineRotate.ParseSingleValue(const Json: TJSONObject);
begin
  Angles.Add(Json.Get('angle', 0.0));
end;

{$endif}
