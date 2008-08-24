{
  Copyright 2008 Michalis Kamburelis.

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
  INodeX3DTimeDependentNode = interface(INodeX3DChildNode)
  ['{19D68914-F2BA-4CFA-90A1-F561DB264678}']
    property Fdloop: TSFBool { read FFdloop } { };
    property FdpauseTime: TSFTime { read FFdpauseTime } { };
    property FdresumeTime: TSFTime { read FFdresumeTime } { };
    property FdstartTime: TSFTime { read FFdstartTime } { };
    property FdstopTime: TSFTime { read FFdstopTime } { };
    property EventelapsedTime: TVRMLEvent { read FEventelapsedTime } { };
    property EventisActive: TVRMLEvent { read FEventisActive } { };
    property EventisPaused: TVRMLEvent { read FEventisPaused } { };
  end;

  TNodeX3DTimeDependentNode = class(TNodeX3DChildNode, INodeX3DTimeDependentNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    private FFdloop: TSFBool;
    public property Fdloop: TSFBool read FFdloop;

    private FFdpauseTime: TSFTime;
    public property FdpauseTime: TSFTime read FFdpauseTime;

    private FFdresumeTime: TSFTime;
    public property FdresumeTime: TSFTime read FFdresumeTime;

    private FFdstartTime: TSFTime;
    public property FdstartTime: TSFTime read FFdstartTime;

    private FFdstopTime: TSFTime;
    public property FdstopTime: TSFTime read FFdstopTime;

    { Event: SFTime, out } { }
    private FEventelapsedTime: TVRMLEvent;
    public property EventelapsedTime: TVRMLEvent read FEventelapsedTime;

    { Event: SFBool, out } { }
    private FEventisActive: TVRMLEvent;
    public property EventisActive: TVRMLEvent read FEventisActive;

    { Event: SFBool, out } { }
    private FEventisPaused: TVRMLEvent;
    public property EventisPaused: TVRMLEvent read FEventisPaused;
  end;

  TTimeFunction = function: TKamTime of object;

  { Common helper for all X3DTimeDependentNode descedants.
    This includes things descending from class TNodeX3DTimeDependentNode,
    and from interface INodeX3DTimeDependentNode. }
  TTimeDependentNodeHandler = class
  private
    FIsActive: boolean;
    FIsPaused: boolean;
    FElapsedTime: TKamTime;
    procedure SetIsActive(const Value: boolean);
    procedure SetIsPaused(const Value: boolean);
    procedure SetElapsedTime(const Value: TKamTime);
  public
    Node: TVRMLNode;

    { These describe current state of this TimeDependentNode,
      see X3D specification about "Time" component.

      Their setting causes appropriate events to be generated.

      @groupBegin }
    property IsActive: boolean read FIsActive write SetIsActive;
    property IsPaused: boolean read FIsPaused write SetIsPaused;
    property ElapsedTime: TKamTime read FElapsedTime write SetElapsedTime;
    { @groupEnd }

    { Cycle interval for this time-dependent node. }
    OnCycleInterval: TTimeFunction;
    function CycleInterval: TKamTime;

    Fdloop: TSFBool;
    FdpauseTime: TSFTime;
    FdresumeTime: TSFTime;
    FdstartTime: TSFTime;
    FdstopTime: TSFTime;

    EventelapsedTime: TVRMLEvent;
    EventisActive: TVRMLEvent;
    EventisPaused: TVRMLEvent;
  end;

  TNodeTimeSensor = class(TNodeX3DTimeDependentNode, INodeX3DSensorNode)
  private
    FTimeDependentNodeHandler: TTimeDependentNodeHandler;
    function CycleInterval: TKamTime;
    procedure EventElapsedTimeReceive(Event: TVRMLEvent; Value: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdcycleInterval: TSFTime;
    public property FdcycleInterval: TSFTime read FFdcycleInterval;

    { Event: SFTime, out } { }
    private FEventcycleTime: TVRMLEvent;
    public property EventcycleTime: TVRMLEvent read FEventcycleTime;

    { Event: SFFloat, out } { }
    private FEventfraction_changed: TVRMLEvent;
    public property Eventfraction_changed: TVRMLEvent read FEventfraction_changed;

    { Event: SFTime, out } { }
    private FEventtime: TVRMLEvent;
    public property Eventtime: TVRMLEvent read FEventtime;

    private FFdenabled: TSFBool;
    public property Fdenabled: TSFBool read FFdenabled;

    property TimeDependentNodeHandler: TTimeDependentNodeHandler
      read FTimeDependentNodeHandler;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DTimeDependentNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdloop := TSFBool.Create(Self, 'loop', false);
  Fields.Add(FFdloop);

  FFdpauseTime := TSFTime.Create(Self, 'pauseTime', 0);
  Fields.Add(FFdpauseTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdresumeTime := TSFTime.Create(Self, 'resumeTime', 0);
  Fields.Add(FFdresumeTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstartTime := TSFTime.Create(Self, 'startTime', 0);
  Fields.Add(FFdstartTime);
  { X3D specification comment: (-Inf,Inf) }

  FFdstopTime := TSFTime.Create(Self, 'stopTime', 0);
  Fields.Add(FFdstopTime);
  { X3D specification comment: (-Inf,Inf) }

  FEventelapsedTime := TVRMLEvent.Create(Self, 'elapsedTime', TSFTime, false);
  Events.Add(FEventelapsedTime);

  FEventisActive := TVRMLEvent.Create(Self, 'isActive', TSFBool, false);
  Events.Add(FEventisActive);

  FEventisPaused := TVRMLEvent.Create(Self, 'isPaused', TSFBool, false);
  Events.Add(FEventisPaused);

  DefaultContainerField := 'children';
end;

procedure TTimeDependentNodeHandler.SetIsActive(const Value: boolean);
begin
  if Value <> FIsActive then
  begin
    FIsActive := Value;
    EventIsActive.Send(Value);
  end;
end;

procedure TTimeDependentNodeHandler.SetIsPaused(const Value: boolean);
begin
  if Value <> FIsPaused then
  begin
    FIsPaused := Value;
    EventIsPaused.Send(Value);
  end;
end;

procedure TTimeDependentNodeHandler.SetElapsedTime(const Value: TKamTime);
begin
  if Value <> FElapsedTime then
  begin
    FElapsedTime := Value;
    EventElapsedTime.Send(Value);
  end;
end;

function TTimeDependentNodeHandler.CycleInterval: TKamTime;
begin
  Assert(Assigned(OnCycleInterval));
  Result := OnCycleInterval();
end;

constructor TNodeTimeSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdcycleInterval := TSFTime.Create(Self, 'cycleInterval', 1);
  Fields.Add(FFdcycleInterval);
  { X3D specification comment: (0,Inf) }

  FEventcycleTime := TVRMLEvent.Create(Self, 'cycleTime', TSFTime, false);
  Events.Add(FEventcycleTime);

  FEventfraction_changed := TVRMLEvent.Create(Self, 'fraction_changed', TSFFloat, false);
  Events.Add(FEventfraction_changed);

  FEventtime := TVRMLEvent.Create(Self, 'time', TSFTime, false);
  Events.Add(FEventtime);

  FFdenabled := TSFBool.Create(Self, 'enabled', true);
  Fields.Add(FFdenabled);

  DefaultContainerField := 'children';

  FTimeDependentNodeHandler := TTimeDependentNodeHandler.Create;
  FTimeDependentNodeHandler.Node := Self;
  FTimeDependentNodeHandler.Fdloop := Fdloop;
  FTimeDependentNodeHandler.FdpauseTime := FdpauseTime;
  FTimeDependentNodeHandler.FdresumeTime := FdresumeTime;
  FTimeDependentNodeHandler.FdstartTime := FdstartTime;
  FTimeDependentNodeHandler.FdstopTime := FdstopTime;
  FTimeDependentNodeHandler.EventisActive:= EventisActive;
  FTimeDependentNodeHandler.EventisPaused := EventisPaused;
  FTimeDependentNodeHandler.EventelapsedTime := EventelapsedTime;
  FTimeDependentNodeHandler.OnCycleInterval := @CycleInterval;

  { TODO: for now, elapsedTime is simply passed to set_fraction event. }
  EventelapsedTime.OnReceive.AppendItem(@EventElapsedTimeReceive);
end;

destructor TNodeTimeSensor.Destroy;
begin
  FreeAndNil(FTimeDependentNodeHandler);
  inherited;
end;

function TNodeTimeSensor.CycleInterval: TKamTime;
begin
  Result := FdCycleInterval.Value;
end;

class function TNodeTimeSensor.ClassNodeTypeName: string;
begin
  Result := 'TimeSensor';
end;

function TNodeTimeSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Node is since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
end;

class function TNodeTimeSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeTimeSensor.EventElapsedTimeReceive(
  Event: TVRMLEvent; Value: TVRMLField);
begin
  { TODO: for now, elapsedTime/CycleInterval is simply passed to set_fraction event. }
  FEventfraction_changed.Send(
    Single((Value as TSFTime).Value / CycleInterval));
end;

procedure RegisterTimeNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeTimeSensor
  ]);
end;

{$endif read_implementation}
