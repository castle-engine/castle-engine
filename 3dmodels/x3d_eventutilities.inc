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
  TNodeX3DSequencerNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;

    { Event: SFBool, in } { }
    private FEventnext: TVRMLEvent;
    public property Eventnext: TVRMLEvent read FEventnext;

    { Event: SFBool, in } { }
    private FEventprevious: TVRMLEvent;
    public property Eventprevious: TVRMLEvent read FEventprevious;

    { Event: SFFloat, in } { }
    private FEventset_fraction: TVRMLEvent;
    public property Eventset_fraction: TVRMLEvent read FEventset_fraction;

    private FFdkey: TMFFloat;
    public property Fdkey: TMFFloat read FFdkey;
  end;

  TNodeX3DTriggerNode = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeBooleanFilter = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    { Event: SFBool, out } { }
    private FEventinputFalse: TVRMLEvent;
    public property EventinputFalse: TVRMLEvent read FEventinputFalse;

    { Event: SFBool, out } { }
    private FEventinputNegate: TVRMLEvent;
    public property EventinputNegate: TVRMLEvent read FEventinputNegate;

    { Event: SFBool, out } { }
    private FEventinputTrue: TVRMLEvent;
    public property EventinputTrue: TVRMLEvent read FEventinputTrue;
  end;

  TNodeBooleanSequencer = class(TNodeX3DSequencerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFBool;
    public property FdkeyValue: TMFBool read FFdkeyValue;

    { Event: SFBool, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeBooleanToggle = class(TNodeX3DChildNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    private FFdtoggle: TSFBool;
    public property Fdtoggle: TSFBool read FFdtoggle;
  end;

  TNodeBooleanTrigger = class(TNodeX3DTriggerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFTime, in } { }
    private FEventset_triggerTime: TVRMLEvent;
    public property Eventset_triggerTime: TVRMLEvent read FEventset_triggerTime;

    { Event: SFBool, out } { }
    private FEventtriggerTrue: TVRMLEvent;
    public property EventtriggerTrue: TVRMLEvent read FEventtriggerTrue;
  end;

  TNodeIntegerSequencer = class(TNodeX3DSequencerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdkeyValue: TMFInt32;
    public property FdkeyValue: TMFInt32 read FFdkeyValue;

    { Event: SFInt32, out } { }
    private FEventvalue_changed: TVRMLEvent;
    public property Eventvalue_changed: TVRMLEvent read FEventvalue_changed;
  end;

  TNodeIntegerTrigger = class(TNodeX3DTriggerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    private FFdintegerKey: TSFInt32;
    public property FdintegerKey: TSFInt32 read FFdintegerKey;

    { Event: SFInt32, out } { }
    private FEventtriggerValue: TVRMLEvent;
    public property EventtriggerValue: TVRMLEvent read FEventtriggerValue;
  end;

  TNodeTimeTrigger = class(TNodeX3DTriggerNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFBool, in } { }
    private FEventset_boolean: TVRMLEvent;
    public property Eventset_boolean: TVRMLEvent read FEventset_boolean;

    { Event: SFTime, out } { }
    private FEventtriggerTime: TVRMLEvent;
    public property EventtriggerTime: TVRMLEvent read FEventtriggerTime;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DSequencerNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventnext := TVRMLEvent.Create('next', TSFBool, true);
  Events.Add(FEventnext);

  FEventprevious := TVRMLEvent.Create('previous', TSFBool, true);
  Events.Add(FEventprevious);

  FEventset_fraction := TVRMLEvent.Create('set_fraction', TSFFloat, true);
  Events.Add(FEventset_fraction);

  FFdkey := TMFFloat.Create('key', []);
  Fields.Add(FFdkey);
  { X3D specification comment: (-Inf,Inf) }

  DefaultContainerField := 'children';
end;

constructor TNodeX3DTriggerNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeBooleanFilter.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create('set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);

  FEventinputFalse := TVRMLEvent.Create('inputFalse', TSFBool, false);
  Events.Add(FEventinputFalse);

  FEventinputNegate := TVRMLEvent.Create('inputNegate', TSFBool, false);
  Events.Add(FEventinputNegate);

  FEventinputTrue := TVRMLEvent.Create('inputTrue', TSFBool, false);
  Events.Add(FEventinputTrue);

  DefaultContainerField := 'children';
end;

class function TNodeBooleanFilter.ClassNodeTypeName: string;
begin
  Result := 'BooleanFilter';
end;

function TNodeBooleanFilter.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBooleanFilter.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeBooleanSequencer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFBool.Create('keyValue', []);
  Fields.Add(FFdkeyValue);

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFBool, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeBooleanSequencer.ClassNodeTypeName: string;
begin
  Result := 'BooleanSequencer';
end;

function TNodeBooleanSequencer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBooleanSequencer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeBooleanToggle.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create('set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);

  FFdtoggle := TSFBool.Create('toggle', false);
  Fields.Add(FFdtoggle);

  DefaultContainerField := 'children';
end;

class function TNodeBooleanToggle.ClassNodeTypeName: string;
begin
  Result := 'BooleanToggle';
end;

function TNodeBooleanToggle.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBooleanToggle.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeBooleanTrigger.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_triggerTime := TVRMLEvent.Create('set_triggerTime', TSFTime, true);
  Events.Add(FEventset_triggerTime);

  FEventtriggerTrue := TVRMLEvent.Create('triggerTrue', TSFBool, false);
  Events.Add(FEventtriggerTrue);

  DefaultContainerField := 'children';
end;

class function TNodeBooleanTrigger.ClassNodeTypeName: string;
begin
  Result := 'BooleanTrigger';
end;

function TNodeBooleanTrigger.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeBooleanTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIntegerSequencer.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFdkeyValue := TMFInt32.Create('keyValue', []);
  Fields.Add(FFdkeyValue);
  { X3D specification comment: (-Inf,Inf) }

  FEventvalue_changed := TVRMLEvent.Create('value_changed', TSFInt32, false);
  Events.Add(FEventvalue_changed);

  DefaultContainerField := 'children';
end;

class function TNodeIntegerSequencer.ClassNodeTypeName: string;
begin
  Result := 'IntegerSequencer';
end;

function TNodeIntegerSequencer.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIntegerSequencer.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeIntegerTrigger.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create('set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);

  { X3D specification mistakenly omitted default value for this field...
    I guess it's 0? }
  FFdintegerKey := TSFInt32.Create('integerKey', 0);
  Fields.Add(FFdintegerKey);
  { X3D specification comment: (-Inf,Inf) }

  FEventtriggerValue := TVRMLEvent.Create('triggerValue', TSFInt32, false);
  Events.Add(FEventtriggerValue);

  DefaultContainerField := 'children';
end;

class function TNodeIntegerTrigger.ClassNodeTypeName: string;
begin
  Result := 'IntegerTrigger';
end;

function TNodeIntegerTrigger.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeIntegerTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeTimeTrigger.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventset_boolean := TVRMLEvent.Create('set_boolean', TSFBool, true);
  Events.Add(FEventset_boolean);

  FEventtriggerTime := TVRMLEvent.Create('triggerTime', TSFTime, false);
  Events.Add(FEventtriggerTime);

  DefaultContainerField := 'children';
end;

class function TNodeTimeTrigger.ClassNodeTypeName: string;
begin
  Result := 'TimeTrigger';
end;

function TNodeTimeTrigger.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeTimeTrigger.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterEventUtilitiesNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeBooleanFilter,
    TNodeBooleanSequencer,
    TNodeBooleanToggle,
    TNodeBooleanTrigger,
    TNodeIntegerSequencer,
    TNodeIntegerTrigger,
    TNodeTimeTrigger
  ]);
end;

{$endif read_implementation}
