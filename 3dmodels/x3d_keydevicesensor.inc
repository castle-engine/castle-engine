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
  TNodeX3DKeyDeviceSensorNode = class(TNodeX3DSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
  end;

  TNodeKeySensor = class(TNodeX3DKeyDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    { Event: SFInt32, out } { }
    private FEventactionKeyPress: TVRMLEvent;
    public property EventactionKeyPress: TVRMLEvent read FEventactionKeyPress;

    { Event: SFInt32, out } { }
    private FEventactionKeyRelease: TVRMLEvent;
    public property EventactionKeyRelease: TVRMLEvent read FEventactionKeyRelease;

    { Event: SFBool, out } { }
    private FEventaltKey: TVRMLEvent;
    public property EventaltKey: TVRMLEvent read FEventaltKey;

    { Event: SFBool, out } { }
    private FEventcontrolKey: TVRMLEvent;
    public property EventcontrolKey: TVRMLEvent read FEventcontrolKey;

    { Event: SFString, out } { }
    private FEventkeyPress: TVRMLEvent;
    public property EventkeyPress: TVRMLEvent read FEventkeyPress;

    { Event: SFString, out } { }
    private FEventkeyRelease: TVRMLEvent;
    public property EventkeyRelease: TVRMLEvent read FEventkeyRelease;

    { Event: SFBool, out } { }
    private FEventshiftKey: TVRMLEvent;
    public property EventshiftKey: TVRMLEvent read FEventshiftKey;
  end;

  TNodeStringSensor = class(TNodeX3DKeyDeviceSensorNode)
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddeletionAllowed: TSFBool;
    public property FddeletionAllowed: TSFBool read FFddeletionAllowed;

    { Event: SFString, out } { }
    private FEvententeredText: TVRMLEvent;
    public property EvententeredText: TVRMLEvent read FEvententeredText;

    { Event: SFString, out } { }
    private FEventfinalText: TVRMLEvent;
    public property EventfinalText: TVRMLEvent read FEventfinalText;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DKeyDeviceSensorNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  DefaultContainerField := 'children';
end;

constructor TNodeKeySensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FEventactionKeyPress := TVRMLEvent.Create(Self, 'actionKeyPress', TSFInt32, false);
  Events.Add(FEventactionKeyPress);

  FEventactionKeyRelease := TVRMLEvent.Create(Self, 'actionKeyRelease', TSFInt32, false);
  Events.Add(FEventactionKeyRelease);

  FEventaltKey := TVRMLEvent.Create(Self, 'altKey', TSFBool, false);
  Events.Add(FEventaltKey);

  FEventcontrolKey := TVRMLEvent.Create(Self, 'controlKey', TSFBool, false);
  Events.Add(FEventcontrolKey);

  FEventkeyPress := TVRMLEvent.Create(Self, 'keyPress', TSFString, false);
  Events.Add(FEventkeyPress);

  FEventkeyRelease := TVRMLEvent.Create(Self, 'keyRelease', TSFString, false);
  Events.Add(FEventkeyRelease);

  FEventshiftKey := TVRMLEvent.Create(Self, 'shiftKey', TSFBool, false);
  Events.Add(FEventshiftKey);

  DefaultContainerField := 'children';
end;

class function TNodeKeySensor.ClassNodeTypeName: string;
begin
  Result := 'KeySensor';
end;

function TNodeKeySensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeKeySensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

constructor TNodeStringSensor.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddeletionAllowed := TSFBool.Create(Self, 'deletionAllowed', true);
  Fields.Add(FFddeletionAllowed);

  FEvententeredText := TVRMLEvent.Create(Self, 'enteredText', TSFString, false);
  Events.Add(FEvententeredText);

  FEventfinalText := TVRMLEvent.Create(Self, 'finalText', TSFString, false);
  Events.Add(FEventfinalText);

  DefaultContainerField := 'children';
end;

class function TNodeStringSensor.ClassNodeTypeName: string;
begin
  Result := 'StringSensor';
end;

function TNodeStringSensor.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);
end;

class function TNodeStringSensor.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure RegisterKeyDeviceSensorNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeKeySensor,
    TNodeStringSensor
  ]);
end;

{$endif read_implementation}
