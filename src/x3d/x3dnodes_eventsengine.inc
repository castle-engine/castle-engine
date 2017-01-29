{
  Copyright 2003-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TX3DEventsEngine (ancestor of TCastleSceneCore, with which the nodes
  implementations interact) and related types. }

{$ifdef read_interface}

  { }
  TX3DBindableStackBasic = class(TX3DNodeList)
  public
    { Handle set_bind event send to given Node.
      This always generates appropriate events. }
    procedure Set_Bind(Node: TAbstractBindableNode; const Value: boolean); virtual; abstract;
  end;

  { Abstract 3D object handling VRML/X3D events.
    Is also notified about changes to VRML/X3D nodes/fields.

    See TCastleSceneCore for documentation of these methods,
    TCastleSceneCore is the only implementation of this interface in practice. }
  TX3DEventsEngine = class(T3D)
  protected
    procedure ExecuteCompiledScript(const HandlerName: string;  ReceivedValue: TX3DField); virtual; abstract;
  public
    function GetBackgroundStack: TX3DBindableStackBasic; virtual; abstract;
    function GetFogStack: TX3DBindableStackBasic; virtual; abstract;
    function GetNavigationInfoStack: TX3DBindableStackBasic; virtual; abstract;
    function GetViewpointStack: TX3DBindableStackBasic; virtual; abstract;
    function Time: TFloatTime; virtual; abstract;
    function NextEventTime: TX3DTime; virtual; abstract;
    procedure ChangedAll; virtual; abstract;
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); virtual; abstract;
    procedure InternalChangedField(Field: TX3DField); virtual; abstract;
  end;

  TX3DEventsEngineList = class(specialize TFPGObjectList<TX3DEventsEngine>)
  public
    { Notify listeners about a field value change.
      See TCastleSceneCore.InternalChangedField for exact specification. }
    procedure InternalChangedField(Field: TX3DField);
  end;
{$endif read_interface}

{$ifdef read_implementation}

procedure TX3DEventsEngineList.InternalChangedField(Field: TX3DField);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].InternalChangedField(Field);
end;

{$endif read_implementation}
