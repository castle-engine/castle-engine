{
  Copyright 2003-2012 Michalis Kamburelis.

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
    Is also notified about changes to VRML/X3D nodes/fields. }
  TX3DEventsEngine = class(T3D)
  protected
    procedure ExecuteCompiledScript(const HandlerName: string;  ReceivedValue: TX3DField); virtual; abstract;
  public
    function GetTime: TX3DTime; virtual; abstract;
    function GetViewpointStack: TX3DBindableStackBasic; virtual; abstract;
    function GetNavigationInfoStack: TX3DBindableStackBasic; virtual; abstract;
    function GetBackgroundStack: TX3DBindableStackBasic; virtual; abstract;
    function GetFogStack: TX3DBindableStackBasic; virtual; abstract;

    procedure ChangedAll; virtual; abstract;
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); virtual; abstract;
    procedure ChangedField(Field: TX3DField); virtual; abstract;
    procedure IncreaseTimeTick; virtual; abstract;
  end;

  TX3DEventsEngineList = class(specialize TFPGObjectList<TX3DEventsEngine>)
  public
    { Notify listeners about a field value change.
      See TCastleSceneCore.ChangedField for exact specification. }
    procedure ChangedField(Field: TX3DField);
  end;
{$endif read_interface}

{$ifdef read_implementation}

procedure TX3DEventsEngineList.ChangedField(Field: TX3DField);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangedField(Field);
end;

{$endif read_implementation}
