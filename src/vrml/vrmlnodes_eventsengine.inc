{
  Copyright 2003-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TVRMLEventsEngine (ancestor of TVRMLScene, with which the nodes
  implementations interact) and related types. }

{$ifdef read_interface}

  { }
  TVRMLBindableStackBasic = class(TVRMLNodesList)
  public
    { Handle set_bind event send to given Node.
      This always generates appropriate events. }
    procedure Set_Bind(Node: TNodeX3DBindableNode; const Value: boolean); virtual; abstract;
  end;

  { Abstract 3D object notified about changes to VRML/X3D nodes/fields. }
  TVRMLChangesEngine = class(T3D)
  public
    procedure ChangedAll; virtual; abstract;
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); virtual; abstract;
    procedure ChangedField(Field: TVRMLField); virtual; abstract;
    procedure IncreaseTimeTick; virtual; abstract;
  end;

  TVRMLChangesEnginesList = class(specialize TFPGObjectList<TVRMLChangesEngine>)
  public
    { Notify listeners about a field value change.
      See TVRMLScene.ChangedField for exact specification. }
    procedure ChangedField(Field: TVRMLField);
  end;

  { Abstract 3D handling VRML/X3D events. }
  TVRMLEventsEngine = class(TVRMLChangesEngine)
  protected
    procedure ExecuteCompiledScript(const HandlerName: string;  ReceivedValue: TVRMLField); virtual; abstract;
  public
    function GetTime: TVRMLTime; virtual; abstract;
    function GetViewpointStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetNavigationInfoStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetBackgroundStack: TVRMLBindableStackBasic; virtual; abstract;
    function GetFogStack: TVRMLBindableStackBasic; virtual; abstract;
  end;
{$endif read_interface}

{$ifdef read_implementation}

procedure TVRMLChangesEnginesList.ChangedField(Field: TVRMLField);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].ChangedField(Field);
end;

{$endif read_implementation}
