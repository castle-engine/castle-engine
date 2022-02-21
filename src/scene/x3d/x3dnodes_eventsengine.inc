{
  Copyright 2003-2022 Michalis Kamburelis.

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

  { Abstract class representing a scene that renders and processed events of the X3D nodes.
    See TCastleSceneCore for documentation of these methods,
    TCastleSceneCore is the only implementation of this abstract class in practice. }
  TX3DEventsEngine = class(TCastleTransform)
  protected
    procedure ExecuteCompiledScript(const HandlerName: string;  ReceivedValue: TX3DField); virtual; abstract;

    { Called before changing one node into another,
      when old node may have beeen associated with a shape using TShapeTree.AssociateNode. }
    procedure InternalMoveShapeAssociations(const OldNode, NewNode: TX3DNode; const ContainingShapes: TObject); virtual; abstract;
  public
    { @exclude
      Make sure to update time-dependent nodes (calling TTimeDependentFunctionality.SetTime
      on all of them) in nearest InternalSetTime,
      and also make sure to call InternalSetTime at the beginning of nearest LocalRender.
      This is useful to call when you start new animation by TTimeSensorNode.Start.

      (when you start new animation by PlayAnimation, then a similar mechanism
      is activated by internal NewPlayingAnimationUse). }
    InternalNeedsUpdateTimeDependent: Boolean;

    function GetBackgroundStack: TX3DBindableStackBasic; virtual; abstract;
    function GetFogStack: TX3DBindableStackBasic; virtual; abstract;
    function GetNavigationInfoStack: TX3DBindableStackBasic; virtual; abstract;
    function GetViewpointStack: TX3DBindableStackBasic; virtual; abstract;
    function Time: TFloatTime; virtual; abstract;
    function NextEventTime: TX3DTime; virtual; abstract;
    procedure ChangedAll(const OnlyAdditions: Boolean = false); virtual; abstract;
    procedure BeforeNodesFree(const InternalChangedAll: boolean = false); virtual; abstract;
    procedure InternalChangedField(const Field: TX3DField; const Change: TX3DChange); overload; virtual; abstract;
    procedure InternalChangedField(const Field: TX3DField); overload;
  end;

  TX3DEventsEngineList = class({$ifdef FPC}specialize{$endif} TObjectList<TX3DEventsEngine>)
  end;

{$endif read_interface}

{$ifdef read_implementation}

procedure TX3DEventsEngine.InternalChangedField(const Field: TX3DField);
begin
  InternalChangedField(Field, Field.ExecuteChange);
end;

{$endif read_implementation}
