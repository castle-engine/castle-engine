// -*- compile-command: "cd ../ && ./compile_console.sh && ./test_castle_game_engine --suite=TTestX3DFields" -*-
{
  Copyright 2017-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test X3DFields unit. }
unit TestX3DFields;

interface

uses
  Classes, SysUtils, {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry,
  CastleTestCase{$else}CastleTester{$endif}, X3DFields, X3DTime;

type
  TTestX3DFields = class(TCastleTestCase)
  strict private
    Counter: Integer;
    procedure Check123(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  published
    procedure TestNotification;
  end;

implementation

uses CastleSceneCore, X3DNodes;

procedure TTestX3DFields.Check123(const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  AssertTrue(Value is TSFInt32);
  AssertEquals(123, (Value as TSFInt32).Value);
  Inc(Counter);
end;

procedure TTestX3DFields.TestNotification;
var
  Scene: TCastleSceneCore;
  Node: TX3DRootNode;
  E: TSFInt32Event;
begin
  { sending events requires a Scene and Node }
  Scene := TCastleSceneCore.Create(nil);
  try
    Node := TX3DRootNode.Create;
    Scene.Load(Node, true);

    E := TSFInt32Event.Create(Node, 'my field', true);
    try
      E.AddNotification({$ifdef FPC}@{$endif}Check123);
      Counter := 0;
      E.Send(123);
      AssertEquals(1, Counter);

      { adding the same notification callback is OK,
        and makes the callback called twice }
      E.AddNotification({$ifdef FPC}@{$endif}Check123);
      Counter := 0;
      E.Send(123);
      AssertEquals(2, Counter);

      { removing the notification removes only 1 copy of it }
      E.RemoveNotification({$ifdef FPC}@{$endif}Check123);
      Counter := 0;
      E.Send(123);
      AssertEquals(1, Counter);

      E.RemoveNotification({$ifdef FPC}@{$endif}Check123);
      Counter := 0;
      E.Send(123);
      AssertEquals(0, Counter);
    finally FreeAndNil(E) end;
  finally FreeAndNil(Scene) end;
end;

{$ifndef CASTLE_TESTER}
initialization
  RegisterTest(TTestX3DFields);
{$endif}
end.
