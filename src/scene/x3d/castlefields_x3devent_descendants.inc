{%MainUnit x3dfields.pas}
{
  Copyright 2013-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TX3DEvent descendants with comfortable Send methods.
  Note that it is *not* necessary to use descendants below,
  instead you can create and use direct TX3DEvent instances.
  The important thing is to set FieldClass right.

  These descendants only add easy Send methods, that can be type-checked
  at compile-time. They are all just shortcuts for calling
  the @link(Send Send(TX3DField, TX3DTime)) method.
  They create a temporary instance of TX3DField for this purpose
  (with field name equal to event name, this is useful for debugging
  --- e.g. by Logger node). }

{$ifdef read_interface}
  { }
  {$ifndef FPC}
  IX3DEvent<T> = interface
  ['{D17B73D3-2A0A-46A3-83F3-FC87EF906CCA}']
    procedure Send(const Value: T; const Time: TX3DTime; const PartialSend: TPartialSend = nil);
    function SendNeeded: boolean;
  end;

  {$endif}


  TSFBoolEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: boolean; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: boolean); overload;
  end;

  TSFDoubleEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: Double); overload;
  end;

  TSFTimeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TFloatTime; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TFloatTime); overload;
  end;

  TSFFloatEvent = class(TX3DEvent{$ifndef FPC}, IX3DEvent<Single>{$endif})
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: Single; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: Single); overload;
  end;

  TSFImageEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TSFLongEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: LongInt); overload;
  end;

  TSFInt32Event = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: LongInt); overload;
  end;

  TSFMatrix3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TMatrix3Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TMatrix3Double); overload;
  end;

  TSFMatrix3fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TMatrix3; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TMatrix3); overload;
  end;

  TSFMatrix4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TMatrix4Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TMatrix4Double); overload;
  end;

  TSFMatrix4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TMatrix4; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TMatrix4); overload;
  end;

  TSFNodeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TSFRotationEvent = class(TX3DEvent{$ifndef FPC}, IX3DEvent<TVector4>{$endif})
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector4; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector4); overload;
  end;

  TSFStringEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: string; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: string); overload;
  end;

  TSFVec2dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector2Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector2Double); overload;
  end;

  TSFVec2fEvent = class(TX3DEvent{$ifndef FPC}, IX3DEvent<TVector2>{$endif})
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector2; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector2); overload;
  end;

  TSFVec3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector3Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector3Double); overload;
  end;

  TSFVec3fEvent = class(TX3DEvent{$ifndef FPC}, IX3DEvent<TVector3>{$endif})
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector3; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector3); overload;
  end;

  TSFVec4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector4Double; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector4Double); overload;
  end;

  TSFVec4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector4; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector4); overload;
  end;

  TSFColorEvent = class(TX3DEvent{$ifndef FPC}, IX3DEvent<TVector3>{$endif})
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TVector3; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TVector3); overload;
  end;

  TSFColorRGBAEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
    procedure Send(const Value: TCastleColor; const Time: TX3DTime; const PartialSend: TPartialSend = nil); overload;
    procedure Send(const Value: TCastleColor); overload;
  end;

  { Universal input event that can receive value of any type.
    It is used by some InstantReality extensions, like Logger.write.
    InstantReality calls this event type XFAny. }
  TXFAnyEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFBoolEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFDoubleEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFTimeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFFloatEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFLongEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFInt32Event = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix3fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFMatrix4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFNodeEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFRotationEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFStringEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec2dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec2fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec3dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec3fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec4dEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFVec4fEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFColorEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;

  TMFColorRGBAEvent = class(TX3DEvent)
  public
    constructor Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
  end;
{$endif read_interface}

{$ifdef read_implementation}

{ TSFBoolEvent --------------------------------------------------------------- }

constructor TSFBoolEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFBool, AInEvent);
end;

procedure TSFBoolEvent.Send(const Value: boolean; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFBool).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFBoolEvent.Send(const Value: boolean);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFDoubleEvent ------------------------------------------------------------- }

constructor TSFDoubleEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFDouble, AInEvent);
end;

procedure TSFDoubleEvent.Send(const Value: Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFDouble).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFDoubleEvent.Send(const Value: Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFTimeEvent --------------------------------------------------------------- }

constructor TSFTimeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFTime, AInEvent);
end;

procedure TSFTimeEvent.Send(const Value: TFloatTime; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFTime).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFTimeEvent.Send(const Value: TFloatTime);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFFloatEvent -------------------------------------------------------------- }

constructor TSFFloatEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFFloat, AInEvent);
end;

procedure TSFFloatEvent.Send(const Value: Single; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFFloat).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFFloatEvent.Send(const Value: Single);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFImageEvent -------------------------------------------------------------- }

constructor TSFImageEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFImage, AInEvent);
end;

{ TSFLongEvent --------------------------------------------------------------- }

constructor TSFLongEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFLong, AInEvent);
end;

procedure TSFLongEvent.Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFLong).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFLongEvent.Send(const Value: LongInt);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFInt32Event -------------------------------------------------------------- }

constructor TSFInt32Event.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFInt32, AInEvent);
end;

procedure TSFInt32Event.Send(const Value: LongInt; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFInt32).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFInt32Event.Send(const Value: LongInt);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFMatrix3dEvent ----------------------------------------------------------- }

constructor TSFMatrix3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFMatrix3d, AInEvent);
end;

procedure TSFMatrix3dEvent.Send(const Value: TMatrix3Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFMatrix3d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFMatrix3dEvent.Send(const Value: TMatrix3Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFMatrix3fEvent ----------------------------------------------------------- }

constructor TSFMatrix3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFMatrix3f, AInEvent);
end;

procedure TSFMatrix3fEvent.Send(const Value: TMatrix3; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFMatrix3f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFMatrix3fEvent.Send(const Value: TMatrix3);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFMatrix4dEvent ----------------------------------------------------------- }

constructor TSFMatrix4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFMatrix4d, AInEvent);
end;

procedure TSFMatrix4dEvent.Send(const Value: TMatrix4Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFMatrix4d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFMatrix4dEvent.Send(const Value: TMatrix4Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFMatrix4fEvent ----------------------------------------------------------- }

constructor TSFMatrix4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFMatrix4f, AInEvent);
end;

procedure TSFMatrix4fEvent.Send(const Value: TMatrix4; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFMatrix4f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFMatrix4fEvent.Send(const Value: TMatrix4);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFNodeEvent --------------------------------------------------------------- }

constructor TSFNodeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFNode, AInEvent);
end;

{ TSFRotationEvent ----------------------------------------------------------- }

constructor TSFRotationEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFRotation, AInEvent);
end;

procedure TSFRotationEvent.Send(const Value: TVector4; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFRotation).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFRotationEvent.Send(const Value: TVector4);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFStringEvent ------------------------------------------------------------- }

constructor TSFStringEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFString, AInEvent);
end;

procedure TSFStringEvent.Send(const Value: string; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFString).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFStringEvent.Send(const Value: string);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec2dEvent -------------------------------------------------------------- }

constructor TSFVec2dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec2d, AInEvent);
end;

procedure TSFVec2dEvent.Send(const Value: TVector2Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec2d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec2dEvent.Send(const Value: TVector2Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec2fEvent -------------------------------------------------------------- }

constructor TSFVec2fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec2f, AInEvent);
end;

procedure TSFVec2fEvent.Send(const Value: TVector2; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec2f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec2fEvent.Send(const Value: TVector2);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec3dEvent -------------------------------------------------------------- }

constructor TSFVec3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec3d, AInEvent);
end;

procedure TSFVec3dEvent.Send(const Value: TVector3Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec3d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec3dEvent.Send(const Value: TVector3Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec3fEvent -------------------------------------------------------------- }

constructor TSFVec3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec3f, AInEvent);
end;

procedure TSFVec3fEvent.Send(const Value: TVector3; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec3f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec3fEvent.Send(const Value: TVector3);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec4dEvent -------------------------------------------------------------- }

constructor TSFVec4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec4d, AInEvent);
end;

procedure TSFVec4dEvent.Send(const Value: TVector4Double; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec4d).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec4dEvent.Send(const Value: TVector4Double);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFVec4fEvent -------------------------------------------------------------- }

constructor TSFVec4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFVec4f, AInEvent);
end;

procedure TSFVec4fEvent.Send(const Value: TVector4; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFVec4f).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFVec4fEvent.Send(const Value: TVector4);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFColorEvent -------------------------------------------------------------- }

constructor TSFColorEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFColor, AInEvent);
end;

procedure TSFColorEvent.Send(const Value: TVector3; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFColor).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFColorEvent.Send(const Value: TVector3);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ TSFColorRGBAEvent ---------------------------------------------------------- }

constructor TSFColorRGBAEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TSFColorRGBA, AInEvent);
end;

procedure TSFColorRGBAEvent.Send(const Value: TCastleColor; const Time: TX3DTime; const PartialSend: TPartialSend);
var
  Field: TX3DField;
begin
  Field := CreateTemp(PartialSend);
  (Field as TSFColorRGBA).Value := Value;
  try
    Send(Field, Time);
  finally FreeTemp(Field) end;
end;

procedure TSFColorRGBAEvent.Send(const Value: TCastleColor);
begin
  if (ParentNode <> nil) and
     (TX3DNode(ParentNode).Scene <> nil) then
    Send(Value, TX3DNode(ParentNode).Scene.NextEventTime);
end;

{ ---------------------------------------------------------------------------- }

constructor TXFAnyEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TX3DField, AInEvent);
end;

constructor TMFBoolEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFBool, AInEvent);
end;

constructor TMFDoubleEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFDouble, AInEvent);
end;

constructor TMFTimeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFTime, AInEvent);
end;

constructor TMFFloatEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFFloat, AInEvent);
end;

constructor TMFLongEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFLong, AInEvent);
end;

constructor TMFInt32Event.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFInt32, AInEvent);
end;

constructor TMFMatrix3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFMatrix3d, AInEvent);
end;

constructor TMFMatrix3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFMatrix3f, AInEvent);
end;

constructor TMFMatrix4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFMatrix4d, AInEvent);
end;

constructor TMFMatrix4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFMatrix4f, AInEvent);
end;

constructor TMFNodeEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFNode, AInEvent);
end;

constructor TMFRotationEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFRotation, AInEvent);
end;

constructor TMFStringEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFString, AInEvent);
end;

constructor TMFVec2dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec2d, AInEvent);
end;

constructor TMFVec2fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec2f, AInEvent);
end;

constructor TMFVec3dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec3d, AInEvent);
end;

constructor TMFVec3fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec3f, AInEvent);
end;

constructor TMFVec4dEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec4d, AInEvent);
end;

constructor TMFVec4fEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFVec4f, AInEvent);
end;

constructor TMFColorEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFColor, AInEvent);
end;

constructor TMFColorRGBAEvent.Create(AParentNode: TX3DFileItem; const AName: string; const AInEvent: boolean);
begin
  inherited Create(AParentNode, AName, TMFColorRGBA, AInEvent);
end;

{$endif read_implementation}
