{ -*- buffer-read-only: t -*-

  Copyright 2015-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Automatically generated node properties.

  Do not edit this file manually!
  To add new properties:
  - add them to the text files in tools/internal/x3d-nodes-to-pascal/nodes-specification/ ,
  - and regenerate include files by running x3d-nodes-to-pascal }

{$ifdef read_interface}

  public
    { Create node fields and events. }
    procedure CreateNode; override;
    class function ClassX3DType: String; override;

    strict private FFdAlign: TMFString;
    { Internal wrapper for property @code(Align). This wrapper API may change, we advise to access simpler @code(Align) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdAlign: TMFString read FFdAlign;
    {  }
    procedure SetAlign(const Value: array of string); overload;
    {  }
    procedure SetAlign(const Value: TCastleStringList); overload;

    strict private FFdOffset: TMFFloat;
    { Internal wrapper for property @code(Offset). This wrapper API may change, we advise to access simpler @code(Offset) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdOffset: TMFFloat read FFdOffset;
    {  }
    procedure SetOffset(const Value: array of Single); overload;
    {  }
    procedure SetOffset(const Value: TSingleList); overload;

    strict private FFdOffsetUnits: TMFString;
    { Internal wrapper for property @code(OffsetUnits). This wrapper API may change, we advise to access simpler @code(OffsetUnits) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdOffsetUnits: TMFString read FFdOffsetUnits;
    {  }
    procedure SetOffsetUnits(const Value: array of string); overload;
    {  }
    procedure SetOffsetUnits(const Value: TCastleStringList); overload;

    strict private FFdScaleMode: TMFString;
    { Internal wrapper for property @code(ScaleMode). This wrapper API may change, we advise to access simpler @code(ScaleMode) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdScaleMode: TMFString read FFdScaleMode;
    {  }
    procedure SetScaleMode(const Value: array of string); overload;
    {  }
    procedure SetScaleMode(const Value: TCastleStringList); overload;

    strict private FFdSize: TMFFloat;
    { Internal wrapper for property @code(Size). This wrapper API may change, we advise to access simpler @code(Size) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdSize: TMFFloat read FFdSize;
    {  }
    procedure SetSize(const Value: array of Single); overload;
    {  }
    procedure SetSize(const Value: TSingleList); overload;

    strict private FFdSizeUnits: TMFString;
    { Internal wrapper for property @code(SizeUnits). This wrapper API may change, we advise to access simpler @code(SizeUnits) instead, if it is defined (TODO: for now, some field types do not have a simpler counterpart). }
    public property FdSizeUnits: TMFString read FFdSizeUnits;
    {  }
    procedure SetSizeUnits(const Value: array of string); overload;
    {  }
    procedure SetSizeUnits(const Value: TCastleStringList); overload;

{$endif read_interface}

{$ifdef read_implementation}

{ TLayoutNode ----------------------------------------------- }

procedure TLayoutNode.SetAlign(const Value: array of string);
begin
  FdAlign.Send(Value);
end;

procedure TLayoutNode.SetAlign(const Value: TCastleStringList);
begin
  FdAlign.Send(Value);
end;

procedure TLayoutNode.SetOffset(const Value: array of Single);
begin
  FdOffset.Send(Value);
end;

procedure TLayoutNode.SetOffset(const Value: TSingleList);
begin
  FdOffset.Send(Value);
end;

procedure TLayoutNode.SetOffsetUnits(const Value: array of string);
begin
  FdOffsetUnits.Send(Value);
end;

procedure TLayoutNode.SetOffsetUnits(const Value: TCastleStringList);
begin
  FdOffsetUnits.Send(Value);
end;

procedure TLayoutNode.SetScaleMode(const Value: array of string);
begin
  FdScaleMode.Send(Value);
end;

procedure TLayoutNode.SetScaleMode(const Value: TCastleStringList);
begin
  FdScaleMode.Send(Value);
end;

procedure TLayoutNode.SetSize(const Value: array of Single);
begin
  FdSize.Send(Value);
end;

procedure TLayoutNode.SetSize(const Value: TSingleList);
begin
  FdSize.Send(Value);
end;

procedure TLayoutNode.SetSizeUnits(const Value: array of string);
begin
  FdSizeUnits.Send(Value);
end;

procedure TLayoutNode.SetSizeUnits(const Value: TCastleStringList);
begin
  FdSizeUnits.Send(Value);
end;

class function TLayoutNode.ClassX3DType: String;
begin
  Result := 'Layout';
end;

procedure TLayoutNode.CreateNode;
begin
  inherited;

  FFdAlign := TMFString.Create(Self, True, 'align', ['CENTER','CENTER']);
   FdAlign.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdAlign);

  FFdOffset := TMFFloat.Create(Self, True, 'offset', [0,0]);
   FdOffset.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdOffset);

  FFdOffsetUnits := TMFString.Create(Self, True, 'offsetUnits', ['WORLD','WORLD']);
   FdOffsetUnits.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdOffsetUnits);

  FFdScaleMode := TMFString.Create(Self, True, 'scaleMode', ['NONE','NONE']);
   FdScaleMode.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdScaleMode);

  FFdSize := TMFFloat.Create(Self, True, 'size', [1,1]);
   FdSize.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSize);

  FFdSizeUnits := TMFString.Create(Self, True, 'sizeUnits', ['WORLD','WORLD']);
   FdSizeUnits.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdSizeUnits);
end;

{$endif read_implementation}
