{ -*- buffer-read-only: t -*-

  Copyright 2015-2021 Michalis Kamburelis.

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
  - and regenerate include files by running x3d-nodes-to-pascal .

  The documentation for properties should go to x3dnodes_documentation.txt . }

{$ifdef read_interface}

  strict private
    function GetLevel: Integer;
    procedure SetLevel(const Value: Integer);
    function GetLogFile: String;
    procedure SetLogFile(const Value: String);
    function GetEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  public
    procedure CreateNode; override;
    class function ClassX3DType: string; override;

    strict private FFdLevel: TSFInt32;
    { X3D field "level". } { }
    public property FdLevel: TSFInt32 read FFdLevel;
    { X3D field "level". } { }
    property Level: Integer read GetLevel write SetLevel;

    strict private FFdLogFile: TSFString;
    { X3D field "logFile". } { }
    public property FdLogFile: TSFString read FFdLogFile;
    { X3D field "logFile". } { }
    property LogFile: String read GetLogFile write SetLogFile;

    strict private FFdEnabled: TSFBool;
    { X3D field "enabled". } { }
    public property FdEnabled: TSFBool read FFdEnabled;
    { X3D field "enabled". } { }
    property Enabled: Boolean read GetEnabled write SetEnabled;

    strict private FEventWrite: TXFAnyEvent;
    { X3D in event "write". } { }
    public property EventWrite: TXFAnyEvent read FEventWrite;

{$endif read_interface}

{$ifdef read_implementation}

{ TLoggerNode ----------------------------------------------- }

function TLoggerNode.GetLevel: Integer;
begin
  Result := FdLevel.Value;
end;

procedure TLoggerNode.SetLevel(const Value: Integer);
begin
  FdLevel.Send(Value);
end;

function TLoggerNode.GetLogFile: String;
begin
  Result := FdLogFile.Value;
end;

procedure TLoggerNode.SetLogFile(const Value: String);
begin
  FdLogFile.Send(Value);
end;

function TLoggerNode.GetEnabled: Boolean;
begin
  Result := FdEnabled.Value;
end;

procedure TLoggerNode.SetEnabled(const Value: Boolean);
begin
  FdEnabled.Send(Value);
end;

class function TLoggerNode.ClassX3DType: String;
begin
  Result := 'Logger';
end;

procedure TLoggerNode.CreateNode;
begin
  inherited;

  FFdLevel := TSFInt32.Create(Self, True, 'level', 1);
   FdLevel.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdLevel);

  FFdLogFile := TSFString.Create(Self, False, 'logFile', '');
   FdLogFile.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdLogFile);

  FFdEnabled := TSFBool.Create(Self, True, 'enabled', true);
   FdEnabled.ChangeAlways := chVisibleNonGeometry;
  AddField(FFdEnabled);

  FEventWrite := TXFAnyEvent.Create(Self, 'write', true);
  AddEvent(FEventWrite);
end;

{$endif read_implementation}
