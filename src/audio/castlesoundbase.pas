{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Sound engine basic types. }
unit CastleSoundBase;

{$I castleconf.inc}

interface

uses SysUtils, Generics.Collections;

type
  ENoMoreSources = class(Exception);

  TSoundDistanceModel = (dmNone,
    dmInverseDistance , dmInverseDistanceClamped,
    dmLinearDistance  , dmLinearDistanceClamped,
    dmExponentDistance, dmExponentDistanceClamped);

  TSoundDevice = class
  private
    FName, FCaption: string;
  public
    { Short device name, used for @link(TSoundEngine.Device). }
    property Name: string read FName;
    { Nice device name to show user. }
    property Caption: string read FCaption;
    property NiceName: string read FCaption; deprecated 'use Caption';
  end;

  TSoundDeviceList = class({$ifdef CASTLE_OBJFPC}specialize{$endif} TObjectList<TSoundDevice>)
    procedure Add(const AName, ACaption: string); reintroduce;
  end;

implementation

{ TSoundDeviceList ----------------------------------------------------------- }

procedure TSoundDeviceList.Add(const AName, ACaption: string);
var
  D: TSoundDevice;
begin
  D := TSoundDevice.Create;
  D.FName := AName;
  D.FCaption := ACaption;
  inherited Add(D);
end;

end.
