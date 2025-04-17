{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test Fairy integration (TTestFairy). }
unit CastleTestFairy;

{$I castleconf.inc}

interface

uses Classes;

type
  { TestFairy integration.
    On Android ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/android/integrated-services/test_fairy/README.md )
    and on iOS ( https://github.com/castle-engine/castle-engine/blob/master/tools/build-tool/data/ios/services/test_fairy/README.md ).
  }
  TTestFairy = class(TComponent)
  strict private
    class procedure LogCallback(const Message: String);
    class procedure FinalizeRemoteLogging;
  public
    { Send CGE logs to TestFairy.
      This is onlly necessary on iOS, on Android all the logs are automatically send to TestFairy.
      It does nothing in case TestFairy service is not used. }
    class procedure InitializeRemoteLogging;
  end;

implementation

uses SysUtils,
  CastleApplicationProperties, CastleLog, CastleUtils, CastleTimeUtils;

class procedure TTestFairy.InitializeRemoteLogging;
begin
  // While it would work on all platforms, it's pointless (not handled) on other than iOS platforms
  {$ifdef CASTLE_IOS}
  { In case logging started before TTestFairy.InitializeRemoteLogging was called,
    push to TestFairy some introductory messages. }
  {$warnings off} // using deprecated knowingly, it should be renamed
  if Log then
  {$warnings on}
  begin
    TTestFairy.LogCallback('Log for "' + ApplicationName + '".' + NL);
    if ApplicationProperties.Version <> '' then
      TTestFairy.LogCallback('  Version: ' + ApplicationProperties.Version + '.' + NL);
    TTestFairy.LogCallback('  TestFairy logging started on ' + DateTimeToAtStr(CastleNow) + '.' + NL);
    TTestFairy.LogCallback('  Castle Game Engine version: ' + CastleEngineVersion + '.' + NL);
    TTestFairy.LogCallback('  Compiled with ' + SCompilerDescription + '.' + NL);
    TTestFairy.LogCallback('  Platform: ' + SPlatformDescription + '.' + NL);
  end;

  if ApplicationProperties.OnLog.IndexOf({$ifdef FPC}@{$endif} LogCallback) = -1 then
    ApplicationProperties.OnLog.Add({$ifdef FPC}@{$endif} LogCallback);
  {$endif}
end;

class procedure TTestFairy.FinalizeRemoteLogging;
begin
  ApplicationProperties.OnLog.Remove({$ifdef FPC}@{$endif} LogCallback);
end;

{$ifdef CASTLE_IOS}
procedure CGE_TestFairyLog(Message: PChar); cdecl; external;
{$endif}

class procedure TTestFairy.LogCallback(const Message: String);
begin
  {$ifdef CASTLE_IOS}
  CGE_TestFairyLog(PChar(Message));
  {$endif}
end;

end.
