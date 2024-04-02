{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test for CastleRendererInternalShader unit. }
unit TestCastleRendererInternalShader;

interface

uses
  Classes, SysUtils,
  CastleTester;

type
  TTestCastleRendererInternalShader = class(TCastleTestCase)
  published
    procedure TestFindPlugName;
  end;

implementation

uses CastleUtils, CastleRendererInternalShader;

procedure TTestCastleRendererInternalShader.TestFindPlugName;
var
  PlugName, DeclaredParameters: String;
begin
  PlugName := FindPlugName(NL +
    'void PLUG_main_texture_apply ( inout vec4 fcol, const in vec3 unused )' + NL +
    '{' + NL +
    '}', DeclaredParameters);
  AssertEquals('main_texture_apply', PlugName);
  AssertEquals('( inout vec4 fcol, const in vec3 unused )', DeclaredParameters);

  PlugName := FindPlugName(NL +
    'void PLUG_main_texture_apply( inout vec4 fcol, const in vec3 unused )' + NL +
    '{' + NL +
    '}', DeclaredParameters);
  AssertEquals('main_texture_apply', PlugName);
  AssertEquals('( inout vec4 fcol, const in vec3 unused )', DeclaredParameters);

  // Make sure PLUG_xxx not followed by ( is not recognized
  PlugName := FindPlugName(NL +
    '/* void PLUG_main_texture_apply bla blah */', DeclaredParameters);
  AssertEquals('', PlugName);
end;

initialization
  RegisterTest(TTestCastleRendererInternalShader);
end.