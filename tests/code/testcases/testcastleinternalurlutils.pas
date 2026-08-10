{
  Copyright 2025-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleInternalUrlUtils unit. }
unit TestCastleInternalUrlUtils;

interface

uses Classes, SysUtils,
  CastleTester;

type
  TTestCastleInternalUrlUtils = class(TCastleTestCase)
  published
    procedure TestGetBaseNameFromUrl;
    procedure TestStripNumericSuffix;
  end;

implementation

uses CastleInternalUrlUtils, CastleLog, CastleClassUtils;

type
  TCastleTransform = class(TCastleComponent)
  end;

procedure TTestCastleInternalUrlUtils.TestGetBaseNameFromUrl;
var
  Parent: TComponent;
  NewComponent: TComponent;
begin
  AssertEquals('FooBar123', GetBaseNameFromUrl('castle-data:/foo-bar_123.gltf'));

  AssertEquals('', GetUrlParentName('castle-data:/foo-bar_123.gltf'));
  AssertEquals('', GetUrlParentName('castle-data:/'));
  AssertEquals('xyz', GetUrlParentName('castle-data:/xyz/foo-bar_123.gltf'));
  AssertEquals('xyz-123abcSOMETHING', GetUrlParentName('castle-data:/xyz-123abcSOMETHING/foo-bar_123.gltf'));
  AssertEquals('xyz', GetUrlParentName('castle-data:/xyz-123abc/foo-bar_123.gltf'));

  Parent := TComponent.Create(nil);
  try
    NewComponent := TComponent.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent);
    AssertEquals('Component1', NewComponent.Name);

    NewComponent := TComponent.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent);
    AssertEquals('Component2', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent);
    AssertEquals('Transform1', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent);
    AssertEquals('Transform2', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent,
      'Scene' + GetBaseNameFromUrl('castle-data:/something/blah_blahXyz--123_foo.wrl'));
    AssertEquals('SceneBlahBlahXyz123Foo1', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent,
      'Scene' + GetBaseNameFromUrl('castle-data:/something/blah_blahXyz--123_foo.wrl'));
    AssertEquals('SceneBlahBlahXyz123Foo2', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent,
      'Scene' + GetBaseNameFromUrl('castle-data:/sketchfab/blah_blahXyz--123_foo-1231083912abcdef/scene.gltf'));
    AssertEquals('SceneBlahBlahXyz123Foo3', NewComponent.Name);

    NewComponent := TCastleTransform.Create(Parent);
    AssertEquals('', NewComponent.Name);
    NewComponent.Name := ProposeComponentName(TComponentClass(NewComponent.ClassType), Parent,
      'Scene' + GetBaseNameFromUrl('castle-data:/something/.wrl'));
    AssertEquals('SceneWrl1', NewComponent.Name);

  finally FreeAndNil(Parent) end;
end;

procedure TTestCastleInternalUrlUtils.TestStripNumericSuffix;
begin
  AssertEquals('1', StripNumericSuffix('1'));
  AssertEquals('a', StripNumericSuffix('a1'));
  AssertEquals('a', StripNumericSuffix('a19812'));
  AssertEquals('foo1a', StripNumericSuffix('foo1a1'));
  AssertEquals('foo1a', StripNumericSuffix('foo1a19812'));
end;

initialization
  RegisterTest(TTestCastleInternalUrlUtils);
end.


