// -*- compile-command: "./test_single_testcase.sh TTestCastleCompositeImage" -*-
{
  Copyright 2009-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Test CastleCompositeImage unit. }
unit TestCastleCompositeImage;

interface

uses
  {$ifndef CASTLE_TESTER}FpcUnit, TestUtils, TestRegistry{$else}CastleTester{$endif};

type
  TTestCastleCompositeImage = class({$ifndef CASTLE_TESTER}TTestCase{$else}TCastleTestCase{$endif})
  published
    procedure TestLoadSave;
    procedure TestLoadSaveS3TC;
  end;

implementation

uses SysUtils, CastleVectors, CastleImages, CastleInternalCompositeImage, Classes;

procedure TTestCastleCompositeImage.TestLoadSave;

  procedure AssertImagesEqual(I1, I2: TEncodedImage);
  begin
    AssertTrue(I1.ClassType = I2.ClassType);
    AssertTrue(I1.Width     = I2.Width);
    AssertTrue(I1.Height    = I2.Height);
    AssertTrue(I1.Depth     = I2.Depth);
    if I1 is TCastleImage then
    begin
      AssertTrue(TCastleImage(I1).PixelSize = TCastleImage(I2).PixelSize);
      AssertTrue(CompareMem(I1.RawPixels, I2.RawPixels,
        I1.Width * I1.Height * I1.Depth * TCastleImage(I1).PixelSize));
    end;
  end;

  procedure AssertCompositeEqual(Composite, Composite2: TCompositeImage);
  var
    I: Integer;
  begin
    AssertTrue(Composite.Images.Count = Composite2.Images.Count);
    for I := 0 to Composite.Images.Count - 1 do
      AssertImagesEqual(Composite.Images[I], Composite2.Images[I]);
  end;

  procedure TestImage(const FileName: string; const Is3d: boolean);
  var
    Composite, Composite2: TCompositeImage;
    StreamNoFlatten, StreamFlatten: TMemoryStream;
    OldImagesCount: Cardinal;
  begin
    StreamNoFlatten := TMemoryStream.Create;
    StreamFlatten := TMemoryStream.Create;
    Composite := TCompositeImage.Create;
    Composite2 := TCompositeImage.Create;

    try
      Composite.LoadFromFile(FileName);

      { save to stream without flattening }
      Composite.SaveToStream(StreamNoFlatten, 'image/x-dds');

      { flatten (making sure it actually did something: changed Composite.Images.Count) }
      OldImagesCount := Composite.Images.Count;
      Composite.Flatten3d;
      if Is3d then
        AssertTrue(OldImagesCount < Cardinal(Composite.Images.Count)) else
        AssertTrue(OldImagesCount = Cardinal(Composite.Images.Count));

      { save to stream with flattening }
      Composite.SaveToStream(StreamFlatten, 'image/x-dds');

      { compare saved with and without flattening: saved image should
        be exactly the same. }
      StreamNoFlatten.Position := 0;
      StreamFlatten.Position := 0;

      { tests:
      StreamNoFlatten.SaveToFile('/tmp/StreamNoFlatten.dds');
      StreamFlatten.SaveToFile('/tmp/StreamFlatten.dds'); }

      AssertTrue(StreamNoFlatten.Size = StreamFlatten.Size);
      AssertTrue(CompareMem(StreamNoFlatten.Memory, StreamFlatten.Memory,
        StreamNoFlatten.Size));

      { read back (to get back to non-flattened version) }
      Composite.Close;
      Composite.LoadFromFile(FileName);

      { compare with what is loaded from StreamNoFlatten and StreamFlatten:
        they both should be equal to current Composite. }
      StreamNoFlatten.Position := 0;
      Composite2.LoadFromStream(StreamNoFlatten, '', 'image/x-dds');
      AssertCompositeEqual(Composite, Composite2);

      StreamFlatten.Position := 0;
      Composite2.LoadFromStream(StreamFlatten, '', 'image/x-dds');
      AssertCompositeEqual(Composite, Composite2);
    finally
      FreeAndNil(Composite);
      FreeAndNil(Composite2);
      FreeAndNil(StreamNoFlatten);
      FreeAndNil(StreamFlatten);
    end;
  end;

begin
  (*TODO: This fails within CGE Jenkins ( https://jenkins.castle-engine.io/ )
    with Access Violation. The reason is unknown as I cannot reproduce it easily now:

    - I was not able to reproduce the crash with *the same* FPC revision
      on regular Linux/x86_64, and even using *the same* Docker image
      as used by Jenkins.
      That is, doing inside Docker image
      (see https://castle-engine.io/docker )

        source /usr/local/fpclazarus/bin/setup.sh trunk && make clean tools
        source /usr/local/fpclazarus/bin/setup.sh trunk && export PATH="${PATH}:${CASTLE_ENGINE_PATH}/tools/build-tool/" && make tests

      ... works, without any bug (all tests pass).

    - Also, testing saving/loading these DDS files using
      https://github.com/castle-engine/castle-view-image
      works OK with the same FPC version.

    But Jenkins shows it fails.
  *)
  {$ifdef FPC}
    {$ifndef VER3_0}
      {$ifndef VER3_1}
        Exit;
      {$endif}
    {$endif}
  {$endif}

  TestImage('castle-data:/images/mipmaps_no.dds', false);
  TestImage('castle-data:/images/mipmaps_yes.dds', false);
  TestImage('castle-data:/images/random3d.dds', true);
  TestImage('castle-data:/images/random3d_with_mipmaps.dds', true);
end;

procedure TTestCastleCompositeImage.TestLoadSaveS3TC;

  procedure TestImage(const FileName: string; const Is3d: boolean);
  var
    Composite: TCompositeImage;
    Stream1, Stream2: TMemoryStream;
  begin
    Stream1 := TMemoryStream.Create;
    Stream2 := TMemoryStream.Create;
    Composite := TCompositeImage.Create;
    try
      { load file into Composite }
      Composite.LoadFromFile(FileName);
      AssertTrue(Composite.Images[0] is TGPUCompressedImage);

      { save Composite into Stream1 }
      Composite.SaveToStream(Stream1, 'image/x-dds');

      { load Stream1 into Composite }
      Stream1.Position := 0;
      Composite.LoadFromStream(Stream1, '', 'image/x-dds');
      AssertTrue(Composite.Images[0] is TGPUCompressedImage);

      { save Composite into Stream2 }
      Composite.SaveToStream(Stream2, 'image/x-dds');

      { Test that both save and load do appropriate vertical flip.
        If only one would do vertical flip, streams would differ.

        Note that we compare two streams obtained from saving Composite.
        We do *not* compare original FileName stream, as it's not guaranteed
        that we save it to exactly the same binary stream (for example,
        when saving we always add PitchOrLinearSize, while on load it may
        not be present). }

      AssertTrue(Stream1.Size = Stream2.Size);
      AssertTrue(CompareMem(Stream1.Memory, Stream2.Memory, Stream1.Size));
    finally
      FreeAndNil(Composite);
      FreeAndNil(Stream1);
      FreeAndNil(Stream2);
    end;
  end;

begin
  TestImage('castle-data:/images/metal_decal_dxt5.dds', true);
end;

initialization
  RegisterTest(TTestCastleCompositeImage);
end.
