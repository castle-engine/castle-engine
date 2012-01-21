{
  Copyright 2009-2012 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

unit TestDDS;

interface

uses
  fpcunit, testutils, testregistry;

type
  TTestDDS = class(TTestCase)
  published
    procedure TestLoadSaveDDS;
    procedure TestLoadSaveS3TC;
  end;

implementation

uses SysUtils, VectorMath, Images, DDS, Classes;

procedure TTestDDS.TestLoadSaveDDS;

  procedure AssertImagesEqual(I1, I2: TEncodedImage);
  begin
    Assert(I1.ClassType = I2.ClassType);
    Assert(I1.Width     = I2.Width);
    Assert(I1.Height    = I2.Height);
    Assert(I1.Depth     = I2.Depth);
    if I1 is TImage then
    begin
      Assert(TImage(I1).PixelSize = TImage(I2).PixelSize);
      Assert(CompareMem(I1.RawPixels, I2.RawPixels,
        I1.Width * I1.Height * I1.Depth * TImage(I1).PixelSize));
    end;
  end;

  procedure AssertDDSEqual(DDS, DDS2: TDDSImage);
  var
    I: Integer;
  begin
    Assert(DDS.Images.Count = DDS2.Images.Count);
    for I := 0 to DDS.Images.Count - 1 do
      AssertImagesEqual(DDS.Images[I], DDS2.Images[I]);
  end;

  procedure TestImage(const FileName: string; const Is3d: boolean);
  var
    DDS, DDS2: TDDSImage;
    StreamNoFlatten, StreamFlatten: TMemoryStream;
    OldImagesCount: Cardinal;
  begin
    StreamNoFlatten := TMemoryStream.Create;
    StreamFlatten := TMemoryStream.Create;
    DDS := TDDSImage.Create;
    DDS2 := TDDSImage.Create;

    try
      DDS.LoadFromFile(FileName);

      { save to stream without flattening }
      DDS.SaveToStream(StreamNoFlatten);

      { flatten (making sure it actually did something: changed DDS.Images.Count) }
      OldImagesCount := DDS.Images.Count;
      DDS.Flatten3d;
      if Is3d then
        Assert(OldImagesCount < Cardinal(DDS.Images.Count)) else
        Assert(OldImagesCount = Cardinal(DDS.Images.Count));

      { save to stream with flattening }
      DDS.SaveToStream(StreamFlatten);

      { compare saved with and without flattening: saved image should
        be exactly the same. }
      StreamNoFlatten.Position := 0;
      StreamFlatten.Position := 0;

      { tests:
      StreamNoFlatten.SaveToFile('/tmp/StreamNoFlatten.dds');
      StreamFlatten.SaveToFile('/tmp/StreamFlatten.dds'); }

      Assert(StreamNoFlatten.Size = StreamFlatten.Size);
      Assert(CompareMem(StreamNoFlatten.Memory, StreamFlatten.Memory,
        StreamNoFlatten.Size));

      { read back (to get back to non-flattened version) }
      DDS.Close;
      DDS.LoadFromFile(FileName);

      { compare with what is loaded from StreamNoFlatten and StreamFlatten:
        they both should be equal to current DDS. }
      StreamNoFlatten.Position := 0;
      DDS2.LoadFromStream(StreamNoFlatten);
      AssertDDSEqual(DDS, DDS2);

      StreamFlatten.Position := 0;
      DDS2.LoadFromStream(StreamFlatten);
      AssertDDSEqual(DDS, DDS2);
    finally
      FreeAndNil(DDS);
      FreeAndNil(DDS2);
      FreeAndNil(StreamNoFlatten);
      FreeAndNil(StreamFlatten);
    end;
  end;

begin
  TestImage('data' + PathDelim + 'images' + PathDelim + 'mipmaps_no.dds', false);
  TestImage('data' + PathDelim + 'images' + PathDelim + 'mipmaps_yes.dds', false);
  TestImage('data' + PathDelim + 'images' + PathDelim + 'random3d.dds', true);
  TestImage('data' + PathDelim + 'images' + PathDelim + 'random3d_with_mipmaps.dds', true);
end;

procedure TTestDDS.TestLoadSaveS3TC;

  procedure TestImage(const FileName: string; const Is3d: boolean);
  var
    DDS: TDDSImage;
    Stream1, Stream2: TMemoryStream;
  begin
    Stream1 := TMemoryStream.Create;
    Stream2 := TMemoryStream.Create;
    DDS := TDDSImage.Create;
    try
      { load file into DDS }
      DDS.LoadFromFile(FileName);
      Assert(DDS.Images[0] is TS3TCImage);

      { save DDS into Stream1 }
      DDS.SaveToStream(Stream1);

      { load Stream1 into DDS }
      Stream1.Position := 0;
      DDS.LoadFromStream(Stream1);
      Assert(DDS.Images[0] is TS3TCImage);

      { save DDS into Stream2 }
      DDS.SaveToStream(Stream2);

      { Test that both save and load do appropriate vertical flip.
        If only one would do vertical flip, streams would differ.

        Note that we compare two streams obtained from saving DDS.
        We do *not* compare original FileName stream, as it's not guaranteed
        that we save it to exactly the same binary stream (for example,
        when saving we always add PitchOrLinearSize, while on load it may
        not be present). }

      Assert(Stream1.Size = Stream2.Size);
      Assert(CompareMem(Stream1.Memory, Stream2.Memory, Stream1.Size));
    finally
      FreeAndNil(DDS);
      FreeAndNil(Stream1);
      FreeAndNil(Stream2);
    end;
  end;

begin
  TestImage('data' + PathDelim + 'images' + PathDelim + 'metal_decal_dxt5.dds', true);
end;

initialization
 RegisterTest(TTestDDS);
end.
