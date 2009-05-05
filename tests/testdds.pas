{
  Copyright 2009 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

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
  TestImage('images' + PathDelim + 'mipmaps_no.dds', false);
  TestImage('images' + PathDelim + 'mipmaps_yes.dds', false);
  TestImage('images' + PathDelim + 'random3d.dds', true);
  TestImage('images' + PathDelim + 'random3d_with_mipmaps.dds', true);
end;

initialization
 RegisterTest(TTestDDS);
end.
