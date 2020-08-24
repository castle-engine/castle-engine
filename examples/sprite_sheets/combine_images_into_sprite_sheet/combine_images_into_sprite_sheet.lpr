{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Generate sprite sheet from a sequence of images and xml file related
  xml file can be used e.g. with the sprite-sheet-to-x3d tool to load animation into TCastleScene.


  Use from the command-line:
  - 1st parameter is the image sequence URL (filename),
    using @counter(xxx),
    like https://castle-engine.io/x3d_implementation_texturing_extensions.php#section_ext_movie_from_image_sequence
    It can also point to a movie file, like avi or mpg, if you have ffmpeg installed.
  - 2nd parameter is output image name
  - 3rd parameter is the number of columns (how many frames in a row)
  - 4rd parameter is 1 or 0 (True or False).
    Use 1 to create the xml file that sets the (0, 0) coordinate of the spritesheet at the top left
    (required by sprite-sheet-to-x3d), use 0 to sets the (0,0) coordinate at the bottom left.

  For example:
  ./combine_images_into_sprite_sheet walking_@counter(1).png sprite_sheet_24fps.png 6 1

  See compile_and_run.sh script to real-life example.
}

program combine_images_into_sprite_sheet;

uses SysUtils, Math, Classes,
  CastleParameters, CastleImages, CastleVideos, CastleUtils, CastleVectors;

var
  InputUrl, OutputUrl: String;
  InputVideo: TVideo;
  OutputImage: TRGBAlphaImage;
  Columns, FinalColumns, Rows, I, X, Y: Cardinal;
  ReverseY: boolean;
  XMLFileList: TStringList;
  XMLFileName: String;

begin
  Parameters.CheckHigh(4);
  InputUrl := Parameters[1];
  OutputUrl := Parameters[2];
  Columns := StrToInt(Parameters[3]);
  ReverseY := StrToBool(Parameters[4]);

  try
    XMLFileName := ExtractFileName(OutputUrl);
    XMLFileList := TStringList.Create;
    XMLFileList.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XMLFileList.Add('<TextureAtlas imagePath="' + XMLFileName + '">');
    InputVideo := TVideo.Create;
    InputVideo.LoadFromFile(InputUrl);

    Writeln(Format('Input: Image sequence with %d images, size %d x %d',
      [InputVideo.Count, InputVideo.Width, InputVideo.Height]));

    FinalColumns := Min(Columns, InputVideo.Count);
    Rows := CeilDiv(InputVideo.Count, Columns);
    OutputImage := TRGBAlphaImage.Create(
      FinalColumns * InputVideo.Width,
      Rows * InputVideo.Height);
    OutputImage.Clear(Vector4Byte(0, 0, 0, 0)); // transparent black

    Writeln(Format('Output: Sprite sheet image with %d columns, %d rows, size %d x %d',
      [FinalColumns, Rows, OutputImage.Width, OutputImage.Height]));

    for I := 0 to InputVideo.Count - 1 do
    begin
      X := (I mod Columns) * InputVideo.Width;
      //Y := (I div Columns) * InputVideo.Height;
      // Lay out image rows from the top:
      Y := OutputImage.Height - (I div Columns + 1) * InputVideo.Height;
      InputVideo.Items[I].DrawTo(OutputImage, X, Y, dmOverwrite);
      if ReverseY then
        Y := ((I div Columns + 1) * InputVideo.Height) - InputVideo.Height;
      XMLFileList.add('<SubTexture name="' + ExtractFileName(InputVideo.Items[I].URL) +
                      '" x="' + X.ToString +
                      '" y="' + Y.ToString +
                      '" width="' + InputVideo.Items[I].Width.ToString +
                      '" height="' + InputVideo.Items[I].Height.ToString + '"/>');
    end;
    XMLFileList.Add('</TextureAtlas>');
    XMLFileList.SaveToFile(ExtractFilePath(OutputUrl) + ChangeFileExt(XMLFileName, '') + '.xml');
    SaveImage(OutputImage, OutputUrl);
  finally
    FreeAndNil(InputVideo);
    FreeAndNil(OutputImage);
    if Assigned (XMLFileList) then
      FreeAndNil(XMLFileList);
  end;
end.


