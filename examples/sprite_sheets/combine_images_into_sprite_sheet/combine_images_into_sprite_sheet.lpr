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

{ Generate sprite sheet from a sequence of images, it can contains more then one animation
  (Images must all have the same size and same name and must be renamed sequentially as follows:
  img_0, img_1, img_2 ... img_29
  or
  img_1, img_2, img_3 ... img_30
  or
  img_00 ... img_29
  or
  img_001 ... img_030
  and so on
  First image must always begin with 0 or 1
  If the sprite sheet is to contain multiple animations, they must still be renamed in the same way
  )
  Generate .xml file related
  (.xml file can be used e.g. with the sprite-sheet-to-x3d tool to load animation into TCastleScene)
  .xml file can have the coordinate (0,0) set at the top left or bottom left of the spritesheet
  (according to your needs)

  Use from the command-line:
  - 1st parameter is the first image sequence URL (filename),
    It can also point to a movie file, like avi or mpg, if you have ffmpeg installed.

  - 2nd parameter is 0 or a string containing the animation parameters
  For example:
  Type 0 if your spritesheet contains only one animation
  Type a string as below if spritesheet contains multiple animations:
  walk,0,9-run,10,19-idle,20,29
  or
  walk,1,10-run,11,20-idle,21,30
  where the first parameter will be the name of the animation, the second and third parameter
  the index of the first and last image of the animation
  Each animation must be separated by the "-"

  - 3rd parameter is the is the padding you used
  For example:
  Type 1 if the first image uses the form _0 or _1
  Type 3 if the first image uses the form _000 or _001

  - 4nd parameter is output image name

  - 5rd parameter is the number of columns (how many frames in a row)

  - 6rd parameter is 1 or 0 (True or False).
    Use 1 to create the xml file that sets the (0, 0) coordinate of the spritesheet at the top left
    (required by sprite-sheet-to-x3d), use 0 to sets the (0,0) coordinate at the bottom left.

  Some examples:
  (under Windows use .\)
  One animation, "walking_1.png" as the first image:
  ./combine_images_into_sprite_sheet walking_1.png 0 1 sprite_sheet_24fps.png 6 1
  Three animation, "left_00.png" as first image
  ./combine_images_into_sprite_sheet ../sprite/left_00.png walk,0,9-run,10,19-idle,20,29 2 ../sprite/sprite_sheet_10fps.png 3 1

  See compile_and_run.sh script to real-life example.
}

program combine_images_into_sprite_sheet;

uses SysUtils, Math, Classes,
  CastleParameters, CastleImages, CastleVideos, CastleUtils, CastleVectors;

var
  InputUrl, AnimOpt, Ext, OutputUrl, AnimName, AnimNameIndex: String;
  FilePath, FileName, FileExt, FileNameOutExt: string;
  InputVideo: TVideo;
  OutputImage: TRGBAlphaImage;
  Columns, Padding, FinalColumns, Rows, I, X, Y, K, A, H, from, unto: Cardinal;
  ReverseY, SingleElement, StartFromZero: boolean;
  XMLFileList: TStringList;
  XMLFileName: String;
  ElementList, AnimList, CheckPadding: TStringArray;

begin
  Parameters.CheckHigh(6);
  InputUrl := Parameters[1];
  AnimOpt := Parameters[2];
  Padding := StrToInt(Parameters[3]);
  OutputUrl := Parameters[4];
  Columns := StrToInt(Parameters[5]);
  ReverseY := StrToBool(Parameters[6]);

  try
    FilePath := ExtractFilePath(InputUrl);
    FileName := ExtractFileName(InputUrl);
    FileExt := ExtractFileExt(InputUrl);
    FileNameOutExt := ChangeFileExt(FileName, '');
    CheckPadding := FileNameOutExt.Split('_');
    if StrToInt(CheckPadding[1]) = 0 then
      StartFromZero := true;
    if AnimOpt = '0' then
     SingleElement := true
    else
      ElementList := AnimOpt.Split('-');
    XMLFileName := ExtractFileName(OutputUrl);
    XMLFileList := TStringList.Create;
    XMLFileList.Add('<?xml version="1.0" encoding="UTF-8"?>');
    XMLFileList.Add('<TextureAtlas imagePath="' + XMLFileName + '">');
    InputVideo := TVideo.Create;
    InputUrl := FilePath + CheckPadding[0] + '_@counter(' + inttostr(Padding) + ')' + FileExt;
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
    Writeln('Output: XML file created');

    for I := 0 to InputVideo.Count - 1 do
    begin
      X := (I mod Columns) * InputVideo.Width;
      //Y := (I div Columns) * InputVideo.Height;
      // Lay out image rows from the top:
      Y := OutputImage.Height - (I div Columns + 1) * InputVideo.Height;
      InputVideo.Items[I].DrawTo(OutputImage, X, Y, dmOverwrite);
      if ReverseY then
        Y := ((I div Columns + 1) * InputVideo.Height) - InputVideo.Height;
      if SingleElement then
        AnimName := ExtractFileName(InputVideo.Items[I].URL)
      else
        begin
          AnimName := 'Index_Not_Found';
          for K := 0 to Length(ElementList)-1 do
          begin
            AnimList := ElementList[K].Split(',');
            from := strtoint(AnimList[1]);
            unto := strtoint(AnimList[2]);
            if StartFromZero then
              H := I
            else
              H := I + 1;
            if ((H >= from) and (H <= unto)) then
            begin
              AnimNameIndex := IntToStr(H);
              if length(AnimNameIndex) < Padding then
              begin
                for A := length(AnimNameIndex) to Padding-1 do
                  AnimNameIndex := '0' + AnimNameIndex;
              end;
              AnimName := AnimList[0] + '_' + AnimNameIndex + FileExt;
              break;
            end;
          end;
        end;
        XMLFileList.add('<SubTexture name="' + AnimName +
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




