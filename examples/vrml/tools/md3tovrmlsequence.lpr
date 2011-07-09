{
  Copyright 2007-2010 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Convert MD3 model to a sequence of VRML models.

  This way we convert MD3 animation to a precalculated animation
  in Kanim format, see
  [http://vrmlengine.sourceforge.net/kanim_format.php].
  This may be played by
  [http://vrmlengine.sourceforge.net/view3dscene.php] (although
  view3dscene can also play MD3 directly) or
  kambi_vrml_game_engine/examples/vrml/demo_animation example.

  For MD3 file named xxx.md3, this will generate VRML files like
  xxx_0000.wrl, xxx_0001.wrl, xxx_0002.wrl etc. and the main file
  to play whole animation xxx.kanim. }
program md3tovrmlsequence;

uses SysUtils, KambiUtils, KambiFilesUtils,
  VRMLNodes, Object3DMD3, Object3DAsVRML;

var
  Md3: TObject3DMD3;
  WWWBasePath: string;
  FileName, OutputFileName, KAnimFileName: string;
  Node: TVRMLNode;
  I: Integer;
  KAnimFile: TextFile;
begin
  Parameters.CheckHigh(1);
  FileName := Parameters[1];

  WWWBasePath := ExtractFilePath(ExpandFilename(FileName));
  Md3 := TObject3DMD3.Create(FileName);
  try

    { begin kanim file }
    KAnimFileName := DeleteFileExt(FileName) + '.kanim';
    SafeRewrite(KAnimFile, KAnimFileName);
    try
      Writeln(KAnimFile, '<?xml version="1.0"?>');
      Writeln(KAnimFile, '<animation>');

      { handle each MD3 frame }
      for I := 0 to Md3.FramesCount - 1 do
      begin
        Node := LoadMD3Frame(Md3, I, WWWBasePath);
        try
          OutputFileName := DeleteFileExt(FileName) + Format('_%.6d.wrl', [I]);
          SaveVRML(Node, OutputFileName, 'md3tovrmlsequence', '', xeClassic);
          Writeln('Wrote ', OutputFileName);
          Writeln(KAnimFile, '  <frame file_name="',
            ExtractFileName(OutputFileName), '" time="', I / 30, '" />');
        finally FreeAndNil(Node) end;
      end;

      { end kanim file }
      Writeln(KAnimFile, '</animation>');
    finally Closefile(KAnimFile) end;
    Writeln('Wrote ', KAnimFileName);

  finally FreeAndNil(Md3) end;
end.
