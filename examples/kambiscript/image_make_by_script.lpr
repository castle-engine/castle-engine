{ Simple program that uses KambiScript to create an image
  [http://vrmlengine.sourceforge.net/kambi_script.php].
  KambiScript allows you to load an existing image, or start from scratch,
  and perform basic image processing. After the script executed,
  we will save image to file.

  Run with two parameters: script filename, and output image filename.
  For example
    image_make_by_script mkimage_gradient.kscript new_image.png

  Within the script, you have variables:
  - result, helper_img (type image),
  - i, j, k, l are ints,
  - x, y, z, are floats,
  - v2, v3, v3 are single-precision vectors of 2,3,4 elements.

  You should make a function named "main" in the script, this will be executed.

  See example scripts mkimage_.*kscript in this directory.
}

uses SysUtils, KambiUtils, KambiFilesUtils, KambiStringUtils, Images,
  KambiClassUtils,
  KambiScript, KambiScriptParser, KambiScriptVectors, KambiScriptImages,
  KambiWarnings;

var
  Vars: TKamScriptValuesList;
  Prog: TKamScriptProgram;
begin
  Parameters.CheckHigh(2);

  OnWarning := @OnWarningWrite;

  Vars := TKamScriptValuesList.Create(true);
  try
    Vars.Add(TKamScriptImage.Create(true));
    Vars.Last.Name := 'result';
    Vars.Add(TKamScriptImage.Create(true));
    Vars.Last.Name := 'helper_img';

    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Last.Name := 'i';
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Last.Name := 'j';
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Last.Name := 'k';
    Vars.Add(TKamScriptInteger.Create(true));
    Vars.Last.Name := 'l';

    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Last.Name := 'x';
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Last.Name := 'y';
    Vars.Add(TKamScriptFloat.Create(true));
    Vars.Last.Name := 'z';

    Vars.Add(TKamScriptVec2f.Create(true));
    Vars.Last.Name := 'v2';
    Vars.Add(TKamScriptVec3f.Create(true));
    Vars.Last.Name := 'v3';
    Vars.Add(TKamScriptVec4f.Create(true));
    Vars.Last.Name := 'v4';

    Prog := ParseProgram(FileToString(Parameters[1]), Vars);
    try
      Prog.Environment.WWWBasePath := InclPathDelim(GetCurrentDir);
      Prog.ExecuteFunction('main', []);
      SaveImage(TKamScriptImage(Vars[0]).Value, Parameters[2]);
    finally FreeAndNil(Prog) end;
  finally FreeAndNil(Vars) end;
end.
