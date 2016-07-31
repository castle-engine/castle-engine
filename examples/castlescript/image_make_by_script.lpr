{ Simple program that uses CastleScript to create an image
  [http://castle-engine.sourceforge.net/caste_script.php].
  CastleScript allows you to load an existing image, or start from scratch,
  and perform basic image processing. After the script executed,
  we will save image to file.

  Run with two parameters: script URL (usually just a filename),
  and output image URL (usualy just a filename).
  For example
    image_make_by_script mkimage_gradient.castlescript new_image.png

  Within the script, you have variables:
  - result, helper_img (type image),
  - i, j, k, l are ints,
  - x, y, z, are floats,
  - v2, v3, v3 are single-precision vectors of 2,3,4 elements.

  You should make a function named "main" in the script, this will be executed.

  See example scripts mkimage_*.castlescript in this directory.
}

uses SysUtils, CastleUtils, CastleFilesUtils, CastleStringUtils, CastleImages,
  CastleClassUtils, CastleParameters, CastleURIUtils,
  CastleScript, CastleScriptParser, CastleScriptVectors, CastleScriptImages,
  CastleLog, CastleApplicationProperties;

var
  Vars: TCasScriptValueList;
  Prog: TCasScriptProgram;
begin
  Parameters.CheckHigh(2);

  ApplicationProperties.OnWarning.Add(@ApplicationProperties.WriteWarningOnConsole);

  Vars := TCasScriptValueList.Create(true);
  try
    Vars.Add(TCasScriptImage.Create(true));
    Vars.Last.Name := 'result';
    Vars.Add(TCasScriptImage.Create(true));
    Vars.Last.Name := 'helper_img';

    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Last.Name := 'i';
    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Last.Name := 'j';
    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Last.Name := 'k';
    Vars.Add(TCasScriptInteger.Create(true));
    Vars.Last.Name := 'l';

    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Last.Name := 'x';
    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Last.Name := 'y';
    Vars.Add(TCasScriptFloat.Create(true));
    Vars.Last.Name := 'z';

    Vars.Add(TCasScriptVec2f.Create(true));
    Vars.Last.Name := 'v2';
    Vars.Add(TCasScriptVec3f.Create(true));
    Vars.Last.Name := 'v3';
    Vars.Add(TCasScriptVec4f.Create(true));
    Vars.Last.Name := 'v4';

    Prog := ParseProgram(FileToString(Parameters[1]), Vars);
    try
      Prog.Environment.BaseUrl := URICurrentPath;
      Prog.ExecuteFunction('main', []);
      SaveImage(TCasScriptImage(Vars[0]).Value, Parameters[2]);
    finally FreeAndNil(Prog) end;
  finally FreeAndNil(Vars) end;
end.
