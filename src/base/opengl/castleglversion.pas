{
  Copyright 2001-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Checking OpenGL version, vendors and such (GLVersion, GLUVersion).
  These should be initialized by calling GLInformationInitialize,
  which is done automatically when opening OpenGL context using
  TCastleWindowCustom or TCastleControlCustom.
}
unit CastleGLVersion;

{$I castleconf.inc}

interface

type
  { OpenGL libraries (core OpenGL or GLU) version information.

    As obtained from glGetString(GL_VERSION)
    or gluGetString(GLU_VERSION), also by glGetString(GL_VENDOR).

    This is usually created by CastleGLUtils.GLInformationInitialize. }
  TGenericGLVersion = class
  public
    constructor Create(const VersionString: string);
  public
    { Required (every OpenGL implemenetation has them)
      major and minor numbers.
      @groupBegin }
    Major: Integer;
    Minor: Integer;
    { @groupEnd }

    { Release is the optional release number (check ReleaseExists first).
      @groupBegin }
    ReleaseExists: boolean;
    Release: Integer;
    { @groupEnd }

    { VendorInfo is whatever vendor-specific information was placed
      inside VersionString, after the
      major_number.minor_number.release_number. It never has any whitespace
      at the beginning (we trim it when initializing). }
    VendorInfo: string;

    function AtLeast(AMajor, AMinor: Integer): boolean;
  end;

  TGLVendorType = (
    gvUnknown,
    { ATI GPU with ATI drivers. }
    gvATI,
    { NVidia GPU with NVidia drivers. }
    gvNvidia,
    { Intel GPU with Intel drivers. }
    gvIntel,
    { Imagination Technologies (PowerVR) GPU, common on mobile devices. }
    gvImaginationTechnologies
  );

  TGLVersion = class(TGenericGLVersion)
  private
    FVendor: string;
    FVendorType: TGLVendorType;
    FRenderer: string;
    FFglrx: boolean;
    FMesa: boolean;
    FVendorMajor: Integer;
    FVendorMinor: Integer;
    FVendorRelease: Integer;
    FBuggyGenerateMipmap: boolean;
    FBuggyGenerateCubeMap: boolean;
    FBuggyFBOCubeMap: boolean;
    FBuggyLightModelTwoSide: boolean;
    FBuggyLightModelTwoSideMessage: string;
    FBuggyVBO: boolean;
    FBuggyShaderShadowMap: boolean;
    FBuggyGLSLConstStruct: boolean;
    FBuggyFBOMultiSampling: boolean;
    FBuggySwapNonStandardViewport: boolean;
    FBuggyDepth32: boolean;
    FBuggyGLSLFrontFacing: boolean;
    FBuggyGLSLReadVarying: boolean;
  public
    constructor Create(const VersionString, AVendor, ARenderer: string);

    { Vendor that created the OpenGL implemenetation.
      This is just glGetString(GL_VENDOR). }
    property Vendor: string read FVendor;

    { Vendor type, derived from @link(Vendor) string. }
    property VendorType: TGLVendorType read FVendorType;

    { Renderer (GPU model, or software method used for rendering) of the OpenGL.
      This is just glGetString(GL_RENDERER). }
    property Renderer: string read FRenderer;

    { Are we using Mesa (http://mesa3d.org/).
      Detected using VendorSpecific information. }
    property Mesa: boolean read FMesa;

    { Vendor-specific drivers version.
      Right now this is detected for Mesa and Intel.
      @groupBegin }
    property VendorMajor: Integer read FVendorMajor;
    property VendorMinor: Integer read FVendorMinor;
    property VendorRelease: Integer read FVendorRelease;
    { @groupEnd }

    { ATI GPU with ATI drivers on Linux. }
    property Fglrx: boolean read FFglrx;

    { Buggy glGenerateMipmapEXT (Mesa and Intel(Windows) bug).

      This was observed with software (no direct) rendering with
      7.0.2 (segfaults) and 7.2.? (makes X crashing; sweet).
      With Mesa 7.5.1 (but tested only with radeon and radeonhd,
      so possibly it's not really related to Mesa version! Reports welcome)
      no problems. }
    property BuggyGenerateMipmap: boolean read FBuggyGenerateMipmap;

    { Buggy generation of cube maps on FBO (Intel(Windows) bug).

      Symptoms: Parts of the cube map texture are uninitialized (left magenta).
      Reproducible with view3dscene on
      demo_models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv .
      Using separate FBO for each cube map face doesn't help (actually,
      makes it worse, there's more magenta),
      so it's not about being unable to do RenderToTexture.SetTexture multiple times.

      Observed, and this workaround is needed, at least on:
      @unorderedList(
        @item Version string: 2.1.0 - Build 8.15.10.2104
        @item Vendor: Intel
        @item Renderer: Intel(R) HD Graphics
      )
    }
    property BuggyFBOCubeMap: boolean read FBuggyFBOCubeMap;

    { Buggy generation of cube maps at all (Intel(Windows) bug).

      Symptoms: Parts of the cube map texture are uninitialized (left magenta).
      Reproducible with view3dscene on
      demo_models/cube_environment_mapping/cubemap_generated_in_dynamic_world.x3dv .
      This is worse then BuggyFBOCubeMap, magenta is always seen at positiveX part
      of the cube map.

      Observed, and this workaround is needed, at least on:
      @unorderedList(
        @item Version string: 3.3.0 - Build 8.15.10.2778
        @item Vendor: Intel
        @item Renderer: Intel(R) HD Graphics 4000
      )
    }
    property BuggyGenerateCubeMap: boolean read FBuggyGenerateCubeMap;

    { Buggy GL_LIGHT_MODEL_TWO_SIDE = GL_TRUE behavior (ATI(Linux) bug).
      See [https://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=14] }
    property BuggyLightModelTwoSide: boolean read FBuggyLightModelTwoSide;
    property BuggyLightModelTwoSideMessage: string read FBuggyLightModelTwoSideMessage;

    { Buggy VBO (Intel(Windows) bug). }
    property BuggyVBO: boolean read FBuggyVBO;

    { Buggy shadow2DProj in some situations (ATI(Linux) bug). }
    property BuggyShaderShadowMap: boolean read FBuggyShaderShadowMap;

    { Buggy GLSL @code("const in gl_Xxx") (NVidia bug).
      Segfaults at glCompileShader[ARB] on GLSL declarations like
      @code("const in gl_MaterialParameters material").
      Affects some NVidia drivers on Linux (like version 295.49
      in Debian testing on 2012-06-02). }
    property BuggyGLSLConstStruct: boolean read FBuggyGLSLConstStruct;

    { Buggy (looks like wireframe) FBO rendering to
      the multi-sampling texture (ATI(Windows) and Intel(Windows) bug).
      This makes our screen effects broken on multi-sampled contexts. }
    property BuggyFBOMultiSampling: boolean read FBuggyFBOMultiSampling;

    { Buggy swap buffers when glViewport does not contain
      whole window (ATI(Linux) bug). }
    property BuggySwapNonStandardViewport: boolean
      read FBuggySwapNonStandardViewport;

    { Buggy 32-bit depth buffer, 24-bit depth buffer works Ok
      (Mesa on ATI (Linux) bug). }
    property BuggyDepth32: boolean read FBuggyDepth32;

    { Buggy gl_FrontFacing in GLSL. Observed on Mesa 10.x with OpenGL 3.x
      (see implemenetation for details where it's observed / not observed).

      Note that avoiding gl_FrontFacing (that seems to always has inverted value?)
      is only a part of the workaround for these GPUs.
      The other is to not render back faces, since it seems
      that normals are *always* oriented to point to the light
      (so, if you don't look at gl_FrontFacing, you are lit from *both* sides).
      So enable backface culling, or just be prepared that backfaces may be
      incorrectly light. }
    property BuggyGLSLFrontFacing: boolean read FBuggyGLSLFrontFacing;

    { Do not read varying values in vertex shader, treat them as write-only. }
    property BuggyGLSLReadVarying: boolean read FBuggyGLSLReadVarying;
  end;

var
  { Core OpenGL version information.
    This is usually created by CastleGLUtils.GLInformationInitialize. }
  GLVersion: TGLVersion;

  {$ifndef OpenGLES}
  { GLU version information.
    This is usually created by CastleGLUtils.GLInformationInitialize. }
  GLUVersion: TGenericGLVersion;
  {$endif}

function VendorTypeToStr(const VendorType: TGLVendorType): string;

implementation

uses SysUtils, CastleStringUtils, CastleUtils, CastleLog;

{ Skip whitespace. Moves I to next index after whitespace. }
procedure ParseWhiteSpaces(const S: string; var I: Integer);
begin
  while SCharIs(S, I, WhiteSpaces) do Inc(I);
end;

{ Parse next non-white-space part of string, assuming we stand on it's start.
  Returns empty string if none (string ended).
  Moves I to next index after this part. }
function ParseString(const S: string; var I: Integer): string;
var
  Start: Integer;
begin
  if (I <= Length(S)) and not (S[I] in WhiteSpaces) then
  begin
    Start := I;
    repeat Inc(I) until not ((I <= Length(S)) and not (S[I] in WhiteSpaces));
    Result := CopyPos(S, Start, I - 1);
  end else
    Result := '';
end;

{ Parse next number in the string.
  Moves I to next character.
  Sets out Number on success, returns false on failure (no number,
  or invalid int format). }
function ParseNumber(const S: string; var I: Integer; out Number: Integer): boolean;
const
  Digits = ['0'..'9'];
var
  Start: Integer;
begin
  if not SCharIs(S, I, Digits) then
    Exit(false);
  Start := I;
  while SCharIs(S, I, Digits) do Inc(I);
  Result := TryStrToInt(CopyPos(S, Start, I - 1), Number);
end;

{ TGenericGLVersion ---------------------------------------------------------- }

constructor TGenericGLVersion.Create(const VersionString: string);
var
  I: Integer;
begin
  inherited Create;

  try
    I := 1;

    { Note: we allow some whitespace that is not allowed by OpenGL/GLU
      spec. That's because we try hard to work correctly even with
      broken GL_VERSION / GLU_VERSION strings. }

    ParseWhiteSpaces(VersionString, I);

    {$ifdef OpenGLES}
    if ParseString(VersionString, I) <> 'OpenGL' then
      WritelnWarning('OpenGL', 'OpenGL ES version string 1st component must be "OpenGL"');
    ParseWhiteSpaces(VersionString, I);

    if not IsPrefix('ES', ParseString(VersionString, I)) then
      WritelnWarning('OpenGL', 'OpenGL ES version string 2nd component must start with "ES"');
    ParseWhiteSpaces(VersionString, I);
    {$endif}

    if not ParseNumber(VersionString, I, Major) then Exit;
    ParseWhiteSpaces(VersionString, I);

    { Dot }
    if not SCharIs(VersionString, I, '.') then
      Exit; // The dot "." separator major and minor version number not found
    Inc(I);
    ParseWhiteSpaces(VersionString, I);

    if not ParseNumber(VersionString, I, Minor) then Exit;

    { Release number }
    ReleaseExists := SCharIs(VersionString, I, '.');
    if ReleaseExists then
    begin
      Inc(I);
      if not ParseNumber(VersionString, I, Release) then Exit;
    end;
    ParseWhiteSpaces(VersionString, I);

    VendorInfo := SEnding(VersionString, I);
  except
    { In case of any error here: silence it.
      We want our program to work even with broken GL_VERSION or GLU_VERSION
      strings.

      Class constructor always starts with Major and Minor initialized
      to 0, ReleaseExists initialized to false, and VendorInfo to ''.
      If we have here an exception, only part of them may be initialized. }
  end;
end;

function TGenericGLVersion.AtLeast(AMajor, AMinor: Integer): boolean;
begin
  Result := (AMajor < Major) or
    ( (AMajor = Major) and (AMinor <= Minor) );
end;

{ TGLVersion ----------------------------------------------------------------- }

constructor TGLVersion.Create(const VersionString, AVendor, ARenderer: string);

  { Parse VendorMajor / VendorMinor / VendorRelease, starting from S[I]. }
  procedure ParseVendorVersion(const S: string; var I: Integer);
  begin
    ParseWhiteSpaces(S, I);

    if not ParseNumber(S, I, FVendorMajor) then Exit;
    ParseWhiteSpaces(S, I);

    { Dot }
    if not SCharIs(S, I, '.') then
      Exit; //The dot "." separator between Vendor major and minor version number not found
    Inc(I);
    ParseWhiteSpaces(S, I);

    if not ParseNumber(S, I, FVendorMinor) then Exit;
    ParseWhiteSpaces(S, I);

    { Vendor release number }
    if SCharIs(S, I, '.') then
    begin
      Inc(I);
      ParseWhiteSpaces(S, I);
      if not ParseNumber(S, I, FVendorRelease) then Exit;
    end else
    begin
      { Some older Mesa versions (like 5.1) and newer (7.2) really
        don't have release number inside a version string.
        Seems like they don't have
        release number at all, and assuming "0" seems sensible following
        version names on WWW. So the missing dot "."
        separator between Vendor minor and release version number should
        be ignored. }
      FVendorRelease := 0;
    end;
  end;

  function VendorVersionAtLeast(VerMaj, VerMin, VerRel: Integer): boolean;
  begin
    Result :=
        (VendorMajor > VerMaj) or
      ( (VendorMajor = VerMaj) and (

        (VendorMinor > VerMin) or
      ( (VendorMinor = VerMin) and (

         VendorRelease >= VerRel
      ))));
  end;

var
  VendorName, S: string;
  MesaStartIndex, I: Integer;
begin
  inherited Create(VersionString);

  try
    I := 1;
    while SCharIs(VendorInfo, I, AllChars - WhiteSpaces) do Inc(I);

    VendorName := CopyPos(VendorInfo, 1, I - 1);
    FMesa := SameText(VendorName, 'Mesa');

    { Handle version strings when vendor version is in parenthesis,
      like this: GL_VERSION = '1.4 (2.1 Mesa 7.0.4)'
      (Debian testing (lenny) on 2008-12-31).
      In such case "Mesa" is within parenthesis, preceeded by another version
      number. }
    if SCharIs(VendorInfo, 1, '(') and
       (VendorInfo[Length(VendorInfo)] = ')') then
    begin
      S := Copy(VendorInfo, 2, Length(VendorInfo) - 2);
      I := 1;

      { omit preceeding version number }
      while SCharIs(S, I, AllChars - WhiteSpaces) do Inc(I);

      { omit whitespace }
      ParseWhiteSpaces(S, I);

      { read "Mesa" (hopefully) string }
      MesaStartIndex := I;
      while SCharIs(S, I, AllChars - WhiteSpaces) do Inc(I);

      VendorName := CopyPos(S, MesaStartIndex, I - 1);
      FMesa := SameText(VendorName, 'Mesa');
      if Mesa then
        ParseVendorVersion(S, I);
    end else
    begin
      { Try to handle normal vendor version.
        This should work on various GPUs (at least for Mesa, Intel and NVidia). }
      while SCharIs(VendorInfo, I, AllChars - ['0'..'9']) and
            (I < Length(VendorInfo)) do
        Inc(I);
      ParseVendorVersion(VendorInfo, I);
    end;
  except
    { Just like in TGenericGLVersion: in case of trouble (broken GL_VERSION
      string) ignore the problem. }
  end;

  FVendor := AVendor;
  FRenderer := ARenderer;

  { calculate FVendorType }
  if IsPrefix('NVIDIA', Vendor) then // Actually seen possible values here: 'NVIDIA Corporation'.
    FVendorType := gvNvidia else
  { Although "ATI Technologies Inc." is usually found,
    according to http://delphi3d.net/hardware/listreports.php
    also just "ATI" is possible. }
  if (Vendor = 'ATI Technologies Inc.') or (Vendor = 'ATI') then
    FVendorType := gvATI else
  if IsPrefix('Intel', Vendor) then
    FVendorType := gvIntel else
  if (Vendor = 'Imagination Technologies') then
    FVendorType := gvImaginationTechnologies else
    FVendorType := gvUnknown;

  FFglrx := {$ifdef LINUX} VendorType = gvATI {$else} false {$endif};

  FBuggyGenerateMipmap := (Mesa and (not VendorVersionAtLeast(7, 5, 0)))
                          {$ifdef WINDOWS} or (VendorType = gvIntel) {$endif};

  FBuggyFBOCubeMap := {$ifdef WINDOWS} VendorType = gvIntel {$else} false {$endif};

  FBuggyGenerateCubeMap := {$ifdef WINDOWS} ((VendorType = gvIntel) and SameText(Renderer, 'Intel(R) HD Graphics 4000')) {$else} false {$endif};
  { On which fglrx versions does this occur?

    - On Catalyst 8.12 (fglrx 8.561) all seems to work fine
      (tested on MacBook Pro "chantal").

    - Catalyst 9.1 (fglrx 8.573) - not known.
      Below we only *assume* the bug started from 9.1.

    - On Catalyst 9.10 and 10.3 the bug does occur.
      Tested on Radeon HD 4300 (on HP ProBook "czarny"), Ubuntu x86_64.

    - Bug confirmed also on Ubuntu 10.04 (fglrx 8.723).
      Tested on Radeon HD 4300 (on HP ProBook "czarny"), Ubuntu x86_64.

    - Bug disappeared on Ubuntu 10.10 (fglrx 8.780). Seems fixed there.
      (fglrx bugzilla was wiped, so we don't have any official
      confirmation about this from AMD.) }

  FBuggyLightModelTwoSide := Fglrx and ReleaseExists and
    (Release >= 8573) and (Release < 8780);
  if BuggyLightModelTwoSide then
    FBuggyLightModelTwoSideMessage := 'Detected fglrx (ATI proprietary Linux drivers) version >= 9.x. ' + 'Setting GL_LIGHT_MODEL_TWO_SIDE to GL_TRUE may cause nasty bugs on some shaders (see http://sourceforge.net/apps/phpbb/vrmlengine/viewtopic.php?f=3&t=14), so disabling two-sided lighting.' else
    FBuggyLightModelTwoSideMessage := '';

  FBuggyVBO := {$ifdef WINDOWS}
    { See demo_models/x3d/background_test_mobile_intel_gpu_bugs.x3d }
    (Vendor = 'Intel') and
    (Renderer = 'Intel Cantiga') and
    (not AtLeast(1, 6))
    {$else}
    false
    {$endif};

  FBuggyShaderShadowMap :=
    { This happens on fglrx, the worst OpenGL driver in the world.
      card: ATI Mobility Radeon HD 4300,
      confirmed on
        Ubuntu 10.10/x86_64 (czarny)
        Ubuntu 10.10/i386   (czarny)
        Ubuntu 11.4/x86_64  (czarny)
        Ubuntu 11.4/i386    (czarny) (fglrx OpenGL version 3.3.10665)
      not occurs on
        Ubuntu 9.10/i386    (czarny)
      Looks like fglrx bug since at least Ubuntu 10.10 (assuming always
      since Ubuntu 10.04, which is fglrx >= 8.723). }
    Fglrx and ReleaseExists and (Release >= 8723);

  FBuggyGLSLConstStruct := {$ifdef LINUX} VendorType = gvNvidia {$else} false {$endif};

   { Reported on Radeon 6600, 6850 - looks like wireframe
     Also on Intel cards - querying multisampled depth buffer returns bad data. }
  FBuggyFBOMultiSampling :=
    {$ifdef WINDOWS} ((VendorType = gvATI) and
      (IsPrefix('AMD Radeon HD 6', Renderer) or IsPrefix('AMD Radeon HD6', Renderer)))
    or ((VendorType = gvIntel) and (not VendorVersionAtLeast(9, 18, 10)))
    {$else} false {$endif};

  { Observed on fglrx (ATI proprietary OpenGL driver under Linux,
    aka the worst OpenGL driver in the world), at least on this version:

    Version string: 3.3.11627 Compatibility Profile Context
    Vendor: ATI Technologies Inc.
    Renderer: ATI Mobility Radeon HD 4300 Series

    This corresponds to Ubuntu 12.04.1 package fglrx 2:8.960-0ubuntu1.1.
    The bug is *not* present on Windows installed on the same hardware
    (or any other GPU, Linux or Windows).
    Easily reproducible by fps_game: the ExtraViewport by default leaves
    glViewport at non-full-screen state at the end of EventDraw,
    causing following SwapBuffers in DoDraw to fail, leaving part
    of the screen not updated (black) without this workaround. }
  FBuggySwapNonStandardViewport := Fglrx;

  { Observed on Mac Book Pro GPU under Linux, with Debian testing on 2013-01-08:

      Version string: 2.1 Mesa 8.0.5
      Version parsed: major: 2, minor: 1, release exists: FALSE, release: 0, vendor-specific version: "Mesa 8.0.5"
      Vendor: VMware, Inc.
      Renderer: Gallium 0.4 on llvmpipe (LLVM 0x209)
  }
  FBuggyDepth32 := Mesa and (VendorMajor = 8) and (VendorMinor = 0) and
    (Vendor = 'VMware, Inc.') and IsPrefix('Gallium 0.4 on llvmpipe', Renderer);

  { Observed on (system "river" owned by Michalis, Debian):

      Version string: 3.0 Mesa 10.2.2
      Vendor: nouveau
      Renderer: Gallium 0.4 on NVC3

    Not observed on (system "chantal" owned by Michalis, Debian):

      Version string: 2.1 Mesa 10.2.1
      Vendor: X.Org R300 Project
      Renderer: Gallium 0.4 on ATI RV530

    Observed on (system "czarny" owned by Michalis, Ubuntu):

      Version string: 3.0 Mesa 10.1.3
      Vendor: X.Org
      Renderer: Gallium 0.4 on AMD RV710
  }
  FBuggyGLSLFrontFacing := Mesa and (VendorMajor = 10) and (Major = 3);

  { observed on Android on

      Version string: OpenGL ES 2.0 build 1.9.RC2@2130229
      Version parsed: major: 2, minor: 0, release exists: False, release: 0, vendor-specific information: "build 1.9.RC2@2130229"
      Vendor-specific version parsed: major: 1, minor: 9, release: 0
      Vendor: Imagination Technologies
      Renderer: PowerVR SGX 540
  }
  FBuggyGLSLReadVarying :=
    {$ifdef ANDROID}
    (VendorType = gvImaginationTechnologies) and
    (Major = 2)
    {$else} false
    {$endif};
end;

const
  VendorTypeNames: array [TGLVendorType] of string =
  ( 'Unknown',
    'ATI',
    'Nvidia',
    'Intel',
    'Imagination Technologies'
  );

function VendorTypeToStr(const VendorType: TGLVendorType): string;
begin
  Result := VendorTypeNames[VendorType];
end;

end.
