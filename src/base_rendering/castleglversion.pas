{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Checking OpenGL version, vendors and such (GLVersion).
  These should be initialized by calling GLInformationInitialize,
  which is done automatically when opening OpenGL context using
  TCastleWindow or TCastleControl.
}
unit CastleGLVersion;

{$I castleconf.inc}

{$ifdef CASTLE_STRICT_CLI}
  {$error When CASTLE_STRICT_CLI is defined, you cannot link to this unit.}
{$endif}

interface

type
  { OpenGL(ES) library version information.

    As obtained from glGetString(GL_VERSION), glGetString(GL_VENDOR) and similar
    routines.

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

    { Vendor-specific version that was at the end of VersionString (after the
      major_number.minor_number.release_number). It never has any whitespace
      at the beginning (we trim it when initializing). }
    VendorVersion: string;

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
    { Imagination Technologies (PowerVR) GPU, found on mobile devices. }
    gvImaginationTechnologies,
    { Qualcomm Adreno, found on mobile devices. }
    gvQualcomm,
    { Arm, makers of Mali GPU, found on mobile devices. }
    gvArm
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
    FBuggyGenerateCubeMap: boolean;
    FBuggyFBOCubeMap: boolean;
    FBuggyVBO: boolean;
    FBuggyFBOMultiSampling: boolean;
    FBuggySwapNonStandardViewport: boolean;
    FBuggyDepth32: boolean;
    FBuggyGLSLFrontFacing: boolean;
    FBuggyGLSLReadVarying: boolean;
    FBuggyPureShaderPipeline: boolean;
    FBuggyTextureSizeAbove2048: Boolean;
    FBuggyGLSLBumpMappingNumSteps: Boolean;
    function AppleRendererOlderThan(const VersionNumber: Cardinal): Boolean;
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
      Note that this is detected using VendorVersion, not Vendor. }
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
      This is worse than BuggyFBOCubeMap, magenta is always seen at positiveX part
      of the cube map.

      Observed, and this workaround is needed, at least on:
      @unorderedList(
        @item Version string: 3.3.0 - Build 8.15.10.2778
        @item Vendor: Intel
        @item Renderer: Intel(R) HD Graphics 4000
      )
    }
    property BuggyGenerateCubeMap: boolean read FBuggyGenerateCubeMap;

    { Buggy VBO (Intel(Windows) bug). }
    property BuggyVBO: boolean read FBuggyVBO;

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

    { Various problems when trying to use shaders to render everything.
      See https://github.com/castle-engine/view3dscene/issues/6#issuecomment-362826781 }
    property BuggyPureShaderPipeline: boolean read FBuggyPureShaderPipeline;

    { MaxTextureSize above 2048 shall not be trusted. }
    property BuggyTextureSizeAbove2048: Boolean read FBuggyTextureSizeAbove2048;

    { Buggy num_steps value in bmSteepParallax and bmSteepParallaxShadowing
      BumpMapping modes on Adreno mobile GPU's (random crashes, sometimes
      variable in shader has wrong value. Observed on:
      - Xiaomi Mi 10 Lite (M2002J9G), Snapdragon 765G, Adreno 620:
      - Motorola Moto G8 (XT2045-2), Snapdragon 665, Adreno 610
      - Motorola Moto G7 Plus (XT2081-2), Snapdragon 460, Adreno 610
      - Xiaomi Redmi Note 8, Snapdragon 665, Adreno 610
      - Xiaomi Redmi Note 7A Snapdragon 439, Adreno 505
      More info in comment TGLVersion.Create().

      It can be tested by https://github.com/and3md/castle-adreno-tests }
    property BuggyGLSLBumpMappingNumSteps: Boolean read FBuggyGLSLBumpMappingNumSteps;
  end;

var
  { Core OpenGL version information.
    This is usually created by CastleGLUtils.GLInformationInitialize. }
  GLVersion: TGLVersion;

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
  if (I <= Length(S)) and not CharInSet(S[I], WhiteSpaces) then
  begin
    Start := I;
    repeat Inc(I) until not ((I <= Length(S)) and not CharInSet(S[I], WhiteSpaces));
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

    VendorVersion := SEnding(VersionString, I);
  except
    { In case of any error here: silence it.
      We want our program to work even with broken GL_VERSION or GLU_VERSION
      strings.

      Class constructor always starts with Major and Minor initialized
      to 0, ReleaseExists initialized to false, and VendorVersion to ''.
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
  procedure ParseVendorVersionSuffix(const S: string; var I: Integer);
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

  { Extract information from VendorVersion String. }
  procedure ParseVendorVersion;
  const
    SCompatibilityProfile = '(Compatibility Profile)';
  var
    MaybeMesa, S: string;
    I, MaybeMesaStart: Integer;
  begin
    try
      { Handle the case like this: GL_VERSION = '1.4 (2.1 Mesa 7.0.4)'
        (Debian testing (lenny) on 2008-12-31).
        In such case "Mesa" is within parenthesis,
        preceeded by another version number that we ignore,
        followed by Mesa version number. }
      if SCharIs(VendorVersion, 1, '(') and
         (VendorVersion[Length(VendorVersion)] = ')') then
      begin
        S := Copy(VendorVersion, 2, Length(VendorVersion) - 2);
        I := 1;

        { omit preceeding version number }
        ParseString(S, I);

        { omit whitespace }
        ParseWhiteSpaces(S, I);

        { read "Mesa" (possibly) string }
        MaybeMesa := ParseString(S, I);
        FMesa := SameText(MaybeMesa, 'Mesa');
        if Mesa then
          ParseVendorVersionSuffix(S, I);
      end else
      begin
        { Try to handle normal vendor version.
          This should work on various GPUs (at least for Mesa, Intel and NVidia). }

        // calculate MaybeMesaStart, to omit SCompatibilityProfile at the beginning
        if IsPrefix(SCompatibilityProfile, VendorVersion, true) then
        begin
          MaybeMesaStart := Length(SCompatibilityProfile) + 1;
          ParseWhiteSpaces(VendorVersion, MaybeMesaStart);
        end else
          MaybeMesaStart := 1;

        // calculate MaybeMesa and FMesa
        I := MaybeMesaStart;
        MaybeMesa := ParseString(VendorVersion, I);
        FMesa := SameText(MaybeMesa, 'Mesa');

        while SCharIs(VendorVersion, I, AllChars - ['0'..'9']) and
              (I < Length(VendorVersion)) do
          Inc(I);
        ParseVendorVersionSuffix(VendorVersion, I);
      end;
    except
      { Just like in TGenericGLVersion: in case of trouble (GL_VERSION
        string that we don't recognize) just ignore the problem.
        We don't strictly need the VendorXxx information. }
    end;
  end;

  { Compare arguments with VendorMajor / VendorMinor / VendorRelease numbers. }
  function VendorVersionAtLeast(const VerMaj, VerMin, VerRel: Integer): boolean;
  begin
    Result :=
        (VendorMajor > VerMaj) or
      ( (VendorMajor = VerMaj) and (

        (VendorMinor > VerMin) or
      ( (VendorMinor = VerMin) and (

         VendorRelease >= VerRel
      ))));
  end;

begin
  inherited Create(VersionString);

  ParseVendorVersion;

  FVendor := AVendor;
  FRenderer := ARenderer;

  { calculate FVendorType }
  if IsPrefix('NVIDIA', Vendor) then // Actually seen possible values here: 'NVIDIA Corporation'.
    FVendorType := gvNvidia
  else
  { Although "ATI Technologies Inc." is usually found,
    according to http://delphi3d.net/hardware/listreports.php
    also just "ATI" is possible. }
  if (Vendor = 'ATI Technologies Inc.') or (Vendor = 'ATI') then
    FVendorType := gvATI
  else
  if IsPrefix('Intel', Vendor) then
    FVendorType := gvIntel
  else
  if (Vendor = 'Imagination Technologies') then
    FVendorType := gvImaginationTechnologies
  else
  if (Vendor = 'Qualcomm') then
    FVendorType := gvQualcomm
  else
  if SameText(Vendor, 'Arm') then
    FVendorType := gvArm
  else
    FVendorType := gvUnknown;

  FFglrx := {$ifdef LINUX} VendorType = gvATI {$else} false {$endif};

  FBuggyFBOCubeMap :=
    {$ifdef MSWINDOWS}
    ( (VendorType = gvIntel) and
      not VendorVersionAtLeast(9, 0, 0)
    )
    {$else} false
    {$endif};

  FBuggyGenerateCubeMap :=
    {$ifdef MSWINDOWS}
    ( (VendorType = gvIntel) and
      SameText(Renderer, 'Intel(R) HD Graphics 4000') and
      not VendorVersionAtLeast(9, 0, 0)
    )
    {$else} false
    {$endif};

  FBuggyVBO := {$ifdef MSWINDOWS}
    { See demo_models/x3d/background_test_mobile_intel_gpu_bugs.x3d }
    (Vendor = 'Intel') and
    (Renderer = 'Intel Cantiga') and
    (not AtLeast(1, 6))
    {$else}
    false
    {$endif};

  { Reported on Radeon 6600, 6850 - looks like wireframe
     Also on Intel cards - querying multisampled depth buffer returns bad data. }
  FBuggyFBOMultiSampling :=
    {$ifdef MSWINDOWS} ((VendorType = gvATI) and
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

  FBuggyPureShaderPipeline :=
    {$ifdef MSWINDOWS}
    ( (VendorType = gvIntel) and
      not VendorVersionAtLeast(9, 0, 0)
    ) or

    { Workaround various troubles on HP ProBook

        Version string: 3.3.11672 Compatibility Profile Context
        Version parsed: major: 3, minor: 3, release exists: True, release: 11672, vendor-specific information: "Compatibility Profile Context"
        Vendor-specific version parsed: major: 0, minor: 0, release: 0
        Vendor: ATI Technologies Inc.
        Vendor type: ATI
        Renderer: ATI Mobility Radeon HD 4300 Series

      Later: bumped to include all troubles on
      AMD Radeon HD 8200 / R3 Series
      reported on https://github.com/castle-engine/view3dscene/issues/9 :

        Version string: 4.5.13492 Compatibility Profile Context 22.19.677.257
        Version parsed: major: 4, minor: 5, release exists: True, release: 13492,
        vendor-specific information: "Compatibility Profile Context 22.19.677.257"
        Vendor-specific version parsed: major: 22, minor: 19, release: 677
        Vendor: ATI Technologies Inc.
        Vendor type: ATI
        Renderer: AMD Radeon HD 8200 / R3 Series

      Later: bumped to 13571 after Elmar Knittel mail,
      https://github.com/castle-engine/view3dscene/issues/9
      still occurs with later release.
    }
    ( (VendorType = gvATI) and
      ReleaseExists and
      (Release <= 13571) )
    {$else} false
    {$endif};

  { On old iPhones, OpenGLES reports it can handle 4096 texture size,
    but actually it can crash on them.
    Testcase: Unholy on iOS with Wedding (ice texture).
    Tested: it works OK on 'Apple A12 GPU' and 'Apple A13 GPU', and crashes on 'Apple A10 GPU'.
    For safety, assume it crashes too on 'Apple A11 GPU' (not tested). }
  FBuggyTextureSizeAbove2048 :=
    {$ifdef CASTLE_IOS}
    AppleRendererOlderThan(12)
    {$else}
    false
    {$endif};

  { BuggyGLSLBumpMappingNumSteps - affected devices:

    Xiaomi Mi 10 Lite (M2002J9G), Snapdragon 765G, Adreno 620:
    Version string: OpenGL ES 3.2 V@444.0 (GIT@1fbdf4e, Ia04d082869, 1585724642) (Date:04/01/20)
    Version parsed: major: 3, minor: 2, release exists: False, release: 0, vendor-specific information: "V@444.0 (GIT@1fbdf4e, Ia04d082869, 1585724642) (Date:04/01/20)"
    Vendor-specific version parsed: major: 1, minor: 0, release: 0
    Vendor: Qualcomm
    Vendor type: Unknown
    Renderer: Adreno (TM) 620

    Redmi Note 8, Snapdragon 665, Adreno 610
    Version string: OpenGL ES 3.2 V@378.0 (GIT@6c0fbe4, I4f6179b11f) (Date:03/05/20)
    Version parsed: major: 3, minor: 2, release exists: False, release: 0, vendor-specific information: "V@378.0 (GIT@6c0fbe4, I4f6179b11f) (Date:03/05/20)"
    Vendor-specific version parsed: major: 6, minor: 0, release: 0
    Vendor: Qualcomm
    Vendor type: Qualcomm
    Renderer: Adreno (TM) 610

    Motorola Moto G8 (XT2045-2), Snapdragon 665, Adreno 610
    Version:
    Version string: OpenGL ES 3.2 V@0502.0 (GIT@d4cfdf3, Ic907de5ed0, 1601055299) (Date:09/25/20)
    Version parsed: major: 3, minor: 2, release exists: False, release: 0, vendor-specific information: "V@0502.0 (GIT@d4cfdf3, Ic907de5ed0, 1601055299) (Date:09/25/20)"
    Vendor-specific version parsed: major: 4, minor: 0, release: 0
    Vendor: Qualcomm
    Vendor type: Qualcomm
    Renderer: Adreno (TM) 610

    Motorola Moto G7 Plus (XT2081-2), Snapdragon 460, Adreno 610
    Version:
    Version string: OpenGL ES 3.2 V@444.0 (GIT@39a1dfd, Ic628754133, 1593471350) (Date:06/29/20)
    Version parsed: major: 3, minor: 2, release exists: False, release: 0, vendor-specific information: "V@444.0 (GIT@39a1dfd, Ic628754133, 1593471350) (Date:06/29/20)"
    Vendor-specific version parsed: major: 39, minor: 0, release: 0
    Vendor: Qualcomm
    Vendor type: Qualcomm
    Renderer: Adreno (TM) 610

    Redmi Note 7A Snapdragon 439, Adreno 505
    Version:
    Version string: OpenGL ES 3.2 V@415.0 (GIT@c692a3f, Ie3bb699d95, 1601378470) (Date:09/29/20)
    Version parsed: major: 3, minor: 2, release exists: False, release: 0, vendor-specific information: "V@415.0 (GIT@c692a3f, Ie3bb699d95, 1601378470) (Date:09/29/20)"
    Vendor-specific version parsed: major: 692, minor: 0, release: 0
    Vendor: Qualcomm
    Vendor type: Qualcomm
    Renderer: Adreno (TM) 505 }
  FBuggyGLSLBumpMappingNumSteps :=
    {$ifdef ANDROID}
    (VendorType = gvQualcomm)
    {$else} false
    {$endif};
end;

function TGLVersion.AppleRendererOlderThan(const VersionNumber: Cardinal): Boolean;
var
  I: Integer;
begin
  for I := 0 to VersionNumber - 1 do
    if Renderer = 'Apple A' + IntToStr(I) + ' GPU' then
      Exit(true);
  Result := false;
end;

const
  VendorTypeNames: array [TGLVendorType] of string =
  ( 'Unknown',
    'ATI',
    'Nvidia',
    'Intel',
    'Imagination Technologies',
    'Qualcomm',
    'Arm'
  );

function VendorTypeToStr(const VendorType: TGLVendorType): string;
begin
  Result := VendorTypeNames[VendorType];
end;

end.
