{
  Copyright 2020-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleLog, CastleApplicationProperties, CastleDownload, CastleClassUtils,
  CastleControls, CastleUIControls, CastleColors, CastleUnicode, CastleUtils,
  CastleFilesUtils, CastleFonts;

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;

  procedure TestLocalChars;

    procedure AssertEquals(const Expected, Actual: String);
    begin
      if Expected <> Actual then
        raise Exception.CreateFmt('Expected "%s", actual "%s"', [Expected, Actual]);
    end;

    { Test reading filenames using FPC RTL, without any CGE functions in the middle. }
    procedure TestReadingRtl(const FileName: String);
    var
      F: TextFile;
      S: String;
    begin
      try
        AssignFile(F, FileName);
        Reset(F);
        try
          Readln(F, S);
          AssertEquals('Testing.', Trim(S));
        finally CloseFile(F) end;
      except
        // catch EInOutError and add FileName
        on E: EInOutError do
        begin
          E.Message := Format('Reading file "%s": ', [FileName]) + E.Message;
          raise;
        end;
      end;
    end;

    { Test reading file using URL (through CGE function). }
    procedure TestReading(const URL: String);
    var
      Stream: TStream;
      S: String;
    begin
      Stream := Download(URL);
      try
        S := StreamToString(Stream);
        AssertEquals('Testing.', Trim(S));
      finally FreeAndNil(Stream) end;
    end;

    { Test reading file, whose URL is written inside another file, in UTF-8 encoding. }
    procedure TestReadingThroughReference(const URL: String);
    var
      Stream: TStream;
      ReferredURL: String;
    begin
      Stream := Download(URL);
      try
        ReferredURL := Trim(StreamToString(Stream));
        TestReading(ReferredURL);
      finally FreeAndNil(Stream) end;
    end;

    { Test reading font (as it goes through FreeType library). }
    procedure TestReadingFont(const FontUrl: String);
    var
      MyNewFont: TCastleFont;
    begin
      MyNewFont := TCastleFont.Create(nil);
      try
        MyNewFont.Size := 20;
        MyNewFont.AntiAliased := true;
        MyNewFont.Url := FontUrl;
      finally FreeAndNil(MyNewFont) end;
    end;

  var
    DataPath: String;
  begin
    {$ifdef MSWINDOWS}
    if not FileExists(ExeNameFromGetModule) then
      raise Exception.CreateFmt('Cannot find own exe file "%s"', [ExeNameFromGetModule]);
    {$endif}

    { This is an *extremely* simplified (only correct in simplest cases) implementation
      of determining application data directory
      ( https://castle-engine.io/manual_data_directory.php ).
      We do it only for TestReadingRtl test here (that uses FPC functions
      *without* any CGE API, just for test).
      In real CGE applications, you should always use 'castle-data:/'
      URL instead of doing it like this! }
    DataPath :=
      {$ifdef MSWINDOWS} ExtractFilePath(ExeNameFromGetModule)
      {$else} InclPathDelim(GetCurrentDir)
      {$endif} + InclPathDelim('data');

    if not DirectoryExists(DataPath) then
      raise Exception.CreateFmt('Cannot find directory "%s"', [DataPath]);

    TestReadingRtl(DataPath + 'ascii_name.txt');
    (*TODO:
      This doesn't work (anymore?) with FPC 3.2.2 or FPC 3.3.1 on Linux/x86_64.
      Unknown how to make it work, {$codepage utf8} doesn't help.

    TestReadingRtl(DataPath + 'name with Polish chars ćma źrebak żmija wąż królik.txt');
    TestReadingRtl(DataPath + 'name with Chinese chars 样例中文文本.txt');
    TestReadingRtl(DataPath + '样例中文文本/name with Chinese chars 样例中文文本.txt');
    TestReadingRtl(DataPath + 'name with Russian chars образец русского текста.txt');
    TestReadingRtl(DataPath + 'образец русского текста/name with Russian chars образец русского текста.txt');
    *)

    TestReading('castle-data:/ascii_name.txt');
    TestReading('castle-data:/name with Polish chars ćma źrebak żmija wąż królik.txt');
    TestReading('castle-data:/name with Chinese chars 样例中文文本.txt');
    TestReading('castle-data:/样例中文文本/name with Chinese chars 样例中文文本.txt');
    TestReading('castle-data:/name with Russian chars образец русского текста.txt');
    TestReading('castle-data:/образец русского текста/name with Russian chars образец русского текста.txt');

    TestReadingThroughReference('castle-data:/reference to file with Chinese chars.txt');
    TestReadingThroughReference('castle-data:/reference to file with Russian chars.txt');
    TestReadingThroughReference('castle-data:/reference to file with Polish chars.txt');

    StringToFile(ApplicationConfig('config_ascii.txt'), 'Testing save.');
    StringToFile(ApplicationConfig('config with Chinese chars 样例中文文本.txt'), 'Testing save.');
    StringToFile(ApplicationConfig('config with Polish chars ćma źrebak żmija wąż królik.txt'), 'Testing save.');
    StringToFile(ApplicationConfig('config with Russian chars образец русского текста.txt'), 'Testing save.');

    TestReadingFont('castle-data:/DejaVuSans name with Russian chars образец русского текста.ttf');
  end;

var
  L: TCastleLabel;
begin
  TestLocalChars;

  L := TCastleLabel.Create(Application);
  L.Color := Yellow;
  L.Caption := 'All tests executed OK :)';
  L.Anchor(hpMiddle);
  L.Anchor(vpMiddle);
  Window.Controls.InsertFront(L);
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;
end.
