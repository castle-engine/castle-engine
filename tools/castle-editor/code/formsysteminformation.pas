{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Show system information - OpenGL, audio, other reports. }
unit FormSystemInformation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, ExtCtrls,
  StdCtrls, CastleControl;

type
  TSystemInformationForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    CastleControl1: TCastleControl;
    ListSections: TListBox;
    MemoSysInfo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    SaveDialogText: TSaveDialog;
    procedure CastleControl1Open(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure HelpButtonClick(Sender: TObject);
    procedure ListSectionsClick(Sender: TObject);
  private
    type
      TInfoType = (itRendering, itAudio, itOther);
    var
      Info: array [TInfoType] of String;
    procedure SoundEngineOpenClose(Sender: TObject);
  public

  end;

var
  SystemInformationForm: TSystemInformationForm;

implementation

{$R *.lfm}

uses CastleGLUtils, CastleSoundEngine, CastleUtils, CastleFilesUtils,
  CastleURIUtils,
  ProjectUtils;

procedure TSystemInformationForm.ListSectionsClick(Sender: TObject);
begin
  MemoSysInfo.Lines.Text := Info[TInfoType(ListSections.ItemIndex)];
end;

procedure TSystemInformationForm.SoundEngineOpenClose(Sender: TObject);
begin
  { Update Info[itAudio] when sound engine initializes / deinitializes. }
  Info[itAudio] := SoundEngine.Information;
end;

procedure TSystemInformationForm.FormShow(Sender: TObject);
var
  OldApplicationDataOverride: String;
begin
  OldApplicationDataOverride := ApplicationDataOverride;
  UseEditorApplicationData;
  CastleControl1.DesignUrl := 'castle-data:/demo_animation/view_demo_animation.castle-user-interface';
  ApplicationDataOverride := OldApplicationDataOverride;

  { Info[itRendering] will be initialized in CastleControl1Open.
    We do not even reset it here, as CastleControl1 may have been already opened
    (when this form was shown previous time)
    so we just may have valid Info[itRendering] already. }

  Info[itAudio] := SoundEngine.Information;
  Info[itOther] :=
    'Castle Game Engine version: ' + CastleEngineVersion + '.' + NL +
    'Editor compiled with ' + SCompilerDescription + '.' + NL +
    'Editor platform: ' + SPlatformDescription + '.' + NL;

  SoundEngine.OnOpenClose.Add(@SoundEngineOpenClose);

  ListSections.ItemIndex := 0;
  MemoSysInfo.Lines.Text := Info[TInfoType(ListSections.ItemIndex)];
end;

procedure TSystemInformationForm.CastleControl1Open(Sender: TObject);
begin
  // use GLInformationString only once rendering context initialized
  Info[itRendering] := GLInformationString;

  if ListSections.ItemIndex = Ord(itRendering) then
    MemoSysInfo.Lines.Text := Info[itRendering];
end;

procedure TSystemInformationForm.FormHide(Sender: TObject);
begin
  CastleControl1.DesignUrl := ''; // unload to stop sound

  SoundEngine.OnOpenClose.Remove(@SoundEngineOpenClose);
end;

procedure TSystemInformationForm.HelpButtonClick(Sender: TObject);
begin
  if SaveDialogText.Execute then
  begin
    StringToFile(FilenameToURISafe(SaveDialogText.FileName),
      'Rendering:' + NL + Info[itRendering] + NL + NL +
      'Audio:' + NL + Info[itAudio] + NL + NL +
      'Other:' + NL + Info[itOther]// + NL + NL +
    );
  end;
end;

end.

