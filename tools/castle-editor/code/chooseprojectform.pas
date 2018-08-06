unit ChooseProjectForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons;

type
  { Choose project (new or existing). }
  TChooseProject = class(TForm)
    ButtonNew: TBitBtn;
    ButtonLoad: TBitBtn;
    ImageLogo: TImage;
    LabelTitle: TLabel;
  private

  public

  end;

var
  ChooseProject: TChooseProject;

implementation

{$R *.lfm}

end.

