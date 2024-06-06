unit FormImportStarling;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, ButtonPanel, Buttons;

type
  TImportStarlingForm = class(TForm)
    ButtonDefault: TBitBtn;
    ButtonPanel1: TButtonPanel;
    FloatSpinEditFPS: TFloatSpinEdit;
    Label1: TLabel;
    LabelStarlingOptions: TLabel;
    LabelStrictDesc: TLabel;
    LabelNamingConvention: TLabel;
    LabelTralingDesc: TLabel;
    EditURL: TEdit;
    LabelURLTitle: TLabel;
    PanelNamingConvention: TPanel;
    LabelFPS: TLabel;
    PanelURL: TPanel;
    PanelFPS: TPanel;
    RadioStrictUnderscore: TRadioButton;
    RadioTralingNumber: TRadioButton;
    procedure ButtonDefaultClick(Sender: TObject);
    procedure LabelTralingDescClick(Sender: TObject);
    procedure LabelStrictDescClick(Sender: TObject);
    procedure OptionsChange(Sender: TObject);
  strict private
    function GetURL: String;
  public
    procedure Initialize(const URL: String);

    property URL: String read GetURL;
  end;

var
  ImportStarlingForm: TImportStarlingForm;

implementation

uses
  Math,
  CastleUriUtils;

{$R *.lfm}

{ TImportStarlingForm }

procedure TImportStarlingForm.OptionsChange(Sender: TObject);
var
  URLAnchor: String;
begin
  if RadioStrictUnderscore.Checked then
    URLAnchor := '#anim-naming:strict-underscore'
  else
    URLAnchor := '#anim-naming:trailing-number';

  if not SameValue(FloatSpinEditFPS.Value, 8.0) then
    URLAnchor := URLAnchor + ',fps:' + Format('%f', [FloatSpinEditFPS.Value]);

  EditURL.Text := URIDeleteAnchor(EditURL.Text) + URLAnchor;
  EditURL.Hint := URIDeleteAnchor(EditURL.Text) + URLAnchor;
end;

procedure TImportStarlingForm.ButtonDefaultClick(Sender: TObject);
begin
  RadioStrictUnderscore.Checked := true;
  FloatSpinEditFPS.Value := 8.0;
end;

procedure TImportStarlingForm.LabelTralingDescClick(Sender: TObject);
begin
  RadioTralingNumber.Checked := true;
end;

procedure TImportStarlingForm.LabelStrictDescClick(Sender: TObject);
begin
  RadioStrictUnderscore.Checked := true;
end;

function TImportStarlingForm.GetURL: String;
begin
  Result := EditURL.Text;
end;

procedure TImportStarlingForm.Initialize(const URL: String);
begin
  EditURL.Text := URL;
end;

end.

