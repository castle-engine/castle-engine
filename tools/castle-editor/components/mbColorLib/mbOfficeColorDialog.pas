unit mbOfficeColorDialog;

{$MODE DELPHI}

interface

uses
  LCLIntf, LCLType,
  SysUtils, Classes, Graphics, Forms, OfficeMoreColorsDialog;

type
  TmbOfficeColorDialog = class(TComponent)
  private
    FWin: TOfficeMoreColorsWin;
    FSelColor: TColor;
    FUseHint: boolean;
    FMaxHue, FMaxSat, FMaxLum: Integer;
    FPickerIndex: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    function Execute: boolean; overload;
    function Execute(AColor: TColor): boolean; overload;
  published
    property SelectedColor: TColor read FSelColor write FSelColor default clWhite;
    property MaxHue: Integer read FMaxHue write FMaxHue default 360;
    property MaxSaturation: Integer read FMaxSat write FMaxSat default 255;
    property MaxLuminance: Integer read FMaxLum write FMaxLum default 255;
    property UseHints: boolean read FUseHint write FUseHint default false;
  end;

implementation

{ TmbOfficeColorDialog }

constructor TmbOfficeColorDialog.Create(AOwner: TComponent);
begin
  inherited;
  FSelColor := clWhite;
  FUseHint := false;
  FMaxHue := 360;
  FMaxSat := 255;
  FMaxLum := 255;
end;

function TmbOfficeColorDialog.Execute: boolean;
begin
  Result := Execute(FSelColor);
end;

function TmbOfficeColorDialog.Execute(AColor: TColor): boolean;
begin
  FWin := TOfficeMoreColorsWin.Create(Application);
  try
    FWin.ShowHint := FUseHint;
    FWin.MaxHue := FMaxHue;
    FWin.MaxSaturation := FMaxSat;
    FWin.MaxLuminance := FMaxLum;
    FWin.PickerIndex := FPickerIndex;
//    FWin.OldSwatch.Color := AColor;
    FWin.SelectedColor := AColor;
    Result := (FWin.ShowModal = IdOK);
    if Result then
      FSelColor := FWin.SelectedColor //FWin.NewSwatch.Color
    else
      FSelColor := clNone;
    FPickerIndex := FWin.PickerIndex;
  finally
    FWin.Free;
  end;
end;

end.
