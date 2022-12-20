unit FormCastleColorPicker;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, HSLRingPicker, CastlePropEdits, CastleColors;

type
  TCastleColorPickerForm = class(TForm)
    HSLRingPicker: THSLRingPicker;
    procedure HSLRingPickerChange(Sender: TObject);
  private

  public
    ColorPropertyEditor: TCastleColorPropertyEditor;
    PrevColor: TCastleColor;

    procedure Init(const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
  end;

var
  CastleColorPickerForm: TCastleColorPickerForm;

implementation

{$R *.lfm}

uses CastleVectors;

{ TCastleColorPickerForm }

procedure TCastleColorPickerForm.HSLRingPickerChange(Sender: TObject);
var
  ColorByte: TVector3Byte;
  NewColor: TCastleColor;
begin
  // on color change
  if Assigned(ColorPropertyEditor) then
  begin
    RedGreenBlue(HSLRingPicker.SelectedColor, ColorByte.X, ColorByte.Y, ColorByte.Z);

    NewColor := Vector4(Vector3(ColorByte), 1); // TODO: Alpha support

    ColorPropertyEditor.SetAllValues(NewColor);
  end;
end;

procedure TCastleColorPickerForm.Init(
  const ColorPropEditor: TCastleColorPropertyEditor; InitColor: TCastleColor);
var
  ColorByte: TVector3Byte;
begin
  ColorPropertyEditor := ColorPropEditor;
  PrevColor := InitColor;

  ColorByte := Vector3Byte(InitColor.XYZ); // edit only Color RGB
  HSLRingPicker.SelectedColor := RGBToColor(ColorByte[0], ColorByte[1], ColorByte[2]);


end;

end.

