unit FormPhysicsLayersNamesPropertyEditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ComCtrls,
  StdCtrls, CastleTransform;

type
  TPhysicsLayersNamesPropertyEditorForm = class(TForm)
    ButtonOK: TButton;
    NamesAndDescListView: TListView;
  strict private
     FLayersNames: TCastleLayersNames;
  public
    procedure Init(const LayersNames: TCastleLayersNames);
  end;

implementation

{$R *.lfm}

{ TPhysicsLayersNamesPropertyEditorForm }

procedure TPhysicsLayersNamesPropertyEditorForm.Init(
  const LayersNames: TCastleLayersNames);
begin
  FLayersNames := LayersNames;
end;

end.

