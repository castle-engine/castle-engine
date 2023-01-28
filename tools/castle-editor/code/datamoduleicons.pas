unit DataModuleIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TIcons = class(TDataModule)
    ToolbarSpriteSheet: TImageList;
    ToolbarIcons: TImageList;
    MenuSpriteSheet: TImageList;
  private
  public
    type
      // Enums with order reflecting ToolbarIcons
      TIconIndexes = (
        iiNewFile,
        iiOpenFile,
        iiSaveFile,
        iiMinus,
        iiPlus,
        iiPlay,
        iiStop,
        iiPause,
        iiSimulationPlay,
        iiSimulationStop,
        iiSimulationPause
      );
  end;

var
  Icons: TIcons;

implementation

{$R *.lfm}

end.

