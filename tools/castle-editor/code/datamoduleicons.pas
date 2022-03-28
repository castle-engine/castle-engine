unit DataModuleIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TIcons = class(TDataModule)
    ToolbarStroke2: TImageList;
    ToolbarStroke1_5: TImageList;
    MenuStroke1_5: TImageList;
  private
  public
    type
      TIconIndexes = (
        iiNewFile = 0,
        iiOpenFile = 1,
        iiSaveFile,
        iiMinus,
        iiPlus,
        iiPhysicsPlay,
        iiPhysicsStop,
        iiPhysicsPause
      );
  end;

var
  Icons: TIcons;

implementation

{$R *.lfm}

end.

