unit DataModuleIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  { Icons for various forms, collected in TImageList components. }
  TIcons = class(TDataModule)
    { Toolbar of FrameDesign form. }
    ToolbarIcons: TImageList;
    { Alt version of ToolbarIcons, for dark theme, used when button inactive (but enabled). }
    ToolbarIconsDarkInactive: TImageList;
    { Alt version of ToolbarIcons, for dark theme, used when button active (and enabled). }
    ToolbarIconsDarkActive: TImageList;
    { Alt version of ToolbarIcons, for dark theme, used when button disabled. }
    ToolbarIconsDarkDisabled: TImageList;

    ToolbarSpriteSheet: TImageList;
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

