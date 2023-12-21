unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.StdCtrls, FMX.Controls.Presentation, Fmx.CastleControl,
  CastleScene, CastleViewport, CastleControls, CastleUiControls;

type
  TForm1 = class(TForm)
    CastleControl1: TCastleControl;
    ButtonLoadUrl: TButton;
    LayoutAnimations: TGridLayout;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ButtonLoadUrlClick(Sender: TObject);
  private
    MainScene: TCastleScene;
    MainViewport: TCastleViewport;
    LabelFps: TCastleLabel;
    procedure LoadScene(const Url: String);
    procedure AnimationButtonClick(Sender: TObject);
    procedure DoUpdate(const Sender: TCastleUserInterface;
      const SecondsPassed: Single; var HandleInput: Boolean);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses CastleSceneCore, CastleFmxUtils, X3DLoad, CastleUriUtils, CastleRenderOptions;

{$R *.fmx}

procedure TForm1.ButtonLoadUrlClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    LoadScene(FilenameToUriSafe(OpenDialog1.FileName));
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  { Initialize UI scaling in CGE.
    Not really needed in this application, that doesn't have any CGE UI,
    we only use CGE for TCastleScene display in viewport. }
  CastleControl1.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { Initialize references to components designed using CGE editor,
    saved in data/main.castle-user-interface. }
  MainScene := CastleControl1.Container.DesignedComponent('MainScene') as TCastleScene;
  MainViewport := CastleControl1.Container.DesignedComponent('MainViewport') as TCastleViewport;
  LabelFps := CastleControl1.Container.DesignedComponent('LabelFps') as TCastleLabel;

  { Load initial scene. }
  LoadScene('castle-data:/spine_dragon/dragon.json');

  { Initialize OpenDialog1 to allow loading all supported scene formats. }
  FileFiltersToDialog(LoadScene_FileFilters, OpenDialog1);

  { Assign event to some OnUpdate, to update FPS display }
  LabelFps.OnUpdate := DoUpdate;
end;

procedure TForm1.DoUpdate(const Sender: TCastleUserInterface;
  const SecondsPassed: Single; var HandleInput: Boolean);
begin
  { update LabelFps every frame }
  LabelFps.Caption := 'FPS: ' + CastleControl1.Container.Fps.ToString;
end;

procedure TForm1.AnimationButtonClick(Sender: TObject);
begin
  var AnimName := (Sender as TButton).Text;

  MainScene.PlayAnimation(AnimName, true);

  { Example how to use TPlayAnimationParameters to adjust
    TransitionDuration (animation cross-fading). }
  {
  var Params := TPlayAnimationParameters.Create;
  try
    Params.Name := AnimName;
    Params.TransitionDuration := 0.25;
    Params.Loop := true;
    MainScene.PlayAnimation(Params);
  finally
    FreeAndNil(Params);
  end;
  }
end;

procedure TForm1.LoadScene(const Url: String);
begin
  MainScene.Load(Url);

  { Set blending sort following "NavigationInfo.blendingSort" info from scene.
    This means we use 2D sorting e.g. for Spine models by default. }
  if (MainScene.NavigationInfoStack.Top <> nil) and
     (MainScene.NavigationInfoStack.Top.BlendingSort <> sortAuto) then
    MainViewport.BlendingSort := MainScene.NavigationInfoStack.Top.BlendingSort
  else
    MainViewport.BlendingSort := sortAuto;

  MainViewport.AssignDefaultCamera; // adjust camera to scene size

  LayoutAnimations.DeleteChildren;

  for var AnimName in MainScene.AnimationsList do
  begin
    var NewButton := TButton.Create(Self);
    NewButton.Text := AnimName;
    NewButton.OnClick := AnimationButtonClick;
    LayoutAnimations.AddObject(NewButton);
  end;
end;

end.
