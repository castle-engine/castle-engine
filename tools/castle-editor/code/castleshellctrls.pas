{ Fork of Lazarus ShellCtrls unit with some features useful
  for Castle Game Engine editor.

  TODO: In time, we will either submit these modifications to LCL,
  or maybe use some more custom control for file browser in CGE editor
  -- undecided for now.

  Copyright of the modifications 2019-2019 Michalis Kamburelis.
  Copyright of the original code: Lazarus contributors.
  The licence of ShellCtrls is the same as the license of Castle Game Engine
  (LGPL with static linking exception).
}
unit CastleShellCtrls;

{$mode objfpc}{$H+}

{.$define debug_shellctrls}

interface

uses
  Classes, SysUtils,
  // LCL
  Forms, Graphics, ComCtrls, LCLProc, LCLStrConsts,
  // LazUtils
  FileUtil, LazFileUtils, LazUTF8;

{$if defined(Windows) or defined(darwin)}
{$define CaseInsensitiveFilenames}
{$endif}
{$IF defined(CaseInsensitiveFilenames) or defined(darwin)}
{$DEFINE NotLiteralFilenames}
{$ENDIF}

type

  { TObjectTypes }

  TObjectType = (otFolders, otNonFolders, otHidden);

  TObjectTypes = set of TObjectType;

  TFileSortType = (fstNone, fstAlphabet, fstFoldersFirst);

  { Forward declaration of the classes }

  TCustomCastleShellTreeView = class;
  TCustomCastleShellListView = class;

  { TCustomCastleShellTreeView }

  TCustomCastleShellTreeView = class(TCustomTreeView)
  private
    FObjectTypes: TObjectTypes;
    FRoot: String;
    FShellListView: TCustomCastleShellListView;
    FFileSortType: TFileSortType;
    FInitialRoot: String;
    FExcludeMask: String;
    FExcludedCount: Cardinal;
    { Setters and getters }
    function GetPath: String;
    procedure SetFileSortType(const AValue: TFileSortType);
    procedure SetObjectTypes(AValue: TObjectTypes);
    procedure SetPath(AValue: String);
    procedure SetRoot(const AValue: String);
    procedure SetShellListView(const Value: TCustomCastleShellListView);
    procedure SetExcludeMask(const AValue: String);
    procedure RefreshContents;
    procedure ExcludedCountChanged;
  protected
    procedure DoCreateNodeClass(var NewNodeClass: TTreeNodeClass); override;
    procedure Loaded; override;
    function CreateNode: TTreeNode; override;
    { Other methods specific to Lazarus }
    function  PopulateTreeNodeWithFiles(
      ANode: TTreeNode; ANodePath: String): Boolean;
    procedure DoSelectionChanged; override;
    function CanExpand(Node: TTreeNode): Boolean; override;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    { Methods specific to Lazarus - useful for other classes }
    class function  GetBasePath: String;
    function  GetRootPath: String;
    class procedure GetFilesInDir(const ABaseDir: String;
      const AMask, AExcludeMask: String;
      const AObjectTypes: TObjectTypes;
      const AResult: TStrings; const AFileSortType: TFileSortType;
      out ExcludedCount: Cardinal);
    { Other methods specific to Lazarus }
    function  GetPathFromNode(ANode: TTreeNode): String;
    procedure PopulateWithBaseFiles;
    procedure Refresh(ANode: TTreeNode); overload;

    { Properties }
    property ObjectTypes: TObjectTypes read FObjectTypes write SetObjectTypes;
    property ShellListView: TCustomCastleShellListView read FShellListView write SetShellListView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType;
    property Root: String read FRoot write SetRoot;
    property Path: String read GetPath write SetPath;
    property ExcludeMask: String read FExcludeMask write SetExcludeMask;

    { Protected properties which users may want to access, see bug 15374 }
    property Items;
  end;

  { TCastleShellTreeView }

  TCastleShellTreeView = class(TCustomCastleShellTreeView)
  published
    { TCustomTreeView properties }
    property Align;
    property Anchors;
    property AutoExpand;
    property BorderSpacing;
    //property BiDiMode;
    property BackgroundColor;
    property BorderStyle;
    property BorderWidth;
    property Color;
    property Constraints;
    property Enabled;
    property ExcludeMask;
    property ExpandSignType;
    property Font;
    property FileSortType;
    property HideSelection;
    property HotTrack;
    property Images;
    property Indent;
    //property ParentBiDiMode;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Root;
    property RowSelect;
    property ScrollBars;
    property SelectionColor;
    property ShowButtons;
    property ShowHint;
    property ShowLines;
    property ShowRoot;
    property StateImages;
    property TabOrder;
    property TabStop default True;
    property Tag;
    property ToolTips;
    property Visible;
    property OnAdvancedCustomDraw;
    property OnAdvancedCustomDrawItem;
    property OnChange;
    property OnChanging;
    property OnClick;
    property OnCollapsed;
    property OnCollapsing;
    property OnCustomDraw;
    property OnCustomDrawItem;
    property OnDblClick;
    property OnEdited;
    property OnEditing;
    property OnEnter;
    property OnExit;
    property OnExpanded;
    property OnExpanding;
    property OnGetImageIndex;
    property OnGetSelectedIndex;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ifndef VER3_0}
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    {$endif}
    property OnSelectionChanged;
    property OnShowHint;
    property OnUTF8KeyPress;
    property Options;
    property TreeLineColor;
    property TreeLinePenStyle;
    property ExpandSignColor;
    { TCustomCastleShellTreeView properties }
    property ObjectTypes;
    property ShellListView;
  end;

  { TCustomCastleShellListView }

  TCSLVFileAddedEvent = procedure(Sender: TObject; Item: TListItem) of object;

  TCustomCastleShellListView = class(TCustomListView)
  private
    FMask, FExcludeMask: String;
    FObjectTypes: TObjectTypes;
    FRoot: String;
    FShellTreeView: TCustomCastleShellTreeView;
    FOnFileAdded: TCSLVFileAddedEvent;
    FFileSortType: TFileSortType;
    FExcludedCount: Cardinal;
    { Setters and getters }
    procedure SetFileSortType(AValue: TFileSortType);
    procedure SetMask(const AValue: String);
    procedure SetExcludeMask(const AValue: String);
    procedure SetShellTreeView(const Value: TCustomCastleShellTreeView);
    procedure SetRoot(const Value: String);
    procedure ExcludedCountChanged;
    function GetSelectedFileNameInRoot: String;
    procedure SetSelectedFileNameInRoot(const Value: String);
  protected
    { Methods specific to Lazarus }
    procedure PopulateWithRoot();
    procedure Resize; override;
    property OnFileAdded: TCSLVFileAddedEvent read FOnFileAdded write FOnFileAdded;
  public
    { Basic methods }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Methods specific to Lazarus }
    function GetPathFromItem(ANode: TListItem; const Absolute: Boolean = true): String;
    procedure RefreshContents;
    { Properties }
    property Mask: String read FMask write SetMask;
    property ExcludeMask: String read FExcludeMask write SetExcludeMask;
    property ObjectTypes: TObjectTypes read FObjectTypes write FObjectTypes;
    property Root: String read FRoot write SetRoot;
    property ShellTreeView: TCustomCastleShellTreeView read FShellTreeView write SetShellTreeView;
    property FileSortType: TFileSortType read FFileSortType write SetFileSortType;
    { Protected properties which users may want to access, see bug 15374 }
    property Items;
    { Selected name in the list, without any path (the path is in @link(Root)).
      Setting this to non-existing item is ignored without any errors. }
    property SelectedFileNameInRoot: String read GetSelectedFileNameInRoot write SetSelectedFileNameInRoot;
  end;

  { TCastleShellListView }

  TCastleShellListView = class(TCustomCastleShellListView)
  public
    property Columns;
  published
    { TCustomListView properties
      The same as TListView excluding data properties }
    property Align;
    property Anchors;
    property BorderSpacing;
    property BorderStyle;
    property BorderWidth;
//    property Checkboxes;
    property Color default clWindow;
//    property ColumnClick;
    property Constraints;
    property DragCursor;
    property DragMode;
//    property DefaultItemHeight;
//    property DropTarget;
    property Enabled;
//    property FlatScrollBars;
    property FileSortType;
    property Font;
//    property FullDrag;
//    property GridLines;
    property HideSelection;
//    property HotTrack;
//    property HotTrackStyles;
//    property HoverTime;
    property LargeImages;
    property Mask;
    property ExcludeMask;
    property MultiSelect;
//    property OwnerData;
//    property OwnerDraw;
    property ParentColor default False;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RowSelect;
    property ScrollBars;
    property ShowColumnHeaders;
    property ShowHint;
//    property ShowWorkAreas;
    property SmallImages;
    property SortColumn;
    property SortType;
    property StateImages;
    property TabStop;
    property TabOrder;
    property ToolTips;
    property Visible;
    property ViewStyle default vsReport;
//    property OnAdvancedCustomDraw;
//    property OnAdvancedCustomDrawItem;
//    property OnAdvancedCustomDrawSubItem;
    property OnChange;
    property OnClick;
    property OnColumnClick;
    property OnCompare;
    property OnContextPopup;
//    property OnCustomDraw;
//    property OnCustomDrawItem;
//    property OnCustomDrawSubItem;
    property OnDblClick;
    property OnDeletion;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ifndef VER3_0}
    property OnMouseWheelHorz;
    property OnMouseWheelLeft;
    property OnMouseWheelRight;
    {$endif}
    property OnResize;
    property OnSelectItem;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnFileAdded;
    { TCustomCastleShellListView properties }
    property ObjectTypes;
    property Root;
    property ShellTreeView;
  end;

  { TShellTreeNode }

  TShellTreeNode = class(TTreeNode)
  private
    FFileInfo: TSearchRec;
    FBasePath: String;
  protected
    procedure SetBasePath(ABasePath: String);
  public
    function ShortFilename: String;
    function FullFilename: String;
    function IsDirectory: Boolean;

    property BasePath: String read FBasePath;
  end;

  EShellCtrl = class(Exception);
  EInvalidPath = class(EShellCtrl);

function DbgS(OT: TObjectTypes): String; overload;

procedure Register;

const
  { Use these to detect whether TListItem inside TCastleShellListView
    is a directory or not. Compare these values with TListItem.Data value. }
  ShellListItemFile = Pointer(1);
  ShellListItemDirectory = Pointer(2);

implementation

uses {$ifdef windows} Windows, {$endif} StrUtils, CastleStringUtils;

const
  //no need to localize, it's a message for the programmer
  sShellTreeViewIncorrectNodeType = 'TCastleShellTreeView: the newly created node is not a TShellTreeNode!';

  {$warnings off}
  { Hide warnings that faXxx are unportable -- it is as expected. }
  flagHidden = faHidden;
  {$warnings on}

function DbgS(OT: TObjectTypes): String; overload;
begin
  Result := '[';
  if (otFolders in OT) then Result := Result + 'otFolders,';
  if (otNonFolders in OT) then Result := Result + 'otNonFolders,';
  if (otHidden in OT) then Result := Result + 'otHidden';
  if Result[Length(Result)] = ',' then System.Delete(Result, Length(Result), 1);
  Result := Result + ']';
end;

{ TFileItem : internal helper class used for temporarily storing info in an internal TStrings component}
type
  { TFileItem }
  TFileItem = class(TObject)
  private
    FFileInfo: TSearchRec;
    FBasePath: String;
  public
    //more data to sort by size, date... etc
    isFolder: Boolean;
    constructor Create(const DirInfo: TSearchRec; ABasePath: String);
    property FileInfo: TSearchRec read FFileInfo write FFileInfo;
  end;


constructor TFileItem.Create(const DirInfo:TSearchRec; ABasePath: String);
begin
  FFileInfo := DirInfo;
  FBasePath:= ABasePath;
  isFolder:=DirInfo.Attr and FaDirectory > 0;
end;



{ TShellTreeNode }

procedure TShellTreeNode.SetBasePath(ABasePath: String);
begin
  FBasePath := ABasePath;
end;


function TShellTreeNode.ShortFilename: String;
begin
  Result := ExtractFileName(FFileInfo.Name);
  if (Result = '') then Result := FFileInfo.Name;
end;

function TShellTreeNode.FullFilename: String;
begin
  if (FBasePath <> '') then
    Result := AppendPathDelim(FBasePath) + FFileInfo.Name
  else
    //root nodes
    Result := FFileInfo.Name;
  {$if defined(windows) and not defined(wince)}
  if (Length(Result) = 2) and (Result[2] = DriveSeparator) then
    Result := Result + PathDelim;
  {$endif}
end;

function TShellTreeNode.IsDirectory: Boolean;
begin
  Result := ((FFileInfo.Attr and faDirectory) > 0);
end;


{ TCustomCastleShellTreeView }

procedure TCustomCastleShellTreeView.SetShellListView(
  const Value: TCustomCastleShellListView);
var
  Tmp: TCustomCastleShellListView;
begin
  if FShellListView = Value then Exit;

  if Assigned(FShellListView) then
  begin
    Tmp := FShellListView;
    FShellListView := nil;
    Tmp.ShellTreeView := nil;
  end;

  FShellListView := Value;

  // Update the pair, it will then update itself
  // in the setter of this property
  // Updates only if necessary to avoid circular calls of the setters
  if Assigned(Value) and (Value.ShellTreeView <> Self) then
    Value.ShellTreeView := Self;
end;

procedure TCustomCastleShellTreeView.SetExcludeMask(const AValue: String);
begin
  if FExcludeMask = AValue then Exit;
  FExcludeMask := AValue;
  RefreshContents;
end;

procedure TCustomCastleShellTreeView.RefreshContents;
var
  CurrPath: String;
begin
  { Refresh visible files. Implementation copied from original LCL SetObjectTypes }
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Refresh(nil);
    try
       SetPath(CurrPath);
    except
      // CurrPath may have been removed in the mean time by another process, just ignore
      on E: EInvalidPath do ;//
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomCastleShellTreeView.ExcludedCountChanged;
begin
  // TODO
end;

procedure TCustomCastleShellTreeView.DoCreateNodeClass(
  var NewNodeClass: TTreeNodeClass);
begin
  NewNodeClass := TShellTreeNode;
  inherited DoCreateNodeClass(NewNodeClass);
end;

procedure TCustomCastleShellTreeView.Loaded;
begin
  inherited Loaded;
  if (FInitialRoot = '') then
    PopulateWithBaseFiles()
  else
    SetRoot(FInitialRoot);
end;

function TCustomCastleShellTreeView.CreateNode: TTreeNode;
begin
  Result := inherited CreateNode;
  //just in case someone attaches a new OnCreateNodeClass which does not return a TShellTreeNode (sub)class
  if not (Result is TShellTreeNode) then
    Raise EShellCtrl.Create(sShellTreeViewIncorrectNodeType);
end;

procedure TCustomCastleShellTreeView.SetRoot(const AValue: String);
var
  RootNode: TTreeNode;
begin
  if FRoot=AValue then exit;
  if (csLoading in ComponentState) then
  begin
    FInitialRoot := AValue;
    Exit;
  end;
  //Delphi raises an exception in this case, but don't crash the IDE at designtime
  if not (csDesigning in ComponentState)
     and (AValue <> '')
     and not DirectoryExistsUtf8(ExpandFilenameUtf8(AValue)) then
     Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[ExpandFileNameUtf8(AValue)]);
  if (AValue = '') then
    FRoot := GetBasePath
  else
    FRoot:=AValue;
  Items.Clear;
  if FRoot = '' then
  begin
    PopulateWithBaseFiles()
  end
  else
  begin
    //Add a node for Root and expand it (issue #0024230)
    //Make FRoot contain fully qualified pathname, we need it later in GetPathFromNode()
    FRoot := ExpandFileNameUtf8(FRoot);
    //Set RootNode.Text to AValue so user can choose if text is fully qualified path or not
    RootNode := Items.AddChild(nil, ExtractFileName(ExcludeTrailingPathDelimiter(AValue)));
    TShellTreeNode(RootNode).FFileInfo.Attr := FileGetAttr(FRoot);
    TShellTreeNode(RootNode).FFileInfo.Name := FRoot;
    TShellTreeNode(RootNode).SetBasePath('');
    RootNode.HasChildren := True;
    RootNode.Expand(False);
  end;
  if Assigned(ShellListView) then
    ShellListView.Root := FRoot;
end;

// ToDo: Optimize, now the tree is populated in constructor, SetRoot and SetFileSortType.
// For some reason it does not show in performance really.
procedure TCustomCastleShellTreeView.SetFileSortType(const AValue: TFileSortType);
var
  RootNode: TTreeNode;
  CurrPath: String;
begin
  if FFileSortType=AValue then exit;
  FFileSortType:=AValue;
  if (csLoading in ComponentState) then Exit;
  CurrPath := GetPath;
  try
    BeginUpdate;
    Items.Clear;
    if FRoot = '' then
      PopulateWithBaseFiles()
    else
    begin
      RootNode := Items.AddChild(nil, FRoot);
      RootNode.HasChildren := True;
      RootNode.Expand(False);
      try
       SetPath(CurrPath);
      except
        // CurrPath may have been removed in the mean time by another process, just ignore
        on E: EInvalidPath do ;//
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TCustomCastleShellTreeView.SetObjectTypes(AValue: TObjectTypes);
begin
  if FObjectTypes = AValue then Exit;
  FObjectTypes := AValue;
  RefreshContents;
end;

function TCustomCastleShellTreeView.CanExpand(Node: TTreeNode): Boolean;
var
  OldAutoExpand: Boolean;
begin
  Result:=inherited CanExpand(Node);
  if not Result then exit;
  OldAutoExpand:=AutoExpand;
  AutoExpand:=False;
  Node.DeleteChildren;
  Result := PopulateTreeNodeWithFiles(Node, GetPathFromNode(Node));
  AutoExpand:=OldAutoExpand;
end;

constructor TCustomCastleShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FInitialRoot := '';

  // Initial property values
  FObjectTypes:= [otFolders];

  // Populating the base dirs is done in Loaded
end;

destructor TCustomCastleShellTreeView.Destroy;
begin
  ShellListView := nil;
  inherited Destroy;
end;


function FilesSortAlphabet(p1, p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  Result:=CompareText(f1.FileInfo.Name, f2.FileInfo.Name);
end;

function FilesSortFoldersFirst(p1,p2: Pointer): Integer;
var
  f1, f2: TFileItem;
begin
  f1:=TFileItem(p1);
  f2:=TFileItem(p2);
  if f1.isFolder=f2.isFolder then
    Result:=FilesSortAlphabet(p1,p2)
  else begin
    if f1.isFolder then Result:=-1
    else Result:=1;
  end;

end;

function STVCompareFiles(f1, f2: Pointer): integer;
begin
  Result:=CompareFilenames(AnsiString(f1),AnsiString(f2));
end;

{ Does given Name match one of the masks in Masks. }
function ExcludedFileName(const Name: String; const Masks: TStringList): Boolean;
var
  Mask: String;
begin
  for Mask in Masks do
    if IsWild(Name, Mask, not FilenamesCaseSensitive) then
      Exit(true);
  Result := false;
end;

{ Helper routine.
  Finds all files/directories directly inside a directory.
  Does not recurse inside subdirectories.

  AResult will contain TFileItem objects upon return, make sure to free them in the calling routine

  AMask may contain multiple file masks separated by ;
  Don't add a final ; after the last mask.
}
class procedure TCustomCastleShellTreeView.GetFilesInDir(
  const ABaseDir: String; const AMask, AExcludeMask: String;
  const AObjectTypes: TObjectTypes; const AResult: TStrings;
  const AFileSortType: TFileSortType; out ExcludedCount: Cardinal);
var
  DirInfo: TSearchRec;
  FindResult: Integer;
  IsDirectory, IsValidDirectory, IsHidden, AddFile: Boolean;
  SearchStr: String;
  MaskStr: String;
  Files: TList;
  FileItem: TFileItem;
  i: Integer;
  MaskStrings, ExcludeMaskStrings: TStringList;
  //FileTree: TStringList;
  ShortFilename: AnsiString;
  {$if defined(windows) and not defined(wince)}
  ErrMode : UInt32;
  {$endif}
begin
  ExcludedCount := 0;

  {$if defined(windows) and not defined(wince)}
  // disables the error dialog, while enumerating not-available drives
  // for example listing A: path, without diskette present.
  ErrMode:=SetErrorMode(SEM_FAILCRITICALERRORS or SEM_NOALIGNMENTFAULTEXCEPT or SEM_NOGPFAULTERRORBOX or SEM_NOOPENFILEERRORBOX);
  try
  {$endif}

  if Trim(AMask) = '' then MaskStr := AllFilesMask
  else MaskStr := AMask;

  // The string list implements support for multiple masks separated
  // by semi-colon ";"
  MaskStrings := TStringList.Create;
  ExcludeMaskStrings := TStringList.Create;
  try

    {$ifdef NotLiteralFilenames}
    MaskStrings.CaseSensitive := False;
    ExcludeMaskStrings.CaseSensitive := False;
    {$else}
    MaskStrings.CaseSensitive := True;
    ExcludeMaskStrings.CaseSensitive := True;
    {$endif}
    MaskStrings.Delimiter := ';';
    MaskStrings.DelimitedText := MaskStr;
    ExcludeMaskStrings.Delimiter := ';';
    ExcludeMaskStrings.DelimitedText := AExcludeMask;

    if AFileSortType=fstNone then Files:=nil
    else Files:=TList.Create;

    for i := 0 to MaskStrings.Count - 1 do
    begin
      if MaskStrings.IndexOf(MaskStrings[i]) < i then Continue; // From patch from bug 17761: TCastleShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      SearchStr := IncludeTrailingPathDelimiter(ABaseDir) + MaskStrings.Strings[i];

      FindResult := FindFirstUTF8(SearchStr, faAnyFile, DirInfo);

      while FindResult = 0 do
      begin
        ShortFilename := DirInfo.Name;

        IsDirectory := (DirInfo.Attr and FaDirectory = FaDirectory);

        IsValidDirectory := (ShortFilename <> '.') and (ShortFilename <> '..');

        IsHidden := (DirInfo.Attr and flagHidden = flagHidden);

        // First check if we show hidden files
        if IsHidden then AddFile := (otHidden in AObjectTypes)
        else AddFile := True;

        // If it is a directory, check if it is a valid one
        if IsDirectory then
          AddFile := AddFile and ((otFolders in AObjectTypes) and IsValidDirectory)
        else
          AddFile := AddFile and (otNonFolders in AObjectTypes);

        // Take into account ExcludeMaskStrings value
        if AddFile then
        begin
          AddFile := not ExcludedFileName(ShortFilename, ExcludeMaskStrings);
          if not AddFile then
            Inc(ExcludedCount);
        end;

        // AddFile identifies if the file is valid or not
        if AddFile then
        begin
          if not Assigned(Files) then
          begin
            (*
              CGE: Since we only use this with hardcoded masks,
              that do not duplicate extensions, we remove FileTree stuff.
              This way it compiles with older LCL too. }
            if FileTree.Find(Pointer(ShortFilename))=nil then
            begin
              // From patch from bug 17761: TCastleShellListView Mask: duplicated items if mask is " *.ext;*.ext "
              FileTree.Add(Pointer(ShortFilename));
              AResult.AddObject(ShortFilename, TFileItem.Create(DirInfo, ABaseDir));
            end;
            *)
            AResult.AddObject(ShortFilename, TFileItem.Create(DirInfo, ABaseDir));
          end else
            Files.Add ( TFileItem.Create(DirInfo, ABaseDir));
        end;

        FindResult := FindNextUTF8(DirInfo);
      end;

      FindCloseUTF8(DirInfo);
    end;
  finally
    MaskStrings.Free;
    ExcludeMaskStrings.Free;
  end;

  if Assigned(Files) then begin

    case AFileSortType of
      fstNone: ; // do no sorting
      fstAlphabet:     Files.Sort(@FilesSortAlphabet);
      fstFoldersFirst: Files.Sort(@FilesSortFoldersFirst);
    end;

    for i:=0 to Files.Count-1 do
    begin
      FileItem:=TFileItem(Files[i]);
      if (i < Files.Count - 1) and (TFileItem(Files[i]).FileInfo.Name = TFileItem(Files[i + 1]).FileInfo.Name) then
      begin
        FileItem.Free;
        Continue; // cause Files is sorted // From patch from bug 17761: TCastleShellListView Mask: duplicated items if mask is " *.ext;*.ext "
      end;
      AResult.AddObject(FileItem.FileInfo.Name, FileItem);
    end;
    //don't free the TFileItems here, they will freed by the calling routine
    Files.Free;
  end;

  {$if defined(windows) and not defined(wince)}
  finally
     SetErrorMode(ErrMode);
  end;
  {$endif}
end;

class function TCustomCastleShellTreeView.GetBasePath: String;
begin
  {$if defined(windows) and not defined(wince)}
  Result := '';
  {$endif}
  {$ifdef wince}
  Result := '\';
  {$endif}
  {$ifdef unix}
  Result := '/';
  {$endif}
  {$ifdef HASAMIGA}
  Result := '';
  {$endif}
end;

function TCustomCastleShellTreeView.GetRootPath: String;
begin
  if FRoot <> '' then
    Result := FRoot
  else
    Result := GetBasePath();
  if Result <> '' then
    Result := IncludeTrailingPathDelimiter(Result);
end;

{ Returns true if at least one item was added, false otherwise }
function TCustomCastleShellTreeView.PopulateTreeNodeWithFiles(
  ANode: TTreeNode; ANodePath: String): Boolean;
var
  i: Integer;
  Files: TStringList;
  NewNode: TTreeNode;

   function HasSubDir(Const ADir: String): Boolean;
   var
     SR: TSearchRec;
     FindRes: LongInt;
     Attr: Longint;
     IsHidden: Boolean;
   begin
     Result:=False;
     try
       Attr := faDirectory;
       if (otHidden in fObjectTypes) then Attr := Attr or flagHidden;
       FindRes := FindFirstUTF8(AppendPathDelim(ADir) + AllFilesMask, Attr , SR);
       while (FindRes = 0) do
       begin
         if ((SR.Attr and faDirectory <> 0) and (SR.Name <> '.') and
            (SR.Name <> '..')) then
         begin
           IsHidden := ((Attr and flagHidden) > 0);
           if not (IsHidden and (not ((otHidden in fObjectTypes)))) then
           begin
             Result := True;
             Break;
           end;
         end;
         FindRes := FindNextUtf8(SR);
       end;
     finally
       FindCloseUTF8(SR);
     end; //try
   end;

begin
  Result := False;
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  Files := TStringList.Create;
  try
    Files.OwnsObjects := True;
    GetFilesInDir(ANodePath, AllFilesMask, ExcludeMask, FObjectTypes, Files,
      FFileSortType, FExcludedCount);
    ExcludedCountChanged;
    Result := Files.Count > 0;

    for i := 0 to Files.Count - 1 do
    begin
      NewNode := Items.AddChildObject(ANode, Files.Strings[i], nil);
      TShellTreeNode(NewNode).FFileInfo := TFileItem(Files.Objects[i]).FileInfo;
      TShellTreeNode(NewNode).SetBasePath(TFileItem(Files.Objects[i]).FBasePath);

      if (fObjectTypes * [otNonFolders] = []) then
        NewNode.HasChildren := (TShellTreeNode(NewNode).IsDirectory and
                               HasSubDir(AppendpathDelim(ANodePath)+Files[i]))
      else
        NewNode.HasChildren := TShellTreeNode(NewNode).IsDirectory;
    end;
  finally
    Files.Free;
  end;
end;

procedure TCustomCastleShellTreeView.PopulateWithBaseFiles;
{$if defined(windows) and not defined(wince)}
var
  r: UInt32;
  Drives: array[0..128] of char;
  pDrive: PChar;
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;
  Items.Clear;
  r := GetLogicalDriveStrings(SizeOf(Drives), Drives);
  if r = 0 then Exit;
  if r > SizeOf(Drives) then Exit;
//    raise Exception.Create(SysErrorMessage(ERROR_OUTOFMEMORY));
  pDrive := Drives;
  while pDrive^ <> #0 do
  begin
    NewNode := Items.AddChildObject(nil, ExcludeTrailingBackslash(pDrive), pDrive);
    //Yes, we want to remove the backslash,so don't use ChompPathDelim here
    TShellTreeNode(NewNode).FFileInfo.Name := ExcludeTrailingBackslash(pDrive);
    //On NT platforms drive-roots really have these attributes
    {$warnings off} // do not warn that faSysFile unportable
    TShellTreeNode(NewNode).FFileInfo.Attr := faDirectory + faSysFile + flagHidden;
    {$warnings on}
    TShellTreeNode(NewNode).SetBasePath('');
    NewNode.HasChildren := True;
    Inc(pDrive, 4);
  end;
end;
{$else}
var
  NewNode: TTreeNode;
begin
  // avoids crashes in the IDE by not populating during design
  // also do not populate before loading is done
  if ([csDesigning, csLoading] * ComponentState <> []) then Exit;
  Items.Clear;

  // This allows showing "/" in Linux, but in Windows it makes no sense to show the base
  if GetBasePath() <> '' then
  begin
    NewNode := Items.AddChild(nil, GetBasePath());
    NewNode.HasChildren := True;
    PopulateTreeNodeWithFiles(NewNode, GetBasePath());
    NewNode.Expand(False);
  end
  else
    PopulateTreeNodeWithFiles(nil, GetBasePath());
end;
{$endif}

procedure TCustomCastleShellTreeView.DoSelectionChanged;
var
  ANode: TTreeNode;
  CurrentNodePath: String;
begin
  inherited DoSelectionChanged;
  ANode := Selected;
  if Assigned(FShellListView) and Assigned(ANode) then
  begin
    //You cannot rely on HasChildren here, because it can become FALSE when user
    //clicks the expand sign and folder is empty
    //Issue 0027571
    CurrentNodePath := ChompPathDelim(GetPathFromNode(ANode));
    if TShellTreeNode(ANode).IsDirectory then
    begin
      //Note: the folder may have been deleted in the mean time
      //an exception will be raised by the next line in that case
      FShellListView.Root := GetPathFromNode(ANode)
    end
    else
    begin
      if not FileExistsUtf8(CurrentNodePath) then
        Raise EShellCtrl.CreateFmt(sShellCtrlsSelectedItemDoesNotExists,[CurrentNodePath]);
      if Assigned(Anode.Parent) then
        FShellListView.Root := GetPathFromNode(ANode.Parent)
      else
        FShellListView.Root := '';
    end;
  end;
end;

function TCustomCastleShellTreeView.GetPathFromNode(ANode: TTreeNode): String;
begin
  if Assigned(ANode) then
  begin
    Result := TShellTreeNode(ANode).FullFilename;
    if TShellTreeNode(ANode).IsDirectory then
      Result := AppendPathDelim(Result);
    if not FilenameIsAbsolute(Result) then
      Result := GetRootPath() + Result;    // Include root directory
  end
  else
    Result := '';
end;


procedure TCustomCastleShellTreeView.Refresh(ANode: TTreeNode);
//nil will refresh root
var
  RootNodeText: String;
  IsRoot: Boolean;
begin
  if (Items.Count = 0) then Exit;
  {$ifdef debug_shellctrls}
  debugln(['TCustomCastleShellTreeView.Refresh: GetFirstVisibleNode.Text = "',Items.GetFirstVisibleNode.Text,'"']);
  {$endif}

  IsRoot := (ANode = nil) or ((ANode = Items.GetFirstVisibleNode) and (GetRootPath <> ''));
  {$ifdef debug_shellctrls}
  debugln(['IsRoot = ',IsRoot]);
  {$endif}


  if (ANode = nil) and (GetRootPath <> '') then ANode := Items.GetFirstVisibleNode;
  if IsRoot then
  begin
    { Castle: commented this out, it is broken.
      In case we would do Tree.Refresh(Tree.Selected),
      with Tree.Selected <> nil and equal to tree root,
      we would effectively set RootNodeText to the name (but not full path!)
      of the root directory.
      But we need a full path, otherwise SetRoot with it will fail. }
    //if Assigned(ANode) then
    //  RootNodeText := ANode.Text  //this may differ from FRoot, so don't use FRoot here
    //else

    RootNodeText := GetRootPath;
    {$ifdef debug_shellctrls}
    debugln(['IsRoot = TRUE, RootNodeText = "',RootNodeText,'"']);
    {$endif}

    FRoot := #0; //invalidate FRoot
    SetRoot(RootNodeText); //re-initialize the entire tree
  end
  else
  begin
    ANode.Expand(False);
  end;
end;

function TCustomCastleShellTreeView.GetPath: String;
begin
  Result := GetPathFromNode(Selected);
end;

{
SetPath: Path can be
- Absolute like '/usr/lib'
- Relative like 'foo/bar'
  This can be relative to:
  - Self.Root (which takes precedence over)
  - Current directory
}
procedure TCustomCastleShellTreeView.SetPath(AValue: String);
var
  sl: TStringList;
  Node: TTreeNode;
  i: integer;
  FQRootPath, RelPath: String;
  RootIsAbsolute: Boolean;
  IsRelPath: Boolean;

  function GetAdjustedNodeText(ANode: TTreeNode): String;
  begin
    if (ANode = Items.GetFirstVisibleNode) and (FQRootPath <> '') then
    begin
      if not RootIsAbsolute then
        Result := ''
      else
        Result := FQRootPath;
    end
    else Result := ANode.Text;
  end;

  function Exists(Fn: String): Boolean;
  //Fn should be fully qualified
  var
    Attr: LongInt;
  begin
    Result := False;
    Attr := FileGetAttrUtf8(Fn);
    {$ifdef debug_shellctrls}
    debugln(['TCustomCastleShellTreeView.SetPath.Exists: Attr = ', Attr]);
    {$endif}
    if (Attr = -1) then Exit;
    if not (otNonFolders in FObjectTypes) then
      Result := ((Attr and faDirectory) > 0)
    else
      Result := True;
    {$ifdef debug_shellctrls}
    debugln(['TCustomCastleShellTreeView.SetPath.Exists: Result = ',Result]);
    {$endif}
  end;

  function PathIsDriveRoot({%H-}Path: String): Boolean;  {$if not (defined(windows) and not defined(wince))}inline;{$endif}
  //WinNT filesystem reports faHidden on all physical drive-roots (e.g. C:\)
  begin
    {$if defined(windows) and not defined(wince)}
    Result := (Length(Path) = 3) and
              (Upcase(Path[1]) in ['A'..'Z']) and
              (Path[2] = DriveSeparator) and
              (Path[3] in AllowDirectorySeparators);
    {$else}
    Result := False;
    {$endif windows}
  end;

  function ContainsHiddenDir(Fn: String): Boolean;
  var
    i: Integer;
    Attr: LongInt;
    Dirs: TStringList;
    RelPath: String;
  begin
    //if fn=root then always return false
    if (CompareFileNames(Fn, FQRootPath) = 0) then
      Result := False
    else
    begin
      Attr := FileGetAttrUtf8(Fn);
      Result := ((Attr and flagHidden) = flagHidden) and not PathIsDriveRoot(Fn);
      if not Result then
      begin
        //it also is not allowed that any folder above is hidden
        Fn := ChompPathDelim(Fn);
        Fn := ExtractFileDir(Fn);
        Dirs := TStringList.Create;
        try
          Dirs.StrictDelimiter := True;
          Dirs.Delimiter := PathDelim;
          Dirs.DelimitedText := Fn;
          Fn := '';
          for i := 0 to Dirs.Count - 1 do
          begin
            if (i = 0) then
              Fn := Dirs.Strings[i]
            else
              Fn := Fn + PathDelim + Dirs.Strings[i];
            if (Fn = '') then Continue;
            RelPath := CreateRelativePath(Fn, FQRootPath, False, True);
            //don't check if Fn now is "higher up the tree" than the current root
            if (RelPath = '') or ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.')) then
            begin
              {$ifdef debug_shellctrls}
              debugln(['TCustomCastleShellTreeView.SetPath.ContainsHidden: Fn is higher: ',Fn]);
              {$endif}
              Continue;
            end;
            {$if defined(windows) and not defined(wince)}
            if (Length(Fn) = 2) and (Fn[2] = ':') then Continue;
            {$endif}
            Attr := FileGetAttrUtf8(Fn);
            if (Attr <> -1) and ((Attr and flagHidden) > 0) and not PathIsDriveRoot(Fn) then
            begin
              Result := True;
              {$ifdef debug_shellctrls}
              debugln(['TCustomCastleShellTreeView.SetPath.Exists: a subdir is hidden: Result := False']);
              {$endif}
              Break;
            end;
          end;
        finally
          Dirs.Free;
        end;
      end;
    end;
  end;

begin
  RelPath := '';

  {$ifdef debug_shellctrls}
  debugln(['SetPath: GetRootPath = "',getrootpath,'"',' AValue=',AValue]);
  {$endif}
  if (GetRootPath <> '') then
    //FRoot is already Expanded in SetRoot, just add PathDelim if needed
    FQRootPath := AppendPathDelim(GetRootPath)
  else
    FQRootPath := '';
  RootIsAbsolute := (FQRootPath = '') or (FQRootPath = PathDelim)
                    or ((Length(FQRootPath) = 3) and (FQRootPath[2] = ':') and (FQRootPath[3] = PathDelim));

  {$ifdef debug_shellctrls}
  debugln(['SetPath: FQRootPath = ',fqrootpath]);
  debugln(['SetPath: RootIsAbsolute = ',RootIsAbsolute]);
  debugln(['SetPath: FilenameIsAbsolute = ',FileNameIsAbsolute(AValue)]);
  {$endif}

  if not FileNameIsAbsolute(AValue) then
  begin
    if Exists(FQRootPath + AValue) then
    begin
      //Expand it, since it may be in the form of ../../foo
      AValue := ExpandFileNameUtf8(FQRootPath + AValue);
    end
    else
    begin
      //don't expand Avalue yet, we may need it in error message
      if not Exists(ExpandFileNameUtf8(AValue)) then
        Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[ExpandFileNameUtf8(FQRootPath + AValue)]);
      //Directory (or file) exists
      //Make it fully qualified
      AValue := ExpandFileNameUtf8(AValue);
    end;
  end
  else
  begin
    //AValue is an absoulte path to begin with
    //if not DirectoryExistsUtf8(AValue) then
    if not Exists(AValue) then
      Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue]);
  end;

  //AValue now is a fully qualified path and it exists
  //Now check if it is a subdirectory of FQRootPath
  //RelPath := CreateRelativePath(AValue, FQRootPath, False);
  IsRelPath := (FQRootPath = '') or TryCreateRelativePath(AValue, FQRootPath, False, True, RelPath);

  {$ifdef debug_shellctrls}
  debugln('TCustomCastleShellTreeView.SetPath: ');
  debugln(['  IsRelPath = ',IsRelPath]);
  debugln(['  RelPath = "',RelPath,'"']);
  debugln(['  FQRootPath = "',FQRootPath,'"']);
  {$endif}

  if (not IsRelpath) or ((RelPath <> '') and ((Length(RelPath) > 1) and (RelPath[1] = '.') and (RelPath[2] = '.'))) then
  begin
    // CreateRelativePath retruns a string beginning with ..
    // so AValue is not a subdirectory of FRoot
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPathRelative,[AValue, FQRootPath]);
  end;

  if (RelPath = '') and (FQRootPath = '') then
    RelPath := AValue;
  {$ifdef debug_shellctrls}
  debugln(['RelPath = ',RelPath]);
  {$endif}

  if (RelPath = '') then
  begin
    {$ifdef debug_shellctrls}
    debugln('Root selected');
    {$endif}
    Node := Items.GetFirstVisibleNode;
    if Assigned(Node) then
    begin
      Node.Expanded := True;
      Node.Selected := True;
    end;
    Exit;
  end;

  if not (otHidden in FObjectTypes) and ContainsHiddenDir(AValue) then
    Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidPath,[AValue, FQRootPath]);

  sl := TStringList.Create;
  sl.Delimiter := PathDelim;
  sl.StrictDelimiter := True;
  sl.DelimitedText := RelPath;
  if (sl.Count > 0) and (sl[0] = '') then  // This happens when root dir is empty
    sl[0] := PathDelim;                    //  and PathDelim was the first char
  if (sl.Count > 0) and (sl[sl.Count-1] = '') then sl.Delete(sl.Count-1); //remove last empty string
  if (sl.Count = 0) then
  begin
    sl.Free;
    Exit;
  end;

  {$ifdef debug_shellctrls}
  for i := 0 to sl.Count - 1 do debugln(['sl[',i,']="',sl[i],'"']);
  {$endif}



  BeginUpdate;
  try
    Node := Items.GetFirstVisibleNode;
    {$ifdef debug_shellctrls}
    if assigned(node) then debugln(['GetFirstVisibleNode = ',GetAdjustedNodeText(Node)]);
    {$endif}
    //Root node doesn't have Siblings in this case, we need one level down the tree
    if (GetRootPath <> '') and Assigned(Node) then
    begin
      {$ifdef debug_shellctrls}
      debugln('Root node doesn''t have Siblings');
      {$endif}
      Node := Node.GetFirstVisibleChild;
      {$ifdef debug_shellctrls}
      debugln(['Node = ',GetAdjustedNodeText(Node)]);
      {$endif}
      //I don't know why I wrote this in r44893, but it seems to be wrong so I comment it out
      //for the time being (2015-12-05: BB)
      //if RootIsAbsolute then sl.Delete(0);
    end;

    for i := 0 to sl.Count-1 do
    begin
      {$ifdef debug_shellctrls}
      DbgOut(['i=',i,' sl[',i,']=',sl[i],' ']);
      if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
      else  DbgOut('Node = NIL');
      debugln;
      {$endif}
      while (Node <> Nil) and
            {$IF defined(CaseInsensitiveFilenames) or defined(NotLiteralFilenames)}
            (Utf8LowerCase(GetAdjustedNodeText(Node)) <> Utf8LowerCase(sl[i]))
            {$ELSE}
            (GetAdjustedNodeText(Node) <> sl[i])
            {$ENDIF}
            do
            begin
              {$ifdef debug_shellctrls}
              DbgOut(['  i=',i,' "',GetAdjustedNodeText(Node),' <> ',sl[i],' -> GetNextVisibleSibling -> ']);
              {$endif}
              Node := Node.GetNextVisibleSibling;
              {$ifdef debug_shellctrls}
              if Node <> nil then DbgOut(['GetAdjustedNodeText = ',GetAdjustedNodeText(Node)])
              else DbgOut('Node = NIL');
              debugln;
              {$endif}
            end;
      if Node <> Nil then
      begin
        Node.Expanded := True;
        Node.Selected := True;
        Node := Node.GetFirstVisibleChild;
      end
      else
        Break;
    end;
  finally
    sl.free;
    EndUpdate;
  end;
end;


{ TCustomCastleShellListView }

procedure TCustomCastleShellListView.SetShellTreeView(
  const Value: TCustomCastleShellTreeView);
var
  Tmp: TCustomCastleShellTreeView;
begin
  if FShellTreeView = Value then Exit;
  if FShellTreeView <> nil then
  begin
    Tmp := FShellTreeView;
    FShellTreeView := nil;
    Tmp.ShellListView := nil;
  end;

  FShellTreeView := Value;

  if not (csDestroying in ComponentState) then
    Clear;

  if Value <> nil then
  begin
    FRoot := Value.GetPathFromNode(Value.Selected);
    PopulateWithRoot();

    // Also update the pair, but only if necessary to avoid circular calls of the setters
    if Value.ShellListView <> Self then Value.ShellListView := Self;
  end;

end;

procedure TCustomCastleShellListView.SetMask(const AValue: String);
begin
  if AValue <> FMask then
  begin
    FMask := AValue;
    RefreshContents;
  end;
end;

procedure TCustomCastleShellListView.SetFileSortType(AValue: TFileSortType);
begin
  if FFileSortType <> AValue then
  begin
    FFileSortType := AValue;
    RefreshContents;
  end;
end;

procedure TCustomCastleShellListView.SetExcludeMask(const AValue: String);
begin
  if AValue <> FExcludeMask then
  begin
    FExcludeMask := AValue;
    RefreshContents;
  end;
end;

procedure TCustomCastleShellListView.SetRoot(const Value: String);
begin
  if FRoot <> Value then
  begin
    //Delphi raises an unspecified exception in this case, but don't crash the IDE at designtime
    if not (csDesigning in ComponentState)
       and (Value <> '')
       and not DirectoryExistsUtf8(ExpandFilenameUtf8(Value)) then
       Raise EInvalidPath.CreateFmt(sShellCtrlsInvalidRoot,[Value]);
    FRoot := Value;
    RefreshContents;
  end;
end;

procedure TCustomCastleShellListView.RefreshContents;
begin
  Clear;
  Items.Clear;
  PopulateWithRoot();
end;

procedure TCustomCastleShellListView.ExcludedCountChanged;
begin
  // TODO
end;

constructor TCustomCastleShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Initial property values
  ViewStyle := vsReport;
  ObjectTypes := [otNonFolders];

  Self.Columns.Add;
  Self.Columns.Add;
  Self.Columns.Add;
  Self.Column[0].Caption := sShellCtrlsName;
  Self.Column[1].Caption := sShellCtrlsSize;
  Self.Column[2].Caption := sShellCtrlsType;
  // Initial sizes, necessary under Windows CE
  Resize;
end;

destructor TCustomCastleShellListView.Destroy;
begin
  ShellTreeView := nil;
  inherited Destroy;
end;

procedure TCustomCastleShellListView.PopulateWithRoot();
var
  i: Integer;
  Files: TStringList;
  NewItem: TListItem;
  CurFileName, CurFilePath: String;
  CurFileSize: Int64;
begin
  // avoids crashes in the IDE by not populating during design
  if (csDesigning in ComponentState) then Exit;

  // Check inputs
  if Trim(FRoot) = '' then Exit;

  Files := TStringList.Create;
  try
    Files.OwnsObjects := True;
    TCustomCastleShellTreeView.GetFilesInDir(FRoot, FMask, FExcludeMask,
      FObjectTypes, Files, FFileSortType, FExcludedCount);
    ExcludedCountChanged;

    for i := 0 to Files.Count - 1 do
    begin
      NewItem := Items.Add;
      CurFileName := Files.Strings[i];
      CurFilePath := IncludeTrailingPathDelimiter(FRoot) + CurFileName;
      if DirectoryExists(CurFilePath) then
      begin
        NewItem.Caption := CurFileName;
        NewItem.SubItems.Add('');
        NewItem.SubItems.Add('');
        NewItem.Data := ShellListItemDirectory;
        NewItem.ImageIndex := 0;
      end else
      begin
        // First column - Name
        NewItem.Caption := CurFileName;
        // Second column - Size
        // The raw size in bytes is stored in the data part of the item
        CurFileSize := FileSize(CurFilePath); // in Bytes
        NewItem.Data := Pointer(PtrInt(CurFileSize));
        if CurFileSize < 1024 then
          NewItem.SubItems.Add(Format(sShellCtrlsBytes, [IntToStr(CurFileSize)]))
        else if CurFileSize < 1024 * 1024 then
          NewItem.SubItems.Add(Format(sShellCtrlsKB, [IntToStr(CurFileSize div 1024)]))
        else
          NewItem.SubItems.Add(Format(sShellCtrlsMB, [IntToStr(CurFileSize div (1024 * 1024))]));
        // Third column - Type
        NewItem.SubItems.Add(ExtractFileExt(CurFileName));
        NewItem.Data := ShellListItemFile;
      end;
      if Assigned(FOnFileAdded) then FOnFileAdded(Self,NewItem);
    end;
    Sort;
  finally
    Files.Free;
  end;
end;

procedure TCustomCastleShellListView.Resize;
begin
  inherited Resize;
  {$ifdef DEBUG_SHELLCTRLS}
    debugln(':>TCustomCastleShellListView.HandleResize');
  {$endif}

  // The correct check is with count,
  // if Column[0] <> nil then
  // will raise an exception
  if Self.Columns.Count < 3 then Exit;

  // If the space available is small,
  // alloc a larger percentage to the secondary
  // fields
  if Width < 400 then
  begin
    Column[0].Width := (50 * Width) div 100;
    Column[1].Width := (25 * Width) div 100;
    Column[2].Width := (25 * Width) div 100;
  end
  else
  begin
    Column[0].Width := (70 * Width) div 100;
    Column[1].Width := (15 * Width) div 100;
    Column[2].Width := (15 * Width) div 100;
  end;

  {$ifdef DEBUG_SHELLCTRLS}
    debugln([':<TCustomCastleShellListView.HandleResize C0.Width=',
     Column[0].Width, ' C1.Width=', Column[1].Width,
     ' C2.Width=', Column[2].Width]);
  {$endif}
end;

function TCustomCastleShellListView.GetPathFromItem(ANode: TListItem; const Absolute: Boolean): String;
var
  FileName: String;
begin
  FileName := ANode.Caption;
  if Absolute then
    Result := IncludeTrailingPathDelimiter(FRoot) + FileName
  else
    Result := FileName;
end;

function TCustomCastleShellListView.GetSelectedFileNameInRoot: String;
begin
  if Selected <> nil then
    Result := GetPathFromItem(Selected, false)
  else
    Result := '';
end;

procedure TCustomCastleShellListView.SetSelectedFileNameInRoot(const Value: String);
begin
  if Value = '' then
    Selected := nil
  else
  begin
    // See LCL source lazarus/lcl/include/listitems.inc for the meaning of Items.FindCaption parameters.
    // Items.FindCaption returns nil when not found.
    Selected := Items.FindCaption(
      0, // StartIndex, 0 to search all items
      Value,
      false, // Partial, would find values that only contain Value
      true, // Inclusive, otherwise StartIndex would actually be interpreted as 1
      false, // Wrap, ignored when StartIndex = 0
      false // PartStart, ignored when Partial = false (would make Partial to match only prefix)
    );
  end;
end;

procedure Register;
begin
  RegisterComponents('Castle', [TCastleShellTreeView, TCastleShellListView]);
end;

end.
