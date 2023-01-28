{
  Go trough every xml doc to be transformed and if some
  link is in TOC change it to html/somefile.html

  Add attrib ref to links - it means that link is targeted to
  reference documentation.

  Addd <producer> tag to xml files to be transformed and
  put this to it for XHTML:
  <script language="javascript">
        if (top.frames.length == 0)
          top.location = '../imaging.html';
      </script>

}

unit XHTMLProducer;

{$I ImagingOptions.inc}

interface

uses
  SysUtils, Classes, Helpers, DemoUtils;

const
  sHTMLStyleSheet = 'doc2html.xsl';
  sHTMLTOCStyleSheet = 'toc2html.xsl';
  sHTMLProjectStyleSheet = 'proj2html.xsl';
  sHTMLExt = 'html';
  sHTMLDir = 'html';
  sHTMLDelim = '/';
  sTempFile = 'temp.xml';
  sDocTempFile = 'doctemp.xsl';
  sTocTempFile = 'toctemp.xsl';

type
  TXHTMLProducer = class(TDocProducer)
  private
    FOutDir: string;
    FRootDir: string;
    FProject: TDocProject;
    FTocTempPath: string;
    FDocTempPath: string;
    procedure CreateContentsFrame;
    procedure CreateMainPages;
    procedure ConvertPagesToHTML(Item: TContentItem);
  public
    constructor Create;
    procedure Process(Project: TDocProject; const OutDir: string); override;
  end;

function CreateHTMLLinker: TLinkChecker;

implementation

function LowerCaseLink(const S, Context: string): string;
begin
  Result := LowerCase(S);
end;

function CreateHTMLLinker: TLinkChecker;
begin
  Result := TLinkChecker.Create;
  Result.StripDir := True;
  Result.Extension := sHTMLExt;
  Result.NewPathDelim := sHTMLDelim;
  Result.CustomConverter := LowerCaseLink;
end;

{ TXHTMLProducer }

constructor TXHTMLProducer.Create;
begin
  inherited Create;
  FName := 'XHTML Doc Producer';
end;

procedure TXHTMLProducer.CreateContentsFrame;
var
  Input, Style, Output: string;
  Linker: TLinkChecker;
  XSLLinker: TXSLLinker;
begin
  Msg('Creating contents frame ...');
  Input := FProject.ContentsFile;
  Style := FProject.XslDir + PathDelim + sHTMLTOCStyleSheet;
  Output := LowerCase(FOutDir + PathDelim + sHTMLDir + PathDelim +
    ExtractFileName(ChangeFileExt(FProject.ContentsFile, '.' + sHTMLExt)));

  XSLLinker := TXSLLinker.Create;
  XSLLinker.IntendedOutput := Output;
  XSLLinker.CheckDocument(Style, FProject);
  XSLLinker.SaveResult(FTocTempPath);

  Linker := CreateHTMLLinker;
  Linker.CheckDocument(Input, FProject);
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, FTocTempPath);

  Linker.DeleteResult;
  Linker.Free;

  XSLLinker.DeleteResult;
  XSLLinker.Free;
end;

procedure TXHTMLProducer.CreateMainPages;
var
  Input, Style, Output: string;
  Linker: TLinkChecker;
begin
  Msg('Creating main page ...');
  Input := FProject.ProjectFile;
  Style := FProject.XslDir + PathDelim + sHTMLProjectStyleSheet;
  Output := FOutDir + PathDelim + LowerCase(FProject.OutputFile) + '.' + sHTMLExt;

  Linker := CreateHTMLLinker;
  Linker.DestDir := sHTMLDir;
  Linker.CheckDocument(Input, FProject);
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, Style);

  Linker.DeleteResult;
  Linker.Free;
end;

procedure TXHTMLProducer.ConvertPagesToHTML(Item: TContentItem);
var
  I: LongInt;
  Input, Output: string;
  Linker: TLinkChecker;
begin
  Input := ExpandFileTo(Item.URL, FRootDir);
  Output := LowerCase(FOutDir + PathDelim + sHTMLDir + PathDelim +
    ChangeFileExt(ExtractFileName(Item.URL), '.' + sHTMLExt));

  Linker := CreateHTMLLinker;
  Linker.IntendedOutput := Output;
  Linker.CheckDocument(Input, FProject);
  Linker.SaveResult(FOutDir + PathDelim + sTempFile);

  TransformDoc(FOutDir + PathDelim + sTempFile, Output, FDocTempPath);

  Linker.DeleteResult;
  Linker.Free;

  for I := 0 to Item.ChildCount - 1 do
    ConvertPagesToHTML(Item[I]);
end;

procedure TXHTMLProducer.Process(Project: TDocProject; const OutDir: string);
var
  Style: string;
  Linker: TXSLLinker;
begin
  FOutDir := LowerCase(OutDir);
  FProject := Project;
  FRootDir := LowerCase(ExtractFileDir(FProject.ProjectFile));

  if (not ForceDirectories(FOutDir)) or
    (not ForceDirectories(FOutDir + PathDelim + sHTMLDir)) then
    raise Exception.Create('Cannot create output directories.');

  FTocTempPath := FOutDir + PathDelim + sTocTempFile;
  FDocTempPath := FOutDir + PathDelim + sDocTempFile;

  CreateContentsFrame;
  CreateMainPages;

  Style := FProject.XslDir + PathDelim + sHTMLStyleSheet;
  Linker := TXSLLinker.Create;
  Linker.IntendedOutput := FOutDir + PathDelim + sHTMLDir + PathDelim + 'bla.blo';
  Linker.CheckDocument(Style, FProject);
  Linker.SaveResult(FDocTempPath);

  Msg('Converting pages to HTML ...');
  ConvertPagesToHTML(FProject.Contents);

  Linker.DeleteResult;
  Linker.Free;
end;


end.
