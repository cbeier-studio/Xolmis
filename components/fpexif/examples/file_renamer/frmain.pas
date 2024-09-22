unit frMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ShellCtrls,
  ExtCtrls, StdCtrls, ComCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BtnRename: TButton;
    BtnClose: TButton;
    BtnCheckAll: TButton;
    BtnCheckNone: TButton;
    BtnInfo: TButton;
    Splitter2: TSplitter;
    ThumbImg: TImage;
    ImageList: TImageList;
    FileListView: TListView;
    Panel1: TPanel;
    Panel2: TPanel;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    procedure BtnCheckAllClick(Sender: TObject);
    procedure BtnCheckNoneClick(Sender: TObject);
    procedure BtnRenameClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure BtnInfoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FileListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewGetSelectedIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewSelectionChanged(Sender: TObject);
  private
    function ExtractExifDate(AFileName: String): TDateTime;
    procedure PopulateListview;
    function RemoveDateFromFilename(AFileName: String): String;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  FileUtil, IniFiles,
  fpeMetadata, fpeExifData, fpeTags;

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;


{ TForm1 }

procedure TForm1.BtnCheckAllClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FileListView.Items.Count-1 do
    FileListView.Items[i].Checked := true;
end;

procedure TForm1.BtnCheckNoneClick(Sender: TObject);
var
  i: Integer;
begin
  for i:=0 to FileListView.Items.Count-1 do
    FileListView.Items[i].Checked := false;
end;

procedure TForm1.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm1.BtnInfoClick(Sender: TObject);
begin
  ShowMessage(
    'This program renames the checked files. It extracts the date/time from '+
    'the EXIF metadata and puts it in front of the original file name.'
  );
end;

procedure TForm1.BtnRenameClick(Sender: TObject);
var
  oldname: String;
  newname: String;
  i: Integer;
  n: Integer;
begin
  n := 0;
  for i:=0 to FileListView.Items.Count-1 do begin
    newname := FileListView.Items[i].SubItems[0];
    if (newname <> '') and FileListView.Items[i].Checked then begin
      newname := ShellTreeView.Path + newname;
      oldname := ShellTreeView.Path + RemoveDateFromFilename(FileListView.Items[i].Caption);
      if not FileExists(newname) then begin
        RenameFile(oldname, newname);
        inc(n);
      end;
    end;
  end;
  PopulateListview;
  ShowMessage(IntToStr(n) + ' files renamed.');
end;

function TForm1.ExtractExifDate(AFileName: String): TDateTime;
var
  imginfo: TImgInfo;
  lTag: TTag;
begin
  Result := 0;
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadfromFile(AFileName);
    if not imgInfo.HasExif then
      exit;
    lTag := imgInfo.ExifData.TagByName['DateTimeOriginal'];
    if lTag = nil then
      lTag := imgInfo.ExifData.TagByName['DateTime'];
    if lTag = nil then
      lTag := imgInfo.ExifData.TagByName['DateTimeDigitized'];
    if lTag is TDateTimeTag then
      Result := TDateTimeTag(lTag).AsDateTime;
  finally
    imgInfo.Free;
  end;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
var
  ini: TCustomIniFile;
begin
  if CanClose then
    ini := CreateIni;
    try
      ini.WriteString('Config', 'Path', ShellTreeView.Path);
    finally
      ini.Free;
    end;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  ini: TCustominiFile;
begin
  ini := CreateIni;
  try
    ShellTreeView.Path := ini.ReadString('Config', 'Path', '');
  finally
    ini.Free;
  end;
end;

procedure TForm1.FileListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  imgInfo: TImgInfo;
  ms: TMemoryStream;
begin
  if not Selected then
    exit;

  Screen.Cursor := crHourglass;
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(ShellTreeView.Path + Item.Caption);
    if not (imgInfo.HasThumbnail) then begin
      ThumbImg.Picture.LoadFromFile(ShellTreeView.Path + Item.Caption);
      exit;
    end;

    ms := TMemoryStream.Create;
    try
      imgInfo.SaveThumbnailToStream(ms);
      if ms.Size > 0 then begin
        ms.Position := 0;
        ThumbImg.Picture.LoadFromStream(ms);
      end else
        ThumbImg.Picture.Clear;
    finally
      ms.Free;
    end;
  finally
    imgInfo.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TForm1.PopulateListview;
const
  DATETIME_MASK = 'yyyymmdd-hhnnss';
var
  L: TStrings;
  i: Integer;
  dt: TDateTime;
  oldname, newname: String;
begin
  Screen.Cursor := crHourglass;
  FileListView.Items.Clear;
  L := TStringList.Create;
  try
    FindAllFiles(L, ShellTreeView.Path, '*.jpg;*.jpeg', false);
    for i:=0 to L.Count-1 do begin
      dt := ExtractExifDate(L[i]);
      oldname := ExtractFileName(L[i]);
      if dt <> 0 then
        newname := FormatDateTime(DATETIME_MASK, dt) + ' '  + oldname
      else
        newname := '';
      with FileListView.Items.Add do begin
        Caption := oldname;
        SubItems.Add(newname);
        Checked := true;
      end;
    end;

  finally
    L.Free;
    Screen.Cursor := crDefault;
  end;
end;

function TForm1.RemoveDateFromFilename(AFileName: String): String;
var
  sy, sm, sd, sh, sn, ss: String;
  vy, vm, vd, vh, vn, vs: Integer;
  fd, ft: TDateTime;
begin
  sy := Copy(AFileName, 1, 4);
  sm := Copy(AFileName, 5, 2);
  sd := Copy(AFileName, 7, 2);
  sh := Copy(AFileName, 10, 2);
  sn := Copy(AFileName, 12, 2);
  ss := Copy(AFileName, 14, 2);
  if TryStrToInt(sy, vy) and TryStrToInt(sm, vm) and TryStrToInt(sd, vd) and
     TryStrToInt(sh, vh) and TryStrToInt(sn, vn) and TryStrToInt(ss, vs) and
     TryEncodeDate(vy,vm,vd, fd) and TryEncodeTime(vh,vn,vs,0, ft)
  then
    Result := trim(Copy(AFileName, 16, Length(AFileName)))
  else
    Result := AFilename;
end;

procedure TForm1.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
begin
  if Node = nil then
    exit;
  if Node.Level = 0 then
    Node.ImageIndex := 0
  else
  if Node.Expanded then
    Node.ImageIndex := 2
  else
    Node.ImageIndex := 1;
  Node.SelectedIndex := Node.ImageIndex;
end;

procedure TForm1.ShellTreeViewGetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
begin
  ShellTreeviewGetImageIndex(nil, Node);
end;

procedure TForm1.ShellTreeViewSelectionChanged(Sender: TObject);
begin
  PopulateListview;
end;

end.

