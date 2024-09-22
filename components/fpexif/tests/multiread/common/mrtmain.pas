unit mrtmain;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  FileUtil,
 {$ELSE}
  Windows, ImgList, {$IFDEF UNICODE}ImageList,{$ENDIF}
 {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, 
  fpeMetaData;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    BtnReadFiles: TButton;
    BtnCreateTxtFiles: TButton;
    BtnRunTest: TButton;
    BtnUncheckAll: TButton;
    BtnCheckAll: TButton;
    EdImageDir: TEdit;
    StateImages: TImageList;
    MismatchInfo: TLabel;
    Memo: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    FileTreeView: TTreeView;
    Panel4: TPanel;
    Panel5: TPanel;
    Splitter1: TSplitter;
    ImageList1: TImageList;
    BtnInfo: TButton;
    procedure BtnInfoClick(Sender: TObject);
    procedure BtnReadFilesClick(Sender: TObject);
    procedure BtnRunTestClick(Sender: TObject);
    procedure BtnCreateTxtFilesClick(Sender: TObject);
    procedure BtnUncheckAllClick(Sender: TObject);
    procedure InfoClick(Sender: TObject);
    procedure FileTreeViewClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTotalCount: Integer;
    FMismatchCount: Integer;
    function CreateRefTags(ANode: TTreeNode; AFileName: String): Boolean;
    function ExtractRefTags(ANode: TTreeNode; AList: TStringList): Boolean;
    function GetImageDir: String;
    procedure Log(AMsg: String);
    procedure RunTest(ANode: TTreeNode);
  public

  end;

var
  MainForm: TMainForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}

uses
 {$IFDEF FPC}
  Process, StrUtils,
 {$ELSE}
  ShellApi,
 {$ENDIF}
  fpeGlobal, fpeUtils, fpeTags, fpeExifData;

{ TMainForm }

const
  EXIFTOOL_CMD = '..\..\tools\exiftool.exe';

  IMG_INDEX_WORKING = 0;
  IMG_INDEX_FAIL = 1;
  IMG_INDEX_IGNORE = 1;
  IMG_INDEX_EXIF = 2;
  IMG_INDEX_SUCCESS = 3;

  IMG_UNCHECKED = 2; //0;
  IMG_CHECKED = 1;

{ Finds all image files in the image folder (--> GetImageDir). For every image
  there is a text file containing the meta data written by ExifTool. Reads this
  reference file and stores the meta data in the nodes af the FileTreeView. }
procedure TMainForm.BtnReadFilesClick(Sender: TObject);
var
  info: TSearchRec;
  imgDir: String;
  node: TTreeNode;
  tagFile: String;
  L: TStringList;
  s: String;
begin
  FileTreeView.Items.Clear;
  imgDir := GetImageDir;
  if FindFirst(imgDir + '*.jpg', faAnyFile and faDirectory, info) = 0 then
  begin
    repeat
      if (info.Name <> '.') and (info.Name <> '..') and (info.Attr and faDirectory = 0) then
      begin
        node := FileTreeview.Items.AddChild(nil, ExtractFileName(info.Name));
        node.ImageIndex := IMG_INDEX_IGNORE;
        tagFile := ChangeFileExt(imgDir + info.Name, '.txt');
        if FileExists(tagFile) then
        begin
          L := TStringList.Create;
          try
            L.LoadFromFile(tagFile);
            // Note: ExifTool wrote the file in UTF8 --> We must convert this for Delphi
           {$IFNDEF FPC}
            {$IFDEF UNICODE}
            L.Text := UTF8Decode(L.Text);
            {$ELSE}
            s := L.Text;
//            s := UTF8Decode(s);
            s := fpeUtils.UTF8ToAnsi(s);
            L.Text := s;
            {$ENDIF}
           {$ENDIF}
            if ExtractRefTags(node, L) then begin
              node.ImageIndex := IMG_INDEX_EXIF;
              node.StateIndex := IMG_CHECKED;
            end;
          finally
            L.Free;
          end;
        end;
        node.SelectedIndex := node.ImageIndex;
      end;
    until FindNext(info) <> 0;
  end;
  FindClose(info);
end;

procedure TMainForm.BtnRunTestClick(Sender: TObject);
var
  node: TTreeNode;
begin
  Memo.Lines.Clear;
  FMismatchCount := 0;
  FTotalCount := 0;
  node := FileTreeView.Items.GetFirstNode;
  while node <> nil do begin
    RunTest(node);
    node := node.GetNextSibling;
  end;
  if FTotalCount = 0 then
    MismatchInfo.Caption := 'No tests executed.'
  else
    MismatchInfo.Caption := Format('%d mismatches out of %d tests (%.0f%%)', [
      FMismatchCount, FTotalCount, FMismatchCount/FTotalCount*100]);
  MismatchInfo.Show;
end;

procedure TMainForm.BtnUncheckAllClick(Sender: TObject);
var
  node: TTreeNode;
  checkNode: Boolean;
begin
  checkNode := (Sender = BtnCheckAll);
  node := FileTreeView.Items.GetFirstNode;
  while node <> nil do begin
    if checkNode and (node.StateIndex = IMG_UNCHECKED) then
      node.StateIndex := IMG_CHECKED
    else if not checkNode and (node.StateIndex = IMG_CHECKED) then
      node.StateIndex := IMG_UNCHECKED;
    node := node.GetNextSibling;
  end;
end;

procedure TMainForm.InfoClick(Sender: TObject);
begin
  Memo.Lines.LoadfromFile('readme.txt');
end;

procedure TMainForm.BtnCreateTxtFilesClick(Sender: TObject);
var
  imgDir: String;
  node: TTreeNode;
begin
  if not FileExists(EXIFTOOL_CMD) then
  begin
    MessageDlg(Format('Program "ExifTool" not found in folder "%s".', [
      ExtractFileDir(ExpandFilename(EXIFTOOL_CMD))
      ]), mtError, [mbOK], 0
    );
    exit;
  end;

  imgDir := GetImageDir;

  node := FileTreeView.Items.GetFirstNode;
  while (node <> nil) do begin
    node.DeleteChildren;
    node.ImageIndex := -1;
    node := node.GetNextSibling;
  end;

  node := FileTreeView.Items.GetFirstNode;
  while (node <> nil) do begin
    node.ImageIndex := IMG_INDEX_WORKING;
    Application.ProcessMessages;
    if not CreateRefTags(node, imgDir + node.Text) then begin
      node.ImageIndex := IMG_INDEX_IGNORE;
    end else
      node.ImageIndex := IMG_INDEX_EXIF;
    node.SelectedIndex := node.ImageIndex;
    node.StateIndex := IMG_CHECKED;
    node := node.GetNextSibling;
  end;
end;

procedure TMainForm.BtnInfoClick(Sender: TObject);
begin
  Memo.Lines.LoadFromFile('readme.txt');
end;

function TMainForm.CreateRefTags(ANode: TTreeNode; AFileName: String): Boolean;
var
  destFile: String;
  output: String;
  L: TStringList;
{$IFNDEF FPC}
  params: String;
  res: Integer;
  s: String;
const
  DEG_SYMBOL: ansistring = #176;
{$ENDIF}
begin
  Result := false;
  destFile := ChangeFileExt(AFileName, '.txt');

  {$IFDEF FPC}
  if RunCommand(EXIFTOOL_CMD, ['-a', '-H', '-s', '-G', '-c', '"%d° %d'' %.2f"\"', AFileName], output) then
    // -a ... extract all tags, also duplicates.
    // -H ... extract hex tag id if possible
    // -s ... short tag name (hopefully this is the dExif tag name)
    // -G ... print group name for each tag
    // -c ... format for GPS coordinates
  begin
    if (output = '') then
      exit;

    L := TStringList.Create;
    try
      L.Text := output;
      if ExtractReftags(ANode, L) then
        ANode.ImageIndex := IMG_INDEX_EXIF else
        ANode.ImageIndex := IMG_INDEX_IGNORE;
      ANode.SelectedIndex := ANode.ImageIndex;
      L.SaveToFile(destFile);
      Result := true;
    finally
      L.Free;
    end;
  end;
  {$ELSE}
//  params := '/c ' + EXIFTOOL_CMD + ' -a -H -s -G -c "%d' + DEG_SYMBOL + ' %d'' %.2f"\"' + AFileName + ' > ' + destFile;
  params := '/c ' + EXIFTOOL_CMD + ' -a -H -s -G -c "%d° %d'' %.2f"\"' + AFileName + ' > ' + destFile;
  res := ShellExecute(Application.Handle, 'open', PChar('cmd'), PChar(params), '', SW_HIDE);
  if (res <= 32) or not FileExists(destFile) then
    exit;
  L := TStringList.Create;
  try
    L.LoadFromFile(destFile);
    // Note: ExifTool wrote the file in UTF8 --> We must convert this for Delphi
    {$IFDEF UNICODE}
    L.Text := UTF8Decode(L.Text);
    {$ELSE}
    s := UTF8ToAnsi(L.Text);
    L.Text := s;
    {$ENDIF}
    if ExtractRefTags(ANode, L) then
      ANode.ImageIndex := IMG_INDEX_EXIF else
      ANode.ImageIndex := IMG_INDEX_IGNORE;
    ANode.SelectedIndex := ANode.ImageIndex;
    Result := true;
  finally
    L.Free;
  end;
  {$ENDIF}
end;

function TMainForm.ExtractRefTags(ANode: TTreeNode; AList: TStringList): Boolean;
const
  GROUP_START = 1;
  GROUP_LEN   = 15;
  TAGID_START = 19;
  TAGID_LEN   =  4;
  NAME_START  = 24;
  NAME_LEN    = 32;
  VALUE_START = 58;
var
  i: Integer;
  p: Integer;
  s: String;
  sGroup: String;
  sTagID: String;
  sTagName: String;
  sTagValue: String;
  tagID: Word;
  node: TTreeNode;
begin
  Result := false;
  for i:=0 to AList.Count-1 do begin
    s := AList[i];
    sGroup := trim(Copy(s, GROUP_START, GROUP_LEN));
    sTagID := trim(Copy(s, TAGID_START, TAGID_LEN));
    sTagName := trim(Copy(s, NAME_START, NAME_LEN));
    sTagValue := trim(Copy(s, VALUE_START, MaxInt));

    if sTagID = '-' then
      Continue;

    // So far, consider only EXIF-Tag
    if sGroup <> '[EXIF]' then
      Continue;

    tagID := StrToInt('$' + sTagID);
    node := ANode.Owner.AddChild(ANode, sTagName + ': ' + sTagValue);
    node.Data := Pointer(PtrInt(tagID));
  end;
  Result := ANode.Count > 0;
end;

procedure TMainForm.FileTreeViewClick(Sender: TObject);
var
  P: TPoint;
  ht: THitTests;
  node: TTreeNode;
begin
  P := FileTreeView.ScreenToClient(Mouse.CursorPos);
  ht := FileTreeView.GetHitTestInfoAt(P.X, P.Y);
  if htOnStateIcon in ht then begin
    node := FileTreeView.GetNodeAt(P.X, P.Y);
    if node.StateIndex = IMG_CHECKED then
      node.StateIndex := IMG_UNCHECKED else
      node.StateIndex := IMG_CHECKED;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  fpExifFmtSettings.ListSeparator := ' ';
  if EdImageDir.Text <> '' then
    BtnReadFilesClick(nil);
end;

function TMainForm.GetImageDir: String;
begin
  Result := IncludeTrailingPathDelimiter(ExpandFilename(EdImageDir.Text));
  Caption := Result;
end;

procedure TMainForm.Log(AMsg: String);
begin
  Memo.Lines.Add(AMsg);
  Memo.SelStart := Length(Memo.Lines.Text);
end;

{ Loads the image file represented by the specified node, reads the meta data,
  and compares with the reference file. }
procedure TMainForm.RunTest(ANode: TTreeNode);
const
 {$IFDEF FPC}
  GPS_MASK = '%0:.0f° %1:.0f'' %2:.2f"';
 {$ELSE}
  {$IFDEF UNICODE}
  GPS_MASK = '%0:.0f° %1:.0f'' %2:.2f"';
  {$ELSE}
  GPS_MASK = '%0:.0f'#176' %1:.0f'' %2:.2f"';
  {$ENDIF}
 {$ENDIF}
var
  imgInfo: TImgInfo;
  tagName: String;
  uctagname: String;
  expectedTagValue: String;
  currTagValue: String;
  s: String;
  p: Integer;
  node: TTreeNode;
  tagID: TTagID;
  lTag: TTag;
  lTagDef: TTagDef;
//  v: Variant;
  offs: Int64;
  localMismatchCount: Integer;
begin
  if ANode.StateIndex = IMG_UNCHECKED then
    exit;

  if ANode.Count = 0 then begin
    Log('Skipping image "' + ANode.Text + '":');
    Log('    No EXIF data found by ExifTool.');
    Log('');
    exit;
  end;

  localMismatchCount := 0;
  Log('Testing image "' + ANode.Text + '":');
  imgInfo := TImgInfo.Create;
  try
    imgInfo.LoadFromFile(GetImageDir + ANode.Text);
    if not imgInfo.HasExif then begin
      Log('Skipping "' + ANode.Text + '":');
      Log('    No EXIF data found by fpExif.');
      Log('');
      exit;
    end;

    node := ANode.GetFirstChild;
    while node <> nil do begin
      s := node.Text;
      p := pos(':', s);
      if p = 0 then begin
        node := node.GetNextSibling;
        Log('    Skipping tag "' + s + '": Has no value');
        continue;
      end;

      tagName := trim(Copy(s, 1, p-1));
      uctagName := Uppercase(tagName);

      lTagDef := FindExifTagDefWithoutParent(PtrInt(node.Data));
      if lTagDef = nil then begin
        Log('   Skipping tag "' + tagName + '": tag definition not found.');
        node := node.GetNextSibling;
        Continue;
      end;
      tagID := lTagDef.TagID;

      if (tagID = TAGPARENT_EXIF + $EA1C) then begin  // "Padding"
        Log(Format('    Skipping tag "%s" ($%.4x): no useful data', [tagName, TTagIDRec(tagID).Tag]));
        node := node.GetNextSibling;
        Continue;
      end;

      lTag := imgInfo.ExifData.FindTagByID(tagID);
      if lTag = nil then begin
        Log(Format('Tag "%s (ID $%.04x) not found.', [tagName, TTagIDRec(tagID).Tag]));
        node := node.GetNextSibling;
        continue;
      end;

      // Modify fpExif's tag format to match that used by ExifTool.
      case lTag.TagID of
        TAGPARENT_GPS + $0000:  // GPSVersionID
          lTag.ListSeparator := '.';
        TAGPARENT_EXIF + $9102,  // CompressedBitsPerPixel
        TAGPARENT_EXIF + $A20E,  // FocalPlaneXResolution
        TAGPARENT_EXIF + $A20F:  // FocalPlaneYResolution
          if lTag is TFloatTag then TFloatTag(lTag).FormatStr := '%2:.3f';
        TAGPARENT_EXIF + $A405:  // FocalLengthIn35mmFilm
          if lTag is TIntegerTag then TFloatTag(lTag).FormatStr := '%d mm';
        else
          if lTag is TDateTimeTag then
            TDateTimeTag(lTag).FormatStr := EXIF_DATETIME_FORMAT
          else
          if lTag is TGpsPositionTag then
            TGpsPositionTag(lTag).FormatStr := GPS_MASK
          else
          if ltag is TExposureTimeTag then
            TExposureTimeTag(ltag).FormatStr := '1/%.0f;%.0f'         // to do: use rational values
          else
          if (lTag is TFloatTag) and
           ((ucTagName = 'FNUMBER') or (pos('APERTURE', ucTagName) > 0))
          then
            TFloatTag(lTag).FormatStr := '%2:.1f';
      end;

      currTagValue := trim(lTag.AsString);
      expectedTagvalue := Copy(s, p+1, MaxInt);
      p := pos(' -->', expectedTagValue);
      if p > 0 then SetLength(expectedTagValue, p);
      expectedTagValue := trim(expectedTagValue);

      case lTag.TagID of
        TAGPARENT_INTEROP + $0001:  // InteropIndex
          if pos('INTEROP', ucTagName) <> 0 then
            expectedTagValue := FirstWord(expectedTagValue);
        TAGPARENT_EXIF + $9101:  // ComponentsConfiguration
          expectedTagValue := LettersOnly(expectedTagValue);
        TAGPARENT_EXIF + $9102,  // CompressedBitsPerPixel
        TAGPARENT_EXIF + $A20E,  // FocalPlaneXResolution
        TAGPARENT_EXIF + $A20F:  // FocalPlaneYResolution
          expectedTagValue := Format('%.3f', [StrToFloat(expectedTagValue, fpExifFmtSettings)], fpExifFmtSettings);
        else
          if (lTag is TIntegerTag) and (pos(';', currTagValue) > 0) then
//            currTagValue := ReplaceText(currTagValue, ';', ',')
            currTagValue := StringReplace(currTagValue, ';', ',', [rfReplaceAll])
          else
          if (lTag is TOffsetTag) then begin
            offs := StrToInt(currTagValue);
            currTagValue := IntToStr(offs + TOffsetTag(lTag).TiffHeaderOffset);
          end;
      end;

      if SameText(expectedTagValue, currTagValue) then
        node.ImageIndex := IMG_INDEX_SUCCESS
      else begin
        Log('    Tag mismatch "' + Format('[$%.4x] %s', [TTagIDRec(tagID).Tag, tagName]) + '"');
        Log('        expected: ' + expectedTagValue);
        Log('        found: ' + currTagValue);
        node.ImageIndex := IMG_INDEX_FAIL;
        node.Text := tagname + ': ' + expectedTagValue + ' --> found: ' + currTagValue;
        inc(FMismatchCount);
        inc(localMismatchCount);
      end;
      node.SelectedIndex := node.ImageIndex;

      node := node.GetNextSibling;
      inc(FTotalCount);
    end;
    if localMismatchCount = 0 then
      Log('    All tags matching');
  finally
    Log('');
    imgInfo.Free;
  end;

  FileTreeView.Invalidate;

end;

end.

