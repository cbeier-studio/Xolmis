unit mdvMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, SynEdit, SynHighlighterXML, Forms, Controls,
  Graphics, Dialogs, ShellCtrls, ExtCtrls, ComCtrls, StdCtrls, fpeGlobal,
  fpeMetadata; 

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnChangeDate: TButton;
    btnApplyChangesXMP: TButton;
    btnSaveXMP: TButton;
    CbDecodeMakerNotes: TCheckBox;
    CbShowTagIDs: TCheckBox;
    CbShowParentTagID: TCheckBox;
    cbProcessXMP: TCheckBox;
    EdChangeDate: TEdit;
    FilenameInfo: TLabel;
    Image: TImage;
    Label1: TLabel;
    LblChangeDate: TLabel;
    Panel5: TPanel;
    XMPListView: TListView;
    Messages: TMemo;
    PageControl: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    DateTimePanel: TPanel;
    Panel4: TPanel;
    PreviewImage: TImage;
    ImageList: TImageList;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    PgMetadata: TTabSheet;
    PgImage: TTabSheet;
    PgXMP: TTabSheet;
    XMPSynEdit: TSynEdit;
    SynXMLSyn: TSynXMLSyn;
    TagListView: TListView;
    ShellPanel: TPanel;
    ShellListView: TShellListView;
    ShellTreeView: TShellTreeView;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure btnApplyChangesXMPClick(Sender: TObject);
    procedure BtnChangeDateClick(Sender: TObject);
    procedure btnSaveXMPClick(Sender: TObject);
    procedure CbShowTagIDsChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ShellListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeViewSelectionChanged(Sender: TObject);
    procedure TagListViewCompare(Sender: TObject; Item1, Item2: TListItem;
      {%H-}Data: Integer; var Compare: Integer);
    procedure TagListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    FImgInfo: TImgInfo;
    FImageLoaded: Boolean;
    FImageOrientation: TExifOrientation;
    FFileName: String;
    procedure LoadFile(const AFileName: String);
    procedure LoadFromIni;
    procedure LoadXMPTags;
    procedure SaveToIni;
    procedure UpdateCaption;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  LCLType, LCLVersion, IniFiles, Math, DateUtils, IntfGraphics,
  fpeTags, fpeExifData, fpeIptcData;

const
  TAG_ID_CAPTION = 'Tag ID';

function CalcIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;

procedure RotateBitmap(const ABitmap: TBitmap; AOrientation: TExifOrientation);
Var
  bmp: TBitmap;
  srcImg, dstImg: TLazIntfImage;
  imgHandle, imgMaskHandle: HBitmap;
  i, j: integer;
  w1, h1: Integer;  // Input bitmap width and height diminished by 1
Begin
  Assert(ABitmap <> nil, 'RotateBitmap: Input bitmap is expected not to be nil.');

  if (AOrientation = eoUnknown) or (AOrientation = eoNormal) then
    exit;

  w1 := ABitmap.Width - 1;
  h1 := ABitmap.Height - 1;
  srcImg := TLazIntfImage.Create(0, 0);
  try
    srcImg.LoadFromBitmap(ABitmap.Handle, ABitmap.MaskHandle);
    bmp := TBitmap.Create;
    try
      dstImg := TLazIntfImage.Create(0, 0);
      try
        if AOrientation in [eoRotate90, eoRotate270, eoMirrorHorRot90, eoMirrorHorRot270] then
        begin
          bmp.SetSize(ABitmap.Height, ABitmap.Width);
          dstImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
          case AOrientation of
            eoRotate90:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[h1-j, i] := srcImg.Colors[i, j];
            eoRotate270:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[j, w1-i] := srcImg.Colors[i, j];
            eoMirrorHorRot90:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[h1-j, w1-i] := srcImg.Colors[i, j];
            eoMirrorHorRot270:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[j, i] := srcImg.Colors[i, j];
          end;
        end else
        if AOrientation in [eoRotate180, eoMirrorHor, eoMirrorVert] then
        begin
          bmp.SetSize(ABitmap.Width, ABitmap.Height);
          dstImg.LoadFromBitmap(bmp.Handle, bmp.MaskHandle);
          case AOrientation of
            eoRotate180:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[w1-i, h1-j] := srcImg.Colors[i, j];
            eoMirrorHor:
              for j:=0 to h1 do
                for i:=0 to w1 do
                  dstImg.Colors[w1-i, j] := srcImg.Colors[i, j];
            eoMirrorVert:
              for i:=0 to w1 do
                for j:=0 to h1 do
                  dstImg.Colors[i, h1-j] := srcImg.Colors[i, j];
          end;
        end;
        dstImg.CreateBitmaps(imgHandle, imgMaskHandle, false);
        bmp.Handle := ImgHandle;
        bmp.MaskHandle := ImgMaskHandle;
      finally
        dstImg.Free;
      end;
      ABitmap.Assign(bmp);
    finally
      bmp.Free;
    end;
  finally
    srcImg.Free;
  end;
end;


{ TMainForm }

procedure TMainForm.BeforeRun;
begin
  LoadFromIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  {$IF LCL_FullVersion < 2010000}
  ShellListView.SmallImages := ImageList;
  ShellTreeView.Images := ImageList;
  {$ENDIF}
  
//  XMPListView.ControlStyle:= XMPListView.Controlstyle + [csOpaque];
end;

procedure TMainForm.BtnChangeDateClick(Sender: TObject);
var
  lTag: TTag;
  dt: TDateTime;
  fn: String;
begin
  if (FImgInfo = nil) or (FImgInfo.ExifData = nil) then
    exit;

  if not TryStrToDateTime(EdChangeDate.Text, dt) then begin
    MessageDlg('No valid date/time. Use your locale settings.', mtError, [mbOK], 0);
    exit;
  end;

  lTag := FImgInfo.ExifData.TagByName['DateTimeOriginal'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  lTag := FImgInfo.ExifData.TagByName['DateTimeDigitized'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  lTag := FImgInfo.ExifData.TagByName['DateTime'];
  if lTag <> nil then
    TDateTimeTag(lTag).AsDateTime := dt;

  fn := FImgInfo.FileName;
  fn := ChangeFileExt(fn, '') + '_modified' + ExtractFileExt(fn);
  FImgInfo.SaveToFile(fn);
end;

procedure TMainForm.btnApplyChangesXMPClick(Sender: TObject);
var
  ms: TMemoryStream;
begin
  if Assigned(FImgInfo) and FImgInfo.HasXMP then
  begin
    ms := TMemoryStream.Create;
    try
      XMPSynEdit.Lines.SaveToStream(ms);
      ms.Position := 0;
      FImgInfo.XMPData.LoadFromStream(ms);
      LoadXMPTags;
    finally
      ms.Free;
    end;
  end;
end;

procedure TMainForm.btnSaveXMPClick(Sender: TObject);
var
  fn: String;
begin
  if Assigned(FImgInfo) then
  begin
    fn := FImgInfo.FileName;
    fn := ChangeFileExt(fn, '') + '_modified' + ExtractFileExt(fn);
    FImgInfo.SaveToFile(fn);
    ShowMessage('Modified image saved as ' + fn);
  end;
end;

procedure TMainForm.CbShowTagIDsChange(Sender: TObject);
var
  c: TListColumn;
  i: Integer;
begin
  TagListView.BeginUpdate;
  try
    c := nil;
    for i:=0 to TagListView.Columns.Count-1 do
      if TagListView.Columns[i].Caption = TAG_ID_CAPTION then begin
        c := TagListView.Columns[i];
        break;
      end;
    if c <> nil then
      c.Visible := CbShowTagIDs.Checked;;
  finally
    TagListView.EndUpdate;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  try
    SaveToIni;
  except
  end;
  FImgInfo.Free;
end;

procedure TMainForm.LoadFile(const AFileName: String);
var
  lTag: TTag;
  item: TListItem;
  i: Integer;
  ms: TMemoryStream;
  suffix: String;
  crs: TCursor;
begin
  FImageLoaded := false;
  FFileName := AFileName;
  Image.Picture.Clear;

  TagListView.Items.BeginUpdate;
  try
    TagListView.Clear;
    FImgInfo.Free;
    FImgInfo := TImgInfo.Create;
    try
      try
        if CbDecodeMakerNotes.Checked then
          FImgInfo.MetadataKinds := FImgInfo.MetadataKinds + [mdkExif] - [mdkExifNoMakerNotes]
        else
          FImgInfo.MetadataKinds := FImgInfo.MetadataKinds - [mdkExif] + [mdkExifNoMakerNotes];
        if CbProcessXMP.Checked then
          FImgInfo.MetadataKinds := FImgInfo.MetaDataKinds + [mdkXMP]
        else
          FImgInfo.MetadataKinds := FImgInfo.MetaDataKinds - [mdkXMP];
        FImgInfo.LoadFromFile(AFileName);
        Messages.Hide;
      except
        on E:EFpExif do begin
          Messages.Lines.Text := E.Message;
          Messages.Show;
        end;
      end;
      if FImgInfo.HasExif then begin
        FImageOrientation := FImgInfo.ExifData.ImgOrientation;
        FImgInfo.ExifData.ExportOptions := FImgInfo.ExifData.ExportOptions + [eoTruncateBinary];
        for i := 0 to FImgInfo.ExifData.TagCount-1 do begin
          lTag := FImgInfo.ExifData.TagByIndex[i];

          if lTag is TMakerNoteStringTag then
            suffix := ':' + IntToStr(TMakerNoteStringTag(lTag).Index)
          else if lTag is TMakerNoteIntegerTag then
            suffix := ':' + IntToStr(TMakerNoteIntegerTag(lTag).Index)
          else if lTag is TMakerNoteFloatTag then
            suffix := ':' + IntToStr(TMakerNoteFloatTag(lTag).Index)
          else
            suffix := '';

          if lTag is TVersionTag then
            TVersionTag(lTag).Separator := '.';
          item := TagListView.Items.Add;
          item.Data := lTag;
          item.Caption := 'EXIF.' + NiceGroupNames[lTag.Group];
          if CbShowParentTagID.Checked then
            item.SubItems.Add(Format('$%.04x:$%.04x%s', [
              lTag.TagIDRec.Parent, lTag.TagIDRec.Tag, suffix]))
          else
            Item.SubItems.Add(Format('$%.04x', [lTag.TagIDRec.Tag]));
          item.SubItems.Add(lTag.Description);
          item.SubItems.Add(lTag.AsString);
        end;

        lTag := FImgInfo.ExifData.TagByName['DateTimeOriginal'];
        if lTag <> nil then
          EdChangeDate.Text := DateTimeToStr(TDateTimeTag(lTag).AsDateTime)
        else
          EdChangeDate.Text := '';
        DateTimePanel.Show;
      end else
        DateTimePanel.Hide;
      
      if FImgInfo.HasXMP then begin
        ms := TMemoryStream.Create;
        try
          FImgInfo.XMPData.SaveToStream(ms);
          ms.Position := 0;
          XMPSynEdit.Lines.LoadFromStream(ms);
          LoadXMPTags;
        finally
          ms.Free;
        end;
      end else
      begin
        XMPListView.Clear;
        XMPSynEdit.Clear;
      end;

      if FImgInfo.HasIptc then begin
        for i := 0 to FImgInfo.IptcData.TagCount-1 do begin
          lTag := FImgInfo.IptcData.TagByIndex[i];
          item := TagListView.Items.Add;
          item.Data := lTag;
          item.Caption := 'IPTC';
          item.SubItems.Add(lTag.Description);
          item.SubItems.Add(lTag.AsString);
        end;
      end;

      if FImgInfo.HasThumbnail then begin
        ms := TMemoryStream.Create;
        try
          FImgInfo.SaveThumbnailToStream(ms);
          ms.Position := 0;
          PreviewImage.Picture.LoadFromStream(ms);
          RotateBitmap(PreviewImage.Picture.Bitmap, FImageOrientation);
        finally
          ms.Free;
        end;
      end else
        PreviewImage.Picture.Clear;

      if FImgInfo.HasWarnings then begin
        Messages.Lines.Text := FImgInfo.Warnings;
        Messages.Show;
      end;

      if PageControl.ActivePage = PgImage then begin
        crs := Screen.Cursor;
        try
          Screen.Cursor := crHourglass;
          Image.Picture.LoadFromFile(AFileName);
          if Assigned(FImgInfo.ExifData) then
            RotateBitmap(Image.Picture.Bitmap, FImageOrientation);
          FImageLoaded := true;
        finally
          Screen.Cursor := crs;
        end;
      end;
    except
      on E:Exception do begin
        FreeAndNil(FImgInfo);
        Messages.Lines.Text := E.Message;
        Messages.Show;
      end;
    end;
    UpdateCaption;
  finally
    TagListView.Items.EndUpdate;
    TagListView.Sort;
  end;
end;

procedure TMainForm.LoadFromIni;
var
  ini: TCustomIniFile;
  i, W, H, L, T: Integer;
  rct: TRect;
  s: String;
  b: Boolean;
begin
  ini := TIniFile.Create(CalcIniName);
  try
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    rct := Screen.DesktopRect;
    if W > rct.Right - rct.Left then W := rct.Right - rct.Left;
    if H > rct.Bottom - rct.Top then H := rct.Bottom - rct.Top;
    if L < rct.Left then L := rct.Left;
    if T < rct.Top then T := rct.Top;
    if L+W > rct.Right then L := rct.Right - W;
    if T+H > rct.Bottom then T := rct.Bottom - H;
    SetBounds(L, T, W, H);

    s := ini.ReadString('MainForm', 'Path', '');
    if s <> '' then ShellTreeView.Path := s;

    w := ini.ReadInteger('MainForm', 'LeftPanelWidth', 0);
    if w <> 0 then ShellPanel.Width := w;

    h := ini.ReadInteger('MainForm', 'TreeHeight', 0);
    if h <> 0 then ShellTreeView.Height := h;

    PageControl.ActivePageIndex := ini.ReadInteger('MainForm', 'PageControl', 0);

    for i:=0 to TagListView.Columns.Count-1 do begin
      w := ini.ReadInteger('TagList', 'ColWidth'+IntToStr(i), 0);
      if w <> 0 then
        TagListView.Columns[i].Width := w;
    end;

    b := ini.ReadBool('TagList', 'ShowTagIDs', true);
    for i:=0 to TagListView.Columns.Count-1 do
      if TagListView.Columns[i].Caption = TAG_ID_CAPTION then
        TagListView.Columns[2].Visible := b;
    CbShowTagIDs.Checked := b;

    b := ini.ReadBool('TagList', 'ShowParentTagID', false);
    CbShowParentTagID.Checked := b;

  finally
    ini.Free;
  end;
end;

procedure TMainForm.LoadXMPTags;
var
  i: Integer;
  item: TListItem;
begin
  XMPListView.Items.BeginUpdate;
  try
    XMPListView.Items.Clear;
    for i := 0 to FImgInfo.XMPData.TagCount-1 do begin
      item := XMPListView.Items.Add;
      item.Caption := FImgInfo.XMPData.TagName[i];
      item.SubItems.Add(FImgInfo.XMPData.TagByIndex[i]);
    end;
  finally
    XMPListView.Items.EndUpdate;
  end;
end;

procedure TMainForm.PageControlChange(Sender: TObject);
var
  crs: TCursor;
begin
  if FImgInfo = nil then
    exit;

  if not FImageLoaded then begin
    crs := Screen.Cursor;
    try
      Screen.Cursor := crHourglass;
      Image.Picture.LoadFromFile(FFileName);
      if Assigned(FImgInfo) and Assigned(FImgInfo.ExifData) then
        RotateBitmap(Image.Picture.Bitmap, FImgInfo.ExifData.ImgOrientation);
      FImageLoaded := true;
    finally
      Screen.Cursor := crs;
    end;
  end;
end;

procedure TMainForm.SaveToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TIniFile.Create(CalcIniName);
  try
    if WindowState = wsNormal then begin
      ini.WriteInteger('MainForm', 'Left', Left);
      ini.WriteInteger('MainForm', 'Top', Top);
      ini.WriteInteger('MainForm', 'Width', Width);
      ini.WriteInteger('MainForm', 'Height', Height);
    end;
    ini.WriteString('MainForm', 'Path', ShellTreeView.Path);
    ini.WriteInteger('MainForm', 'LeftPanelWidth', ShellPanel.Width);
    ini.WriteInteger('MainForm', 'TreeHeight', ShellTreeView.Height);
    ini.WriteInteger('MainForm', 'PageControl', PageControl.ActivePageIndex);

    ini.WriteBool('TagList', 'ShowTagIDs', CbShowTagIDs.Checked);
    ini.WriteBool('TagList', 'ShowParentTagID', CbShowParentTagID.Checked);
    for i:=0 to TagListView.Columns.Count-1 do
      ini.WriteInteger('TagList', 'ColWidth'+IntToStr(i), TagListView.Columns[i].Width);
  finally
    ini.Free;
  end;
end;

procedure TMainForm.ShellListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  dir, fn: String;
begin
  if Selected then
  begin
    dir := ShellTreeView.GetPathFromNode(ShellTreeView.Selected);
    fn := Item.Caption;
    LoadFile(dir + fn);
  end;
end;

procedure TMainForm.ShellTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
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

procedure TMainForm.ShellTreeViewSelectionChanged(Sender: TObject);
begin
  TagListView.Items.Clear;
  PreviewImage.Picture.Assign(nil);
  ShellTreeViewGetImageIndex(nil, ShellTreeView.Selected);
  FreeAndNil(FImgInfo);
  UpdateCaption;
end;

procedure TMainForm.TagListViewCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  tag1, tag2: TTag;
begin
  tag1 := TTag(Item1.Data);
  tag2 := TTag(Item2.Data);
  Compare := CompareValue(ord(tag1.Group), ord(tag2.Group));
  if Compare = 0 then
    Compare := CompareText(Item1.SubItems[0], Item2.SubItems[0]);
end;

procedure TMainForm.TagListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
const
  { TTagType:
  ttUInt8 = 1, ttString, ttUInt16, ttUInt32, ttURational,
  ttSInt8, ttBinary, ttSInt16, ttSInt32, ttSRational,
  ttSingle, ttDouble,
  ttIFD   // rarely used, in Olympus maker notes
  }
  TAGTYPE_NAMES: array[TTagType] of string = (
    'BYTE',  'ASCII', 'UINT16', 'UINT32', 'URATIONAL',
    'SBYTE', 'BINARY', 'SINT16', 'SINT32', 'SRATIONAL',
    'SINGLE', 'DOUBLE',
    'IFD'
  );
var
  lTag: TTag;
  tagID: TTagIDRec;
begin
  if Selected then begin
    lTag := TTag(Item.Data);
    if lTag <> nil then begin
      tagID := TTagIDRec(lTag.TagID);
      Statusbar1.Panels[0].Text := Format('ID %d [$%.4x]', [tagID.Tag, tagID.Tag]);
      Statusbar1.Panels[1].Text := Format('Parent %d [$%.4x]', [tagID.Parent, tagID.Parent]);
      Statusbar1.Panels[2].Text := 'Name: ' + lTag.Name;
      Statusbar1.Panels[3].Text := 'Type: ' + TAGTYPE_NAMES[lTag.TagType];
      Statusbar1.Panels[4].Text := 'Elements: ' + IntToStr(lTag.Count);
      exit;
    end;
  end;
  Statusbar1.Panels[0].Text := '';
  Statusbar1.Panels[1].Text := '';
  Statusbar1.Panels[2].Text := '';
  Statusbar1.Panels[3].Text := '';
  Statusbar1.Panels[4].Text := '';
end;

procedure TMainForm.UpdateCaption;
begin
  if FImgInfo <> nil then
    FileNameInfo.Caption := Format(
      'File: %s' + LineEnding +
      'Size: %d kB' + LineEnding +
      'Date: %s', [
      FImgInfo.Filename, FImgInfo.FileSize div 1024, DateTimeToStr(FImgInfo.FileDate)])
  else
  if FFileName <> '' then
    FilenameInfo.Caption := Format('File: %s', [FFileName])
  else
    FilenameInfo.Caption := '< no file >';
end;

end.

