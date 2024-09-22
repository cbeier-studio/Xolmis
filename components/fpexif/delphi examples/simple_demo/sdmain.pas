unit sdMain;

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, fpeMetadata, fpeMakerNote;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnLoad: TButton;
    BtnBrowse: TButton;
    BtnSave: TButton;
    CbDecodeValue: TCheckBox;
    CbFilename: TComboBox;
    CbVerbosity: TComboBox;
    CbTruncateBinaryTags: TCheckBox;
    CbBinaryAsASCII: TCheckBox;
    CbTags: TComboBox;
    EdNewTagValue: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Thumbnail: TImage;
    Memo: TMemo;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    procedure BtnLoadClick(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure CbBinaryAsASCIIClick(Sender: TObject);
    procedure CbDecodeValueClick(Sender: TObject);
    procedure CbFilenameSelect(Sender: TObject);
    procedure CbTagsSelect(Sender: TObject);
    procedure CbTruncateBinaryTagsClick(Sender: TObject);
    procedure CbVerbosityChange(Sender: TObject);
    procedure EdNewTagValueEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FImgInfo: TImgInfo;
    FModified: Boolean;
    procedure AddToHistory(AFileName: String);
    procedure DisplayMetadata;
    procedure LoadFile(const AFileName: String);
    procedure LoadThumbnail;
    procedure PopulateTagCombo;
    procedure ReadFromIni;
    procedure UpdateCaption(AInit: Boolean);
    procedure WriteToIni;

  public
    procedure BeforeRun;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  IniFiles, jpeg, fpeGlobal, fpeTags, fpeExifData;

{ TMainForm }

procedure TMainForm.AddToHistory(AFileName: String);
var
  i: Integer;
begin
  if (AFileName = '') or (not FileExists(AFileName)) then
    exit;

  i := CbFileName.Items.Indexof(AFileName);
  if i > -1 then
    CbFileName.Items.Delete(i);
  CbFileName.Items.Insert(0, AFileName);
  CbFileName.ItemIndex := 0;
end;

procedure TMainForm.BeforeRun;
begin
  ReadFromIni;
end;

procedure TMainForm.BtnBrowseClick(Sender: TObject);
var
  olddir: String;
begin
  olddir := GetCurrentDir;
  OpenDialog.FileName := '';
  if OpenDialog.Execute then begin
    AddToHistory(OpenDialog.Filename);
    SetCurrentDir(oldDir);
    LoadFile(OpenDialog.Filename);
  end;
end;

procedure TMainForm.BtnLoadClick(Sender: TObject);
begin
  LoadFile(CbFilename.Text);
end;

procedure TMainForm.BtnSaveClick(Sender: TObject);
var
  fn, ext: String;
begin
  ext := ExtractFileExt(CbFilename.Text);
  fn := ChangeFileExt(CbFileName.Text, '');
  if pos('_modified', fn) <> Length(fn) - Length('modified') then
    fn := fn + '_modified' + ext
  else
    fn := CbFilename.Text;
  FImgInfo.SaveToFile(fn);
  MessageDlg(Format('File saved as "%s"', [fn]), mtInformation, [mbOK], 0);
end;

procedure TMainForm.CbBinaryAsASCIIClick(Sender: TObject);
begin
  DisplayMetadata;
end;

procedure TMainForm.CbDecodeValueClick(Sender: TObject);
begin
  DisplayMetaData;
end;

procedure TMainForm.CbFilenameSelect(Sender: TObject);
begin
  LoadFile(CbFileName.Text);
end;

procedure TMainForm.CbTagsSelect(Sender: TObject);
var
  lTag: TTag;
  decoded: Boolean;
begin
  if FImgInfo.HasExif then begin
    lTag := FImgInfo.ExifData.TagByName[CbTags.Text];
    decoded := lTag.DecodeValue;
    lTag.DecodeValue := false;
    EdNewTagValue.Text := lTag.AsString;
    lTag.DecodeValue := decoded;
  end;
end;

procedure TMainForm.CbTruncateBinaryTagsClick(Sender: TObject);
begin
  DisplayMetadata;
end;

procedure TMainForm.CbVerbosityChange(Sender: TObject);
begin
  DisplayMetadata;
end;

procedure TMainForm.DisplayMetadata;
const
  SEPARATOR = ': ';
var
  exportOptions: TExportOptions;
begin
  Memo.Lines.Clear;

  if FImgInfo <> nil then begin
    exportOptions := [eoShowTagName];
    case CbVerbosity.ItemIndex of
      1: Include(exportOptions, eoShowDecimalTagID);
      2: Include(exportOptions, eoShowHexTagID);
    end;
    if CbDecodeValue.Checked then
      Include(exportOptions, eoDecodeValue) else
      Exclude(exportOptions, eoDecodeValue);
    if CbTruncateBinaryTags.Checked then
      Include(exportOptions, eoTruncateBinary) else
      Exclude(exportOptions, eoTruncateBinary);
    if CbBinaryAsASCII.Checked then
      Include(exportOptions, eoBinaryAsASCII) else
      Exclude(exportOptions, eoBinaryAsASCII);

    Memo.Lines.BeginUpdate;
    try
      if FImgInfo.ExifData <> nil then begin
        FImgInfo.ExifData.ExportOptions := exportOptions;
        FImgInfo.ExifData.ExportToStrings(Memo.Lines, SEPARATOR);
      end;
      if FImgInfo.IptcData <> nil then
        FImgInfo.IptcData.ExportToStrings(Memo.Lines, exportOptions, SEPARATOR);
      if FImgInfo.XmpData <> nil then
        FImgInfo.XmpData.ExportToStrings(Memo.Lines, exportOptions, SEPARATOR);
    finally
      Memo.Lines.EndUpdate;
      Memo.Invalidate;
    end;
  end;
end;

procedure TMainForm.EdNewTagValueEditingDone(Sender: TObject);
var
  lTag: TTag;
  i: Integer;
  f: Double;
  dt: TDateTime;
begin
  if FImgInfo.HasExif then begin
    lTag := FImgInfo.ExifData.TagByName[CbTags.Text];
    if lTag = nil then begin
      MessageDlg('Tag not found.', mtError, [mbOK], 0);
      exit;
    end;
    if lTag.ReadOnly then begin
      MessageDlg('This tag is readonly.', mtError, [mbOK], 0);
      exit;
    end;

    if (lTag is TDateTimeTag) then begin
      if TryStrToDateTime(EdNewTagValue.Text, dt) then begin
        FModified := FModified or (TDateTimeTag(lTag).AsDateTime <> dt);
        TDateTimeTag(lTag).AsDateTime := dt;
      end else begin
        MessageDlg('Date/time value expected for this kind of tag.', mtError, [mbOK], 0);
        exit;
      end;
    end else
    if (lTag is TShutterSpeedTag) then begin
      FModified := true;
      TShutterSpeedTag(lTag).AsString := EdNewTagValue.Text;
    end else
    if (lTag is TStringTag) then begin
      FModified := FModified or (EdNewTagValue.Text <> TStringTAg(lTag).AsString);
      TStringTag(lTag).AsString := EdNewTagValue.Text;
    end else
    if (lTag is TIntegerTag) and (lTag.Count = 1) then begin
      if TryStrToInt(EdNewTagValue.Text, i) then begin
        FModified := FModified or (TIntegerTag(lTag).AsInteger <> i);
        TIntegerTag(lTag).AsInteger := i;
      end else begin
        MessageDlg('Integer value expected for this kind of tag.', mtError, [mbOK], 0);
        exit;
      end;
    end else
    if (lTag is TFloatTag) and (lTag.Count = 1) then begin
      if TryStrToFloat(EdNewTagValue.Text, f) then begin
        FModified := FModified or (TFloatTag(lTag).AsFloat <> f);
        TFloatTag(lTag).AsFloat := f;
      end else begin
        MessageDlg('Floating point value expected for this kind of tag.', mtError, [mbOK], 0);
        exit;
      end;
    end;
    DisplayMetadata;
    UpdateCaption(false);
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  UpdateCaption(true);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
  FImgInfo.Free;
end;

procedure TMainForm.LoadFile(const AFileName: String);
var
  exportOptions: TExportOptions;
begin
  if FImgInfo = nil then
    FImgInfo := TImgInfo.Create;

  try
    FImgInfo.LoadFromFile(ExpandFileName(CbFilename.Text));
    if FImgInfo.ExifData <> nil then begin
      DisplayMetadata;
      LoadThumbnail;
      PopulateTagCombo;
      AddToHistory(AFilename);
    end else begin
      Thumbnail.Picture.Assign(nil);
      CbTags.Items.Clear;
    end;
    if FImgInfo.HasWarnings then begin
      Memo.Lines.Add('');
      Memo.Lines.Add('*** WARNINGS ****');
      Memo.Lines.Add(FImgInfo.Warnings);
    end;
    UpdateCaption(false);
    FModified := false;
    BtnSave.Enabled := FImgInfo.ImgFormat = ifJpeg;
  except
    on E:EFpExifReader do begin
      Memo.Lines.Text := E.Message;
      Thumbnail.Picture.Assign(nil);
      CbTags.Items.Clear;
      ShowMessage(E.Message);
    end;
  end;
end;

procedure TMainForm.LoadThumbnail;
var
  ms: TMemoryStream;
  jpg: TJpegImage;
begin
  if not FImgInfo.HasThumbnail then
    exit;

  //if (FImgInfo.ExifData = nil) or (not FImgInfo.Exifdata.HasThumbnail) then
  //  exit;

  ms := TMemoryStream.Create;
  try
    FImgInfo.SaveThumbnailToStream(ms);
    ms.Position := 0;
    jpg := TJpegImage.Create;
    try
      jpg.LoadFromStream(ms);
      Thumbnail.Picture.Assign(jpg);
    finally
      jpg.Free;
    end;
  finally
    ms.Free;
  end;
end;

procedure TMainForm.PopulateTagCombo;
var
  i: Integer;
  L: TStrings;
  lTag: TTag;
begin
  L := TStringList.Create;
  try
    if FImgInfo.HasExif then
      for i:=0 to FImgInfo.ExifData.TagCount-1 do begin
        lTag := FImgInfo.ExifData.TagByIndex[i];
        if not lTag.ReadOnly or lTag.IsVolatile then
          L.Add(GroupNames[lTag.Group] + '.' + lTag.Name);
      end;
    CbTags.Items.Assign(L);
    CbTags.ItemIndex := -1;
  finally
    L.Free;
  end;
end;

function CreateIni: TCustomIniFile;
begin
  Result := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  list: TStrings;
  i: Integer;
  W, H, L, T: Integer;
  R: TRect;
begin
  ini := CreateIni;
  try
    list := TStringList.Create;
    try
      if WindowState = wsNormal then begin
        W := ini.ReadInteger('MainForm', 'Width', Width);
        H := ini.ReadInteger('MainForm', 'Height', Height);
        L := ini.ReadInteger('MainForm', 'Left', Left);
        T := ini.ReadInteger('MainForm', 'Top', Top);
        R := Screen.DesktopRect;
        if W > R.Right - R.Left then W := R.Right - R.Left;
        if L+W > R.Right then L := R.Right - W;
        if L < R.Left then L := R.Left;
        if H > R.Bottom - R.Top then H := R.Bottom - R.Top;
        if T+H > R.Bottom then T := R.Bottom - H;
        if T < R.Top then T := R.Top;
        SetBounds(L, T, W, H);
      end;

      CbVerbosity.ItemIndex := ini.ReadInteger('Settings', 'Verbosity', CbVerbosity.ItemIndex);
      CbDecodeValue.Checked := ini.ReadBool('Settings', 'DecodeValue', CbDecodeValue.Checked);

      ini.ReadSection('History', list);
      for i:=list.Count-1 downto 0 do  // count downward because AddToHistory adds to the beginning of the list
        AddToHistory(ini.ReadString('History', list[i], ''));
      if CbFilename.Items.Count = 0 then
        AddToHistory('..\test-image.jpg');


      CbFilename.ItemIndex := 0;
    finally
      list.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TMainForm.UpdateCaption(AInit: Boolean);
const
  DEFAULT_CAPTION = 'Picture metadata viewer';
var
  mask: String;
begin
  if AInit then
    Caption := DEFAULT_CAPTION
  else
  begin
    if FModified then
      mask := '%s - [*] %s' else
      mask := '%s - %s';
    if FImgInfo.Filename <> '' then
      Caption := Format(mask, [DEFAULT_CAPTION, '"' + FImgInfo.FileName + '"'])
    else
      Caption := Format(mask, [DEFAULT_CAPTION, 'ERROR']);
  end;
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := CreateIni;
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);

    ini.WriteInteger('Settings', 'Verbosity', CbVerbosity.ItemIndex);
    ini.WriteBool('Settings', 'DecodeValue', CbDecodeValue.Checked);

    for i:=0 to CbFileName.Items.Count-1 do
      if (CbFilename.Items[i] <> '') and FileExists(CbFilename.Items[i]) then
        ini.WriteString('History', 'Item'+IntToStr(i+1), CbFilename.Items[i]);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;


end.
