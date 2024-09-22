unit rwMain;

{$I ..\..\..\fpExif.inc}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

interface

uses
 {$IFDEF FPC}
  LazUtf8,
 {$ELSE}
  Windows, Messages, ImgList, jpeg,
  {$IFDEF UNICODE}
  System.ImageList,
  {$ENDIF}
 {$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons, ComCtrls, ExtCtrls, Variants,
  fpeGlobal, fpeTags, fpeMetadata;

type

  { TMainForm }

  TMainForm = class(TForm)
    BtnTest1: TSpeedButton;
    BtnTest2: TSpeedButton;
    CbTestfile: TComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    ListView: TListView;
    OpenDialog: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    ExifListView: TListView;
    ExifTabControl: TTabControl;
    BtnBrowse: TSpeedButton;
    Splitter1: TSplitter;
    procedure CbTestfileEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnTest1Click(Sender: TObject);
    procedure ExifTabControlChange(Sender: TObject);
    procedure BtnBrowseClick(Sender: TObject);
  private
    ImgInfo: TImgInfo;
    OutFile: String;
    procedure ExecTest(const AParamsFile: String);
    procedure ExifToListview(AImgInfo: TImgInfo; AListView: TListView);
    function ReadTagValue(ATagName: String; out ATag: TTag): String; overload;
    function ReadTagValue(ATagName: String): String; overload;
    function Success(ATag: TTag; ACurrValue, AExpectedValue: String): Boolean;
    procedure WriteTagValue(ATagName, ATagValue: String);

    procedure AddToHistory(AFilename: String);
    procedure ReadFromIni;
    procedure WriteToIni;

  public
    procedure BeforeRun;

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
  StrUtils, Math, IniFiles,
  fpeUtils, fpeExifData;

const
  IMGINDEX_SUCCESS = 0;
  IMGINDEX_FAIL = 1;

  TESTCASES_DIR = 'common\';

type
  TStringArray = array of string;

function Split(s: String; AMinCount: Integer; Separator: Char = #9): TStringArray;
const
  BLOCK_SIZE = 20;
var
  i, j, n, L: Integer;
begin
  if s = '' then begin
    SetLength(Result, 0);
    exit;
  end;

  s := s + Separator;
  L := Length(s);
  SetLength(Result, BLOCK_SIZE);
  i := 1;
  j := 1;
  n := 0;
  while (i <= L) do begin
    if (s[i] = Separator) or (i = L)  then begin
      Result[n] := Copy(s, j, i-j);
      inc(n);
      if n mod BLOCK_SIZE = 0 then
        SetLength(Result, Length(Result) + BLOCK_SIZE);
      j := i+1;
    end;
    inc(i);
  end;
  while n < AMinCount do begin
    Result[n] := '';
    inc(n);
    if n mod BLOCK_SIZE = 0 then
      SetLength(Result, Length(Result) + BLOCK_SIZE);
  end;
  SetLength(Result, n);
end;

{ The date/time string is expected in the ISO format "yyyy-mm-dd hh:nn:ss" }
function ExtractDateTime(AValue: String): TDateTime;
var
  p: Integer;
  yr, mn, dy, h, m, s: Integer;
begin
  Result := 0;
  p := pos('-', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  yr := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos('-', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  mn := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(' ', AValue);
  if p = 0 then begin
    dy := StrToInt(AValue);
    Result := EncodeDate(yr, mn, dy);
    exit;
  end;
  dy := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(':', AValue);
  if p = 0 then
    raise Exception.Create('ISO date/time format expected: "yyyy-mm-dd hh:nn:ss"');
  h := StrToInt(copy(AValue, 1, p-1));
  Delete(AValue, 1, p);
  p := pos(':', AValue);
  if p = 0 then begin
    m := StrToInt(AValue);
    s := 0;
  end else begin
    m := StrToInt(copy(AValue, 1, p-1));
    s := StrToInt(copy(AValue, p+1, MaxInt));
  end;
  Result := EncodeDate(yr, mn, dy) + EncodeTime(h, m, s, 0);
end;

function DecimalSep: Char;
begin
 {$IFDEF FPC}
   Result := FormatSettings.DecimalSeparator;
 {$ELSE}
  {$IFDEF VER150}  // Delphi 7
   Result := DecimalSeparator;
  {$ELSE}
   Result := FormatSettings.DecimalSeparator;
  {$ENDIF}
 {$ENDIF}
end;

function CleanFloatStr(AText: String): String;
var
  i: Integer;
begin
  Result := '';
  i := 1;
  while i <= Length(AText) do begin
    // case aperture value, e.g. "F/2.8"
    if (i < Length(AText)) and (AText[i] in ['f', 'F']) and (AText[i+1] = '/') then
      inc(i)
    else
    if AText[i] in ['0'..'9', '.', '/'] then
      Result := Result + AText[i]
    else if AText[i] = ',' then
      Result := Result + '.';
    inc(i);
  end;
end;


{ TMainForm }

procedure TMainForm.AddToHistory(AFileName: String);
var
  i: Integer;
begin
  if (AFileName = '') or (not FileExists(AFileName)) then
    exit;

  i := CbTestFile.Items.Indexof(AFileName);
  if i > -1 then
    CbTestfile.Items.Delete(i);
  CbTestFile.Items.Insert(0, AFileName);
  CbTestFile.ItemIndex := 0;
end;

procedure TMainForm.BeforeRun;
begin
  ReadFromIni;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  ImgInfo := TImgInfo.Create;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
  ImgInfo.Free;
end;

procedure TMainForm.BtnTest1Click(Sender: TObject);
begin
  AddToHistory(CbTestFile.Text);
  if Sender = BtnTest1 then
    ExecTest(TESTCASES_DIR + 'testcases1.txt')
  else if Sender = BtnTest2 then
    ExecTest(TESTCASES_DIR + 'testcases2.txt')
  else
    raise Exception.Create('BtnTextClick: Unexpected Sender');
end;

procedure TMainForm.CbTestfileEditingDone(Sender: TObject);
begin
  AddToHistory(CbTestFile.Text);
end;

procedure TMainForm.ExecTest(const AParamsFile: String);
var
  testCases: TStringList;
  i, j, n, p: Integer;
  s: String;
  testdata: TStringArray;
  listitem: TListItem;
  lTag: TTag;
  tagName: String;
  currTagValue: String;
  newTagValue: String;
  newTagValues: TStringArray;
  jpeg: TJpegImage;
  {$IFDEF FPC}
  stream: TMemorystream;
  {$ELSE}
  stream: TMemoryStream;
  a: ansistring;
  {$ENDIF}
begin
  Listview.Items.Clear;

  if not FileExists(AParamsFile) then begin
    showMessage('Parameter file "' + AParamsFile + '" not found.');
    exit;
  end;
  if not FileExists(CbTestfile.Text) then begin
    ShowMessage('Test picture file "' + CbTestfile.Text + '" not found.');
    exit;
  end;

  // Read test parameters
  testCases := TStringList.Create;
  try

  {$IFDEF FPC}
    // The testcases text files are encoded in ANSI for Delphi7 compatibility
    // In Lazarus we must convert to UTF8 }
    testCases.LoadFromFile(AParamsFile);
    s := testCases.Text;
   {$IFDEF FPC3+}
    testCases.Text := WinCPToUTF8(s);
   {$ELSE}
    testCases.Text := AnsiToUTF8(s);
   {$ENDIF}
  {$ELSE}
    stream := TMemoryStream.Create;
    try
      stream.LoadFromFile(AParamsFile);
      SetLength(a, stream.Size);
      stream.Read(a[1], Length(a));
      testcases.Text := a;
    finally
      stream.Free;
    end;
  {$ENDIF}

    // Read EXIF tags from image file
    ImgInfo.LoadFromFile(CbTestfile.Text);
    if not ImgInfo.HasExif then
      ImgInfo.CreateExifData(false);

    OutFile := 'test-image.jpg';   // File name of the modified test image

    ListView.Items.BeginUpdate;
    try
      j := 0;
      n := testCases.Count;
      for i:=0 to n-1 do begin
        if (testCases[i] = ':quit') then
          break;

        if (testCases[i] = '') or (testCases[i][1] = ';') then
          Continue;

        // Extract test parameters
        testdata := Split(testCases[i], 2);
        tagName := testdata[0];
        newTagValue := testdata[1];
        newTagValues := Split(newTagValue, 2, '|');
        if Length(newTagValues) =0 then begin
          SetLength(newTagValues, 1);
          newTagValues[0] := '';
        end;

        // Add test to listview
        listitem := ListView.Items.Add;
        listItem.Caption := tagname;

        // Read current tag value
        currTagValue := ReadTagValue(tagName, lTag);
        listItem.SubItems.Add(currTagValue);
        listItem.Data := lTag;

        // Write new tag value into ExifObj
        WriteTagValue(tagName, newTagValues[0]);
        listItem.SubItems.Add(newTagValue);
      end;
    finally
      ListView.Items.EndUpdate;
    end;

    // Write new tags to file
    ImgInfo.SaveToFile(OutFile);

    // read back
    ImgInfo.LoadFromFile(OutFile);
    if not ImgInfo.HasExif then
      raise Exception.Create('No EXIF structure detected in "' + Outfile + '"');

    j := 0;
    for i:=0 to testCases.Count-1 do begin
      if (testcases[i] = ':quit') then
        break;
      if (testcases[i] = '') or (testcases[i][1] = ';') then
        Continue;
      testdata := Split(testCases[i], 2);
      tagname := testdata[0];
      newTagValue := testdata[1];
      currTagValue := ReadTagValue(tagname, lTag);
      listItem := ListView.Items[j];
      listItem.SubItems.Add(currTagValue);
      if Success(lTag, currTagValue, newTagValue) then
        listItem.ImageIndex := IMGINDEX_SUCCESS else
        listItem.ImageIndex := IMGINDEX_FAIL;
      inc(j);
    end;

    jpeg := TJpegImage.Create;
    try
      try
        jpeg.LoadFromFile(OutFile);
        listitem := ListView.Items.Add;
        listItem.Caption := 'Successfully loaded';
        listItem.ImageIndex := IMGINDEX_SUCCESS;
      except
        listitem := ListView.Items.Add;
        listItem.Caption := 'Loading failed.';
        listItem.ImageIndex := IMGINDEX_FAIL;
      end;
    finally
      jpeg.Free;
    end;

  finally
    testCases.Free;
  end;

  ExifTabControlChange(nil);
end;

procedure TMainForm.BtnBrowseClick(Sender: TObject);
var
  olddir: String;
begin
  olddir := GetCurrentDir;
  OpenDialog.FileName := '';
  if OpenDialog.Execute then
    AddToHistory(OpenDialog.Filename);
  SetCurrentDir(oldDir);
end;

function TMainForm.Success(ATag: TTag; ACurrValue, AExpectedValue: String): Boolean;
const
  relEPS = 1E-3;
var
  p: Integer;
  snum, sdenom: String;
  valexp, valcurr: Double;
  decode: Boolean;
  currVal, expVal: String;
begin
  Result := ACurrValue = AExpectedValue;
  if Result then
    exit;

  if (ACurrValue = '') or (AExpectedValue = '') then begin
    Result := false;
    exit;
  end;
                                       (*
  { Check for alternative expected value }
  p := pos('|', AExpectedValue);
  if p > 0 then begin
    expected2 := Copy(AExpectedValue, p+1, MaxInt);;
    expected1 := Copy(AExpectedValue, 1, p-1);
    Result := (ACurrValue = expected1);
    if Result then
      exit;
    Result := (ACurrValue = expected2);
    if Result then
      exit;
  end;    *)

  { Check for float values, e.g. 12.0 vs 12 }
  if (ATag is TFloatTag) then begin
    currVal := CleanFloatStr(ACurrValue);
    expVal := CleanFloatStr(AExpectedValue);

    Result := currVal = expval;
    if Result then
      exit;

    { Check for fractional result, e.g. exposure time }
    p := pos('/', currVal);
    if p > 0 then begin
      snum := Copy(currVal, 1, p-1);
      sdenom := Copy(currVal, p+1, MaxInt);
      valcurr := StrToInt(snum) / StrToInt(sdenom);
    end else
      valcurr := StrToFloat(currVal, fpExifFmtSettings);

    p := pos('/', expVal);
    if p > 0 then begin
      snum := Copy(expval, 1, p-1);
      sdenom := Copy(currval, p+1, MaxInt);
      valexp := StrToInt(snum) / StrToInt(sdenom);
    end else
      valexp := StrToFloat(expval, fpExifFmtSettings);

    Result := SameValue(valcurr, valexp, relEPS * valexp);
    if Result then
      exit;
  end;

  if (ATag is TIntegerTag) then begin
    decode := ATag.DecodeValue;
    ATag.DecodeValue := not decode;
    currVal := ATag.AsString;
    ATag.DecodeValue := decode;
    Result := (currVal = AExpectedValue);
    if Result then
      exit;
  end;
end;

procedure TMainForm.ExifToListview(AImgInfo: TImgInfo; AListView: TListView);
var
  i: Integer;
  lTag: TTag;
  item: TListItem;
begin
  AListview.Items.BeginUpdate;
  try
    AListview.Items.Clear;
    if not AImgInfo.HasExif then
      exit;
    for i:=0 to AImgInfo.ExifData.TagCount-1 do begin
      lTag := AImgInfo.ExifData.TagByIndex[i];
      if lTag = nil then
        Continue;
      item := AListView.Items.Add;
      with item do begin
        Caption := lTag.Description;
        SubItems.Add(lTag.AsString);
      end;
    end;
    AListView.AlphaSort;
  finally
    AListview.Items.EndUpdate;
  end;
end;

function TMainForm.ReadTagValue(ATagName: String): String;
var
  lTag: TTag;
begin
  Result := ReadTagValue(ATagName, lTag);
end;

function TMainForm.ReadTagValue(ATagName: String; out ATag: TTag): String;
begin
  if ATagName = 'Comment' then begin
    Result := ImgInfo.Comment;
    ATag := nil;
  end else
  begin
    ATag := ImgInfo.ExifData.FindTagByName(ATagName);
    if ATag = nil then
      Result := ''
    else
      Result := ATag.AsString;
  end;
end;

procedure TMainForm.ExifTabControlChange(Sender: TObject);
var
  data: TImgInfo;
begin
  data := TImgInfo.Create;
  try
    case ExifTabControl.TabIndex of
      0: data.LoadFromFile(CbTestfile.Text);
      1: data.LoadFromFile(OutFile);
    end;
    ExifToListView(data, ExifListView);
  finally
    data.Free;
  end;
end;

procedure TMainForm.WriteTagValue(ATagName, ATagValue: String);
var
  lTag: TTag;
begin
  if ATagName = 'Comment' then
    ImgInfo.Comment := ATagValue
  else begin
    lTag := ImgInfo.ExifData.TagByName[ATagName];
    if lTag = nil then
      lTag := ImgInfo.ExifData.AddTagByName(ATagName);
    lTag.AsString := ATagvalue;
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

      ini.ReadSection('History', list);
      for i:=list.Count-1 downto 0 do  // count downward because AddToHistory adds to the beginning of the list
        AddToHistory(ini.ReadString('History', list[i], ''));
      CbTestFile.ItemIndex := 0;
    finally
      list.Free;
    end;
  finally
    ini.Free;
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

    for i:=0 to CbTestFile.Items.Count-1 do
      if (CbTestFile.Items[i] <> '') and FileExists(CbTestFile.Items[i]) then
        ini.WriteString('History', 'Item'+IntToStr(i+1), CbTestFile.Items[i]);
    ini.UpdateFile;
  finally
    ini.Free;
  end;
end;

end.

