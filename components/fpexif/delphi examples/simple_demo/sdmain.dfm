object MainForm: TMainForm
  Left = 329
  Top = 131
  Caption = 'MainForm'
  ClientHeight = 477
  ClientWidth = 788
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    788
    477)
  TextHeight = 13
  object Thumbnail: TImage
    Left = 620
    Top = 41
    Width = 160
    Height = 151
    Anchors = [akTop, akRight]
    Center = True
    Proportional = True
    Stretch = True
  end
  object Label1: TLabel
    Left = 620
    Top = 380
    Width = 50
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'New value'
    Color = clBtnFace
    ParentColor = False
  end
  object Label2: TLabel
    Left = 618
    Top = 330
    Width = 18
    Height = 13
    Anchors = [akRight, akBottom]
    Caption = 'Tag'
    Color = clBtnFace
    ParentColor = False
  end
  object BtnLoad: TButton
    Left = 728
    Top = 8
    Width = 52
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Load'
    TabOrder = 0
    OnClick = BtnLoadClick
  end
  object CbFilename: TComboBox
    Left = 8
    Top = 9
    Width = 677
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 1
    Text = '..\test-image.jpg'
    OnSelect = CbFilenameSelect
  end
  object Memo: TMemo
    Left = 8
    Top = 41
    Width = 604
    Height = 384
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object BtnBrowse: TButton
    Left = 689
    Top = 8
    Width = 35
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = BtnBrowseClick
  end
  object Panel1: TPanel
    Left = 0
    Top = 442
    Width = 788
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    DesignSize = (
      788
      35)
    object CbVerbosity: TComboBox
      Left = 8
      Top = 1
      Width = 192
      Height = 21
      Style = csDropDownList
      ItemIndex = 2
      TabOrder = 0
      Text = 'Hex tag IDs'
      Items.Strings = (
        'Tag names only'
        'Decimal tag IDs'
        'Hex tag IDs')
    end
    object CbDecodeValue: TCheckBox
      Left = 208
      Top = 3
      Width = 96
      Height = 19
      Caption = 'Decode values'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = CbDecodeValueClick
    end
    object CbTruncateBinaryTags: TCheckBox
      Left = 312
      Top = 3
      Width = 127
      Height = 19
      Caption = 'Truncate binary tags'
      Checked = True
      State = cbChecked
      TabOrder = 2
      OnClick = CbTruncateBinaryTagsClick
    end
    object CbBinaryAsASCII: TCheckBox
      Left = 447
      Top = 3
      Width = 123
      Height = 19
      Caption = 'Binary tags as ASCII'
      TabOrder = 3
      OnClick = CbBinaryAsASCIIClick
    end
    object BtnSave: TButton
      Left = 650
      Top = 0
      Width = 130
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Save as "_modified"'
      Enabled = False
      TabOrder = 4
      OnClick = BtnSaveClick
    end
  end
  object CbTags: TComboBox
    Left = 620
    Top = 349
    Width = 160
    Height = 21
    Style = csDropDownList
    Anchors = [akRight, akBottom]
    DropDownCount = 32
    TabOrder = 5
    OnSelect = CbTagsSelect
  end
  object EdNewTagValue: TEdit
    Left = 620
    Top = 399
    Width = 160
    Height = 21
    Anchors = [akRight, akBottom]
    TabOrder = 6
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.jpg'
    Filter = 
      'All supported images (*.jpg; *.jpeg; *.jfe); *.tiff; *.tif|*.jpg' +
      ';*.jpeg;*.jfe;*.tiff;*.tif|JPG files (*.jpg; *.jpeg; *.jfe)|*.jp' +
      'g;*.jpeg;*.jfe|TIFF files (*.tiff; *.tif)|*.tiff;*.tif'
    Left = 248
    Top = 168
  end
end
