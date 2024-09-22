object MainForm: TMainForm
  Left = 430
  Top = 141
  Caption = 'Metadata viewer'
  ClientHeight = 715
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Splitter2: TSplitter
    Left = 274
    Top = 0
    Width = 5
    Height = 692
  end
  object ShellPanel: TPanel
    Left = 0
    Top = 0
    Width = 274
    Height = 692
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 0
      Top = 288
      Width = 274
      Height = 5
      Cursor = crVSplit
      Align = alTop
    end
    object PreviewImage: TImage
      Left = 0
      Top = 548
      Width = 274
      Height = 144
      Hint = 'Thumbnail image embedded in the image file'
      Align = alBottom
      Center = True
      Proportional = True
      Stretch = True
    end
    object ShellTreeView: TDirectoryOutline
      Left = 0
      Top = 19
      Width = 274
      Height = 269
      Hint = 'Navigate to the folder with your images.'
      Align = alTop
      ItemHeight = 13
      Options = [ooDrawFocusRect]
      PictureLeaf.Data = {
        46030000424D460300000000000036000000280000000E0000000E0000000100
        2000000000001003000000000000000000000000000000000000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080000000000000000000000000000000000000000000000000000000
        00000000000000000000000000008000800080008000800080000000000000FF
        FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
        FF000000000080008000800080008000800000000000FFFFFF0000FFFF00FFFF
        FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000008000
        800080008000800080000000000000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
        FF00FFFFFF0000FFFF00FFFFFF0000FFFF000000000080008000800080008000
        800000000000FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFF
        FF0000FFFF00FFFFFF00000000008000800080008000800080000000000000FF
        FF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FF
        FF000000000080008000800080008000800000000000FFFFFF0000FFFF00FFFF
        FF0000FFFF00FFFFFF0000FFFF00FFFFFF0000FFFF00FFFFFF00000000008000
        8000800080008000800000000000000000000000000000000000000000000000
        0000000000000000000000000000000000008000800080008000800080008000
        80008000800000000000FFFFFF0000FFFF00FFFFFF0000FFFF00000000008000
        8000800080008000800080008000800080008000800080008000800080008080
        8000000000000000000000000000000000008080800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        8000800080008000800080008000800080008000800080008000800080008000
        80008000800080008000}
      TabOrder = 0
      OnChange = ShellTreeViewChange
      Data = {10}
    end
    object ShellListView: TFileListBox
      Left = 0
      Top = 293
      Width = 274
      Height = 242
      Hint = 'Select the image for which you want to see the metadata'
      Align = alClient
      Mask = '*.jpg;*.jpeg;*.jpe;*.tiff;*.tif'
      ShowGlyphs = True
      TabOrder = 2
      OnChange = ShellListViewChange
    end
    object Panel4: TPanel
      Left = 0
      Top = 535
      Width = 274
      Height = 13
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 0
        Width = 79
        Height = 13
        Caption = 'Thumbnail image'
        Color = clBtnFace
        ParentColor = False
      end
    end
    object Panel5: TPanel
      Left = 0
      Top = 0
      Width = 274
      Height = 19
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 3
      OnResize = Panel5Resize
      object DriveComboBox1: TDriveComboBox
        Left = 0
        Top = 0
        Width = 274
        Height = 19
        TabOrder = 0
        OnChange = DriveComboBox1Change
      end
    end
  end
  object Panel2: TPanel
    Left = 279
    Top = 0
    Width = 651
    Height = 692
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 2
    object Splitter3: TSplitter
      Left = 0
      Top = 572
      Width = 651
      Height = 5
      Cursor = crVSplit
      Align = alBottom
    end
    object Panel3: TPanel
      Left = 0
      Top = 0
      Width = 651
      Height = 21
      Align = alTop
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 4
      TabOrder = 0
      object FilenameInfo: TLabel
        Left = 4
        Top = 4
        Width = 23
        Height = 13
        Caption = 'File: '
        Color = clBtnFace
        ParentColor = False
      end
    end
    object PageControl: TPageControl
      Left = 0
      Top = 21
      Width = 651
      Height = 551
      ActivePage = PgXMP
      Align = alClient
      TabOrder = 1
      OnChange = PageControlChange
      object PgMetadata: TTabSheet
        Caption = 'Meta data'
        object TagListView: TListView
          Left = 0
          Top = 0
          Width = 643
          Height = 504
          Align = alClient
          Columns = <
            item
              Caption = 'Group'
              Width = 120
            end
            item
              Caption = 'Tag ID'
              Width = 60
            end
            item
              Caption = 'Property'
              Width = 220
            end
            item
              AutoSize = True
              Caption = 'Value'
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnCompare = TagListViewCompare
          OnSelectItem = TagListViewSelectItem
        end
        object Panel1: TPanel
          Left = 0
          Top = 504
          Width = 643
          Height = 19
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 1
          object CbDecodeMakerNotes: TCheckBox
            Left = 0
            Top = 0
            Width = 127
            Height = 19
            Hint = 'Try to decode information in the MakerNote tag if possible'
            Caption = 'Decode MakerNotes'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
        end
      end
      object PgXMP: TTabSheet
        Caption = 'XMP'
        ImageIndex = 2
        object Splitter4: TSplitter
          Left = 0
          Top = 150
          Width = 643
          Height = 3
          Cursor = crVSplit
          Align = alTop
        end
        object XMPListView: TListView
          Left = 0
          Top = 0
          Width = 643
          Height = 150
          Align = alTop
          Columns = <
            item
              Caption = 'Description'
              Width = 200
            end
            item
              Caption = 'Value'
              Width = 200
            end>
          TabOrder = 0
          ViewStyle = vsReport
        end
        object XMPSynEdit: TMemo
          Left = 0
          Top = 153
          Width = 643
          Height = 336
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -12
          Font.Name = 'Courier New'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
        end
        object Panel6: TPanel
          Left = 0
          Top = 489
          Width = 643
          Height = 34
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object cbProcessXMP: TCheckBox
            Left = 8
            Top = 8
            Width = 129
            Height = 17
            Caption = 'Load && display XMP'
            Checked = True
            State = cbChecked
            TabOrder = 0
          end
          object btnApplyChangesXMP: TButton
            Left = 143
            Top = 7
            Width = 98
            Height = 25
            Caption = 'Apply changes'
            TabOrder = 1
            OnClick = btnApplyChangesXMPClick
          end
          object btnSaveXMP: TButton
            Left = 247
            Top = 7
            Width = 75
            Height = 25
            Caption = 'Save'
            TabOrder = 2
            OnClick = btnSaveXMPClick
          end
        end
      end
      object PgImage: TTabSheet
        Caption = 'Image'
        object Image: TImage
          Left = 0
          Top = 0
          Width = 643
          Height = 523
          Align = alClient
          Center = True
          Proportional = True
          Stretch = True
        end
      end
    end
    object Messages: TMemo
      Left = 0
      Top = 577
      Width = 651
      Height = 90
      Align = alBottom
      TabOrder = 2
    end
    object DateTimePanel: TPanel
      Left = 0
      Top = 667
      Width = 651
      Height = 25
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 3
      object LblChangeDate: TLabel
        Left = 4
        Top = 5
        Width = 124
        Height = 13
        Caption = 'Change EXIF date/time to'
        Color = clBtnFace
        ParentColor = False
      end
      object EdChangeDate: TEdit
        Left = 144
        Top = 1
        Width = 152
        Height = 21
        Hint = 'New date to be assigned to the selected image'
        TabOrder = 0
      end
      object BtnChangeDate: TButton
        Left = 304
        Top = 0
        Width = 67
        Height = 25
        Hint = 'Replaces the image date.'
        Caption = 'Execute'
        TabOrder = 1
        OnClick = BtnChangeDateClick
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 692
    Width = 930
    Height = 23
    Panels = <
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 250
      end
      item
        Width = 150
      end
      item
        Width = 100
      end>
  end
end
