object MainForm: TMainForm
  Left = 299
  Top = 192
  Width = 1040
  Height = 599
  ActiveControl = BtnRunTest
  Caption = 'Multi read test'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 403
    Top = 33
    Width = 5
    Height = 527
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1024
    Height = 33
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    DesignSize = (
      1024
      33)
    object BtnReadFiles: TButton
      Left = 945
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Read files'
      TabOrder = 0
      OnClick = BtnReadFilesClick
    end
    object EdImageDir: TEdit
      Left = 4
      Top = 4
      Width = 931
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = '..\pictures\originals'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 403
    Height = 527
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'Panel2'
    TabOrder = 1
    object Bevel1: TBevel
      Left = 4
      Top = 494
      Width = 395
      Height = 4
      Align = alBottom
      Shape = bsSpacer
    end
    object FileTreeView: TTreeView
      Left = 4
      Top = 4
      Width = 395
      Height = 490
      Align = alClient
      Images = ImageList1
      Indent = 19
      ReadOnly = True
      StateImages = StateImages
      TabOrder = 0
      OnClick = FileTreeViewClick
    end
    object Panel3: TPanel
      Left = 4
      Top = 498
      Width = 395
      Height = 25
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 1
      object BtnCreateTxtFiles: TButton
        Left = 0
        Top = 0
        Width = 100
        Height = 25
        Caption = 'Create txt files'
        TabOrder = 0
        OnClick = BtnCreateTxtFilesClick
      end
      object BtnUncheckAll: TButton
        Left = 104
        Top = 0
        Width = 75
        Height = 25
        Caption = 'Uncheck all'
        TabOrder = 1
        OnClick = BtnUncheckAllClick
      end
      object BtnCheckAll: TButton
        Left = 184
        Top = 0
        Width = 75
        Height = 25
        Caption = 'Check all'
        TabOrder = 2
        OnClick = BtnUncheckAllClick
      end
    end
  end
  object Panel4: TPanel
    Left = 408
    Top = 33
    Width = 616
    Height = 527
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object Bevel2: TBevel
      Left = 4
      Top = 492
      Width = 608
      Height = 4
      Align = alBottom
      Shape = bsSpacer
    end
    object Panel5: TPanel
      Left = 4
      Top = 496
      Width = 608
      Height = 27
      Align = alBottom
      AutoSize = True
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        608
        27)
      object MismatchInfo: TLabel
        Left = 0
        Top = 0
        Width = 361
        Height = 27
        Align = alLeft
        AutoSize = False
        Caption = 'MismatchInfo'
        Color = clBtnFace
        ParentColor = False
        Layout = tlCenter
      end
      object BtnRunTest: TButton
        Left = 539
        Top = 0
        Width = 69
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Run test'
        TabOrder = 0
        OnClick = BtnRunTestClick
      end
      object BtnInfo: TButton
        Left = 456
        Top = 0
        Width = 75
        Height = 25
        Anchors = [akTop, akRight]
        Caption = 'Info'
        TabOrder = 1
        OnClick = BtnInfoClick
      end
    end
    object Memo: TMemo
      Left = 4
      Top = 4
      Width = 608
      Height = 488
      Align = alClient
      ScrollBars = ssBoth
      TabOrder = 1
    end
  end
  object ImageList1: TImageList
    Left = 99
    Top = 111
    Bitmap = {
      494C010104000900300010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000D7CCC400A57E5E00B2805600AF7E
      5200A47A5900CFC2B70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003F3D
      ED413B38EB08000000000000000000000000000000000000000000000000211F
      E3081E1CE2410000000000000000000000000000000000000000000000000000
      00000000000000000000E6E0DA00A9876A00B2815800CBAB8900D1B49500BB8E
      6300B5875A00AB774D00A3806300E1D9D4000000000000000000000000000000
      000000000000317A360A2D753207000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000077130900000000000000000000
      00000000000000000000000000000000000000000000000000004A47F0414F4C
      F2FF403EEDFD3C39EB08000000000000000000000000000000002725E5082422
      E4FC312FEAFF1F1DE24100000000000000000000000000000000000000000000
      0000F3F6F400FAFBFA00AD805700D5BB9F00D6BB9E00D3B89C00D1B39400B789
      5D00BA8E6200B88D6100B2815600A8764E000000000000000000000000000000
      00003985400A37833DFF317B37FB2E7633070000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000007D21F5037B1EFF00791521000000000000
      000000000000000000000000000000000000000000005451F3415856F5FF6361
      FAFF5855F6FF413FEDFC3D3AEC080000000000000000302DE7082C2AE6FC413F
      F1FF4C4AF6FF312FEAFF1F1DE241000000000000000000000000C6D4C700689A
      6C0063A26A0061A16900B17E5200E1CDB800D8C0A500D8C0A700D4BA9D00B88C
      6000B78A6000B88D6100BA8E6200B17E52000000000000000000000000004292
      490A408E47FF54A35CFF4F9F57FF327C38FE2E77340800000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000001832BF543A15FFF007B1FE4007919270000
      000000000000000000000000000000000000000000005956F52B5B58F6FF6562
      FAFF7170FFFF5956F6FF4240EEFC3E3BEC083937EB083532E9FC4745F2FF6362
      FFFF4A48F4FF2F2DE9FF2220E32B00000000DCE4DD0076A07A0066A36C0093C0
      99009EC7A40071AC7800AF7E5100E3D0BC00DAC3AB00D3B89E00C7A37D00C198
      6F00B6895C00B78A6000BA8E6200B180540000000000000000004B9E530A499A
      51FF5BAC64FF77CA82FF74C87EFF51A059FF337D39FE2F783508000000000000
      0000000000000000000000000000000000000000000000000000000000002197
      51FE1B9149FE158F43FE0F8B3BFE3A9F5EFF80C196FF46A362FF007D1FE70079
      192A0000000000000000000000000000000000000000000000005A57F52B5B59
      F6FF6663FAFF7471FFFF5A58F6FF4341EEFC3E3CECFD504DF4FF6867FFFF504E
      F5FF3634EBFF2A27E52B0000000000000000649F6C00A9CDAF00A6CCAC00A2C9
      A90099C59F006BA97400AE7C4F00DCC8B000BF9F8100B88D6500D1B38F00D1B3
      8F00BB906600BC916800B78A6000B17E52000000000053A95C0A51A65AFF63B5
      6DFF7ECE89FF7BCC87FF76CA81FF76C981FF52A25AFF347E3AFE307935080000
      000000000000000000000000000000000000000000000000000000000000299B
      5BFF90CAA9FF8DC8A5FF8AC6A1FF88C59EFF6AB685FF82C297FF48A566FF007D
      21EA00791B300000000000000000000000000000000000000000000000005B58
      F62B5C5AF6FF6764FAFF7472FFFF7370FFFF706EFFFF6E6CFFFF5755F7FF3F3D
      EEFF3230E82B00000000000000000000000062A16900C0DAC500ADD0B300ABCE
      B1009EC8A6006DAA7600957B7E005A61C8005058E3004F56E000585FC8009078
      8400BB906600D1B38F00C6A27B00A97950005AB4650959B063FF6BBD76FF84D2
      90FF7AC985FF60B26AFF63B46DFF78C983FF78CB82FF53A35CFF347F3AFD317A
      360800000000000000000000000000000000000000000000000000000000319F
      63FF94CDADFF6FBA8EFF6BB889FF66B685FF61B380FF67B582FF83C298FF3CA0
      5CFF007F25FC0000000000000000000000000000000000000000000000000000
      00005C59F62B5D5BF7FF7976FFFF5956FFFF5754FFFF7270FFFF4846F0FF3C39
      EB2B0000000000000000000000000000000060A06800C5DEC900B4D4B900A4C9
      AA0081AB9A00616DC3005058E0006668EB009393F4006163EA00585BE4004952
      DC006063BE00A6897F00C19A7100B89F8B005EB969465BB566E479C986FF80CE
      8DFF51A65AFC4DA1566F499C518B5CAD67FF7CCC86FF79CB85FF54A45DFF3580
      3BFC317B370800000000000000000000000000000000000000000000000037A3
      6BFF96CEB0FF94CDADFF91CBAAFF90CBA8FF74BC90FF8AC7A1FF46A568FF0787
      35FD01832D0F0000000000000000000000000000000000000000000000000000
      0000615EF8085D5AF6FD7D79FFFF5E5BFFFF5B58FFFF7674FFFF4643EFFD413F
      ED08000000000000000000000000000000005D9F6500B9D6BE0087BA8F0071AC
      78005359DC00666AEB009896F4009191F300898AF0005B5FE7005F62E9005D61
      E8005158E4004A55D800E3DDDB00FCFAFA00000000005FBA6A3C5CB666E66DC0
      79FF55AC5F6F00000000000000004A9D52915EAE68FF7DCD89FF7CCD87FF56A5
      5FFF36813CFC327C380800000000000000000000000000000000000000003DA5
      6FFF37A36DFD33A167FD2F9D61FD55AF7CFF91CBAAFF4FAB74FF178F45FD118B
      3D0C000000000000000000000000000000000000000000000000000000006967
      FB086663F9FC706DFBFF807EFFFF7E7BFFFF7C79FFFF7977FFFF5E5CF7FF4744
      EFFC4240EE0800000000000000000000000065A06C0086BA8F0099C6A20074AD
      7C004F57E200B4B1F9009796F4009393F4008C8DF0005C60E8005C61E7005D61
      E8005F62E9004F57E200E4E5F2000000000000000000000000005FBB6A435CB7
      6765000000000000000000000000000000004B9E53915FAF69FF7FCE8AFF7ECE
      89FF57A660FF37823DFC337D3908000000000000000000000000000000000000
      0000000000000000000000000000319F63F55AB381FF289857FF1F954F090000
      0000000000000000000000000000000000000000000000000000716EFD086E6B
      FCFC7774FDFF8682FFFF7673FCFF6462F8FF605DF7FF6D6AFAFF7B79FFFF605D
      F7FF4845EFFC4341EE08000000000000000093B397007CB4850076AF7E006FAB
      78004E54E100B4B1F9009596F500666AEB006F71EC006E72EC005A5CE5005C61
      E7005F62E9005158E200E4E5F200000000000000000000000000000000000000
      000000000000000000000000000000000000000000004B9F549160B06AFF81CF
      8DFF7FCF8BFF58A761FF398540FF347E3A080000000000000000000000000000
      000000000000000000000000000037A36BF5319F65FF2D9D5F09000000000000
      000000000000000000000000000000000000000000007673FF087471FEFD7D7A
      FEFF8A87FFFF7C79FDFF6C69FBFF6361F92B5F5CF72B615EF8FF6E6CFAFF7D7A
      FFFF615FF7FF4946F0FC4441EE0500000000FAFBFA00DFE6DF00CBD7CC006EA8
      77004C52E000A2A2F4006A6CEC006163EA009793F7009793F7006468E9006566
      EA005C61E7004F57E200E4E5F200000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004CA0559162B2
      6CFF82D18FFF7AC885FF57A660FF38843F7B0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000007774FF1F7A77FFFF817E
      FFFF817EFEFF7471FDFF6C69FB2B0000000000000000605DF72B625FF8FF6F6D
      FBFF7E7CFFFF625FF8FF4A47F06F4542EE020000000000000000000000000000
      0000555BDB007C7CF2009793F7006468E9005258E3005258E3006468E9009793
      F7007C7CF2004E57D900E9EAF500000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004DA1
      569163B36DFF5FAF69FF41914979000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000007774FF1F7A77
      FFFF7976FEFF726FFD2B00000000000000000000000000000000615EF82B6461
      F8FF6A68F9FF5451F3A84F4DF229000000000000000000000000000000000000
      00009195D9006E6FEC006668EB005F62E9007878F0007474F0005F62E900696B
      EB006F71EC009094D70000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00004EA257914A9D527F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007774
      FF1F7774FF2B000000000000000000000000000000000000000000000000625F
      F82B5D5BF76F5956F53E00000000000000000000000000000000000000000000
      0000FAFAFD00DFE1F100CBCDE7006163E3005157E2005157E2005F62E300C9CC
      E600DFE0F100FAFAFD0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006360F80A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000FFFFFFFFFF03FFFFFFFFE7E7FC00F9FF
      FF7FC3C3F000F0FFFE3F8181C000E07FFE1F80010000C03FE00FC0030000801F
      E007E0070000000FE007F00F00000007E007F00F00008603E00FE0070001CF01
      FE1FC0030001FF80FE3F80010001FFC0FFFF8180F001FFE1FFFFC3C1F003FFF3
      FFFFE7E3F003FFFFFFFFFFF7FFFFFFFF00000000000000000000000000000000
      000000000000}
  end
  object StateImages: TImageList
    Left = 99
    Top = 176
    Bitmap = {
      494C010103003800480010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000001919
      1926151515661515157515151575151515751515157515151575151515751515
      1566191919260000000000000000000000000000000000000000000000001919
      1926151515661515157515151575151515751515157515151575151515751515
      1566191919260000000000000000000000000000000000000000000000001919
      1926151515661515157515151575151515751515157515151575151515751515
      1566191919260000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000003838
      3862CCCCCCD6E9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFCCCC
      CCD6383838620000000000000000000000000000000000000000000000003838
      3862CCCCCCD6E9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFCCCC
      CCD6383838620000000000000000000000000000000000000000000000003838
      3862CCCCCCD6E9E9E9FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE9E9E9FFCCCC
      CCD6383838620000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000004949
      496EEBEBEBFFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFEBEB
      EBFF4949496E0000000000000000000000000000000000000000000000004949
      496EEBEBEBFFE3E3E3FFD3D3D3FF5C5C5CFFD3D3D3FFE3E3E3FFE3E3E3FFEBEB
      EBFF4949496E0000000000000000000000000000000000000000000000004949
      496EEBEBEBFFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFE3E3E3FFEBEB
      EBFF4949496E0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005252
      526DEEEEEEFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFEEEE
      EEFF5252526D0000000000000000000000000000000000000000000000005252
      526DEEEEEEFFD9D9D9FF6B6B6BFF6B6B6BFF6B6B6BFFD9D9D9FFE8E8E8FFEEEE
      EEFF5252526D0000000000000000000000000000000000000000000000005252
      526DEEEEEEFFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFE8E8E8FFEEEE
      EEFF5252526D0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000005C5C
      5C6CF1F1F1FFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFF1F1
      F1FF5C5C5C6C0000000000000000000000000000000000000000000000005C5C
      5C6CF1F1F1FF7C7C7CFF7C7C7CFFD0D0D0FF7C7C7CFF7C7C7CFFDFDFDFFFF1F1
      F1FF5C5C5C6C0000000000000000000000000000000000000000000000005C5C
      5C6CF1F1F1FFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFEDEDEDFFF1F1
      F1FF5C5C5C6C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006464
      646AF6F6F6FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF6F6
      F6FF6464646A0000000000000000000000000000000000000000000000006464
      646AF6F6F6FF8C8C8CFFE5E5E5FFF2F2F2FFE5E5E5FF8C8C8CFF8C8C8CFFF6F6
      F6FF6464646A0000000000000000000000000000000000000000000000006464
      646AF6F6F6FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF2F2F2FFF6F6
      F6FF6464646A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000006C6C
      6C69F9F9F9FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF9F9
      F9FF6C6C6C690000000000000000000000000000000000000000000000006C6C
      6C69F9F9F9FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFEBEBEBFF979797FFF9F9
      F9FF6C6C6C690000000000000000000000000000000000000000000000006C6C
      6C69F9F9F9FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF7F7F7FFF9F9
      F9FF6C6C6C690000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007474
      7468FDFDFDFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFDFD
      FDFF747474680000000000000000000000000000000000000000000000007474
      7468FDFDFDFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFE3E3E3FFFDFD
      FDFF747474680000000000000000000000000000000000000000000000007474
      7468FDFDFDFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFBFBFBFFFDFD
      FDFF747474680000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007A7A
      7A5AE9E9E9D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9
      E9D37A7A7A5A0000000000000000000000000000000000000000000000007A7A
      7A5AE9E9E9D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9
      E9D37A7A7A5A0000000000000000000000000000000000000000000000007A7A
      7A5AE9E9E9D3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9
      E9D37A7A7A5A0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000007F7F
      7F227F7F7F597F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F
      7F597F7F7F220000000000000000000000000000000000000000000000007F7F
      7F227F7F7F597F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F
      7F597F7F7F220000000000000000000000000000000000000000000000007F7F
      7F227F7F7F597F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F7F667F7F
      7F597F7F7F220000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000E007E007E0070000E007E007E0070000E007E007E0070000
      E007E007E0070000E007E007E0070000E007E007E0070000E007E007E0070000
      E007E007E0070000E007E007E0070000E007E007E0070000FFFFFFFFFFFF0000
      FFFFFFFFFFFF0000FFFFFFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
end