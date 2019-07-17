object FactionForm: TFactionForm
  Tag = 3
  Left = 206
  Top = 103
  HelpContext = 9
  Anchors = []
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  Caption = 'Factions'
  ClientHeight = 430
  ClientWidth = 559
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000008FFFFFF000000000
    080000F0000000008FFFFFF00000000080000F8000000000FFFF880000000000
    80000F800000000008FFFFF000000000800F0FF000000000FFFFF00000000000
    FFFFF0000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000E00F0000E00F0000F00F0000E00F0000E00F0000E01F0000E00F
    0000F00F0000E00F0000E00F0000E01F0000E03F0000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FactionBox: TGroupBox
    Left = 372
    Top = 144
    Width = 181
    Height = 253
    Caption = 'My Faction'
    TabOrder = 0
    object lUnclaim: TLabel
      Left = 12
      Top = 24
      Width = 80
      Height = 13
      Caption = 'Unclaimed: 1000'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 12
      Top = 48
      Width = 76
      Height = 13
      Caption = 'Default Attitude:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object WarNum: TLabel
      Left = 112
      Top = 82
      Width = 54
      Height = 13
      Caption = '10 Regions'
    end
    object TradeNum: TLabel
      Left = 112
      Top = 106
      Width = 54
      Height = 13
      Caption = '10 Regions'
    end
    object MagicNum: TLabel
      Left = 112
      Top = 130
      Width = 36
      Height = 13
      Caption = '1 Mage'
    end
    object WarLabel: TLabel
      Left = 12
      Top = 82
      Width = 20
      Height = 13
      Caption = 'War'
    end
    object TradeLabel: TLabel
      Left = 12
      Top = 106
      Width = 28
      Height = 13
      Caption = 'Trade'
    end
    object MagicLabel: TLabel
      Left = 12
      Top = 130
      Width = 29
      Height = 13
      Caption = 'Magic'
    end
    object Label5: TLabel
      Left = 12
      Top = 176
      Width = 36
      Height = 13
      Caption = 'Leader:'
    end
    object Label6: TLabel
      Left = 28
      Top = 196
      Width = 140
      Height = 13
      Caption = '(unit to receive faction orders)'
    end
    object lAlignment: TLabel
      Left = 12
      Top = 220
      Width = 49
      Height = 13
      Caption = 'Alignment:'
    end
    object DefAttCombo: TComboBox
      Left = 92
      Top = 44
      Width = 77
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = DefAttComboChange
      Items.Strings = (
        'Hostile'
        'Unfriendly'
        'Neutral'
        'Friendly'
        'Ally')
    end
    object MagicTrack: TTrackBar
      Left = 44
      Top = 128
      Width = 65
      Height = 25
      LineSize = 2
      Max = 5
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 1
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = MagicTrackChange
    end
    object TradeTrack: TTrackBar
      Left = 44
      Top = 104
      Width = 65
      Height = 25
      LineSize = 2
      Max = 5
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 2
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = TradeTrackChange
    end
    object WarTrack: TTrackBar
      Left = 44
      Top = 80
      Width = 65
      Height = 25
      LineSize = 2
      Max = 5
      Orientation = trHorizontal
      Frequency = 1
      Position = 0
      SelEnd = 0
      SelStart = 0
      TabOrder = 3
      ThumbLength = 15
      TickMarks = tmBottomRight
      TickStyle = tsAuto
      OnChange = WarTrackChange
    end
    object LeaderBox: TComboBox
      Left = 64
      Top = 172
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      Sorted = True
      TabOrder = 4
      OnChange = LeaderBoxChange
    end
    object cmAlignment: TComboBox
      Left = 64
      Top = 216
      Width = 105
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 5
      OnChange = cmAlignmentChange
      Items.Strings = (
        'neutral'
        'good'
        'evil')
    end
  end
  object GroupBox1: TGroupBox
    Left = 372
    Top = 4
    Width = 181
    Height = 137
    Caption = 'Faction'
    TabOrder = 1
    object Label3: TLabel
      Left = 12
      Top = 24
      Width = 39
      Height = 13
      Caption = 'Attitude:'
    end
    object Label4: TLabel
      Left = 12
      Top = 50
      Width = 27
      Height = 13
      Caption = 'Color:'
    end
    object AttCombo: TComboBox
      Left = 56
      Top = 20
      Width = 73
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = AttComboChange
      Items.Strings = (
        'Hostile'
        'Unfriendly'
        'Neutral'
        'Friendly'
        'Ally')
    end
    object ColorBtn: TColorBtn
      Left = 56
      Top = 48
      Width = 34
      Height = 17
      Color = -1
      ColorDialog = ColorDialog
      Transparency = True
      OnClick = ColorBtnClick
    end
    object btnRequest: TButton
      Left = 12
      Top = 76
      Width = 85
      Height = 21
      Caption = 'Request EMail'
      TabOrder = 2
      OnClick = btnRequestClick
    end
    object cbHideUnconfirmed: TCheckBox
      Left = 16
      Top = 104
      Width = 149
      Height = 17
      Caption = 'Hide if email not confirmed'
      TabOrder = 3
      OnClick = cbHideUnconfirmedClick
    end
    object btnRequestAll: TButton
      Left = 100
      Top = 76
      Width = 69
      Height = 21
      Caption = 'Request All'
      TabOrder = 4
      OnClick = btnRequestAllClick
    end
  end
  object Button2: TButton
    Left = 476
    Top = 404
    Width = 75
    Height = 21
    Caption = 'Close'
    Default = True
    TabOrder = 2
    OnClick = Button2Click
  end
  object FactionGrid: TPowerGrid
    Left = 4
    Top = 4
    Width = 361
    Height = 421
    Color = clBlack
    ColCount = 4
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    ImageCol = 0
    Options = [pgoLines, pgoSortOnClick, pgoRowSelect]
    RowCount = 1
    Sorted = True
    SortBy = 0
    StickySelect = True
    TopRow = 1
    OnDrawCell = FactionGridDrawCell
    OnSelectCell = FactionGridSelectCell
    ColWidths = (
      135
      39
      59
      108)
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 504
    Top = 16
  end
end
