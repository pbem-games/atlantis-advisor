object MapExportForm: TMapExportForm
  Left = 203
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Export Map'
  ClientHeight = 290
  ClientWidth = 219
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    219
    290)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 28
    Top = 76
    Width = 104
    Height = 13
    Caption = 'Select items to export:'
  end
  object cbAdvances: TCheckBox
    Left = 32
    Top = 104
    Width = 129
    Height = 17
    Caption = 'Advanced resources'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 138
    Top = 262
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object Button2: TButton
    Left = 54
    Top = 262
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object cbBuildings: TCheckBox
    Left = 32
    Top = 124
    Width = 73
    Height = 17
    Caption = 'Buildings'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object cbTransports: TCheckBox
    Left = 32
    Top = 144
    Width = 81
    Height = 17
    Caption = 'Transports'
    TabOrder = 4
  end
  object cbPlayerUnits: TCheckBox
    Left = 32
    Top = 164
    Width = 85
    Height = 17
    Caption = 'Player'#39's units'
    TabOrder = 5
  end
  object cbOtherUnits: TCheckBox
    Left = 32
    Top = 204
    Width = 77
    Height = 17
    Caption = 'Other units'
    TabOrder = 6
  end
  object cbDetails: TCheckBox
    Left = 48
    Top = 184
    Width = 153
    Height = 17
    Caption = 'With details (items, skills etc)'
    TabOrder = 7
  end
  object rbMap: TRadioButton
    Left = 32
    Top = 20
    Width = 81
    Height = 17
    Caption = 'Whole map'
    Checked = True
    TabOrder = 8
    TabStop = True
  end
  object rbRegion: TRadioButton
    Left = 32
    Top = 40
    Width = 101
    Height = 17
    Caption = 'Selected region'
    TabOrder = 9
  end
  object cbGates: TCheckBox
    Left = 32
    Top = 224
    Width = 61
    Height = 17
    Caption = 'Gates'
    TabOrder = 10
  end
end
