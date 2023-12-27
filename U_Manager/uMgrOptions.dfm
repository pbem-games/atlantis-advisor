object ManagerOptionsForm: TManagerOptionsForm
  Left = 201
  Top = 103
  BorderStyle = bsDialog
  Caption = 'Game Manager'
  ClientHeight = 287
  ClientWidth = 332
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  DesignSize = (
    332
    287)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 156
    Top = 260
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 240
    Top = 260
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 156
    Width = 317
    Height = 89
    Caption = 'Engine for local games'
    TabOrder = 2
    object Label18: TLabel
      Left = 20
      Top = 24
      Width = 92
      Height = 13
      Caption = 'Path to executable:'
    end
    object eEngineFile: TFilenameEdit
      Left = 12
      Top = 44
      Width = 293
      Height = 21
      Filter = 'Executable files (*.exe)|*.exe|All files (*.*)|*.*'
      NumGlyphs = 1
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 8
    Width = 317
    Height = 141
    Caption = 'Options'
    TabOrder = 3
    object Label2: TLabel
      Left = 32
      Top = 27
      Width = 25
      Height = 13
      Caption = 'Keep'
    end
    object Label3: TLabel
      Left = 118
      Top = 27
      Width = 122
      Height = 13
      Caption = 'turns, write older to history'
    end
    object KeepEdit: TSpinEdit
      Left = 68
      Top = 24
      Width = 41
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
    object MoveRepCheck: TCheckBox
      Left = 32
      Top = 56
      Width = 185
      Height = 17
      Caption = 'Move added reports to game folder'
      TabOrder = 1
    end
    object cbLastOrder: TCheckBox
      Left = 32
      Top = 96
      Width = 157
      Height = 17
      Caption = 'Try to load order for last turn'
      TabOrder = 2
    end
    object PromptWrongCheck: TCheckBox
      Left = 32
      Top = 76
      Width = 165
      Height = 17
      Caption = 'Prompt to remove bad reports'
      TabOrder = 3
    end
  end
end
