object ScriptEditForm: TScriptEditForm
  Left = 203
  Top = 106
  Width = 550
  Height = 450
  Caption = 'Scripts'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Edit: TMemoEx
    Left = 0
    Top = 29
    Width = 542
    Height = 394
    Cursor = crIBeam
    TabOrder = 0
    GutterWidth = 25
    RightMarginVisible = False
    RightMarginColor = clSilver
    Completion.Enabled = False
    Completion.Separator = '='
    Completion.ItemHeight = 13
    Completion.Interval = 800
    Completion.ListBoxStyle = lbStandard
    Completion.CaretChar = '|'
    Completion.CRLF = '/n'
    TabSize = 2
    IndentSize = 2
    AutoIndentSize = 0
    SmartTab = False
    SelForeColor = clHighlightText
    SelBackColor = clHighlight
    OnGetLineAttr = EditGetLineAttr
    OnKeyUp = EditKeyUp
    OnPaintGutter = EditPaintGutter
    Align = alClient
    Ctl3D = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Pitch = fpFixed
    Font.Style = []
    ParentColor = False
    TabStop = True
    UseDockManager = False
    WordWrap = False
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 542
    Height = 29
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    object ToolButton1: TToolButton
      Left = 0
      Top = 2
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object btnSave: TToolButton
      Left = 8
      Top = 2
      Hint = 'Save Scripts'
      Caption = 'btnSave'
      ImageIndex = 58
      OnClick = btnSaveClick
    end
  end
end
