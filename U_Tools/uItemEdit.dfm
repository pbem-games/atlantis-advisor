object ItemEditForm: TItemEditForm
  Left = 657
  Top = 354
  HelpContext = 10
  BorderStyle = bsDialog
  Caption = 'Item Editor'
  ClientHeight = 453
  ClientWidth = 585
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    585
    453)
  PixelsPerInch = 96
  TextHeight = 13
  object ItemGrid: TPowerGrid
    Left = 4
    Top = 32
    Width = 233
    Height = 411
    ColCount = 5
    DefaultRowColor = clBlack
    Editing = False
    FixedRows = 1
    Images = ResForm.IconList
    ImageCol = 1
    Options = [pgoLines, pgoColSizing, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
    RowCount = 1
    Sorted = True
    SortBy = 1
    StickySelect = True
    TopRow = 1
    OnDragOver = ItemGridDragOver
    OnEndDrag = ItemGridEndDrag
    OnMouseDown = ItemGridMouseDown
    OnSelectCell = ItemGridSelectCell
    ColWidths = (
      18
      41
      68
      32
      70)
  end
  object ToolBar1: TToolBar
    Left = 4
    Top = 4
    Width = 177
    Height = 25
    Align = alNone
    Caption = 'ToolBar1'
    EdgeBorders = []
    Images = ResForm.BtnImages
    TabOrder = 1
    object btnNoFilter: TToolButton
      Left = 0
      Top = 2
      Hint = 'No filter'
      Caption = 'btnNoFilter'
      Down = True
      Grouped = True
      ImageIndex = 9
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = btnFilterClick
    end
    object btnFilter: TToolButton
      Tag = 1
      Left = 23
      Top = 2
      Hint = 'Filter by current item'
      Caption = 'btnFilter'
      Grouped = True
      ImageIndex = 25
      ParentShowHint = False
      ShowHint = True
      Style = tbsCheck
      OnClick = btnFilterClick
    end
    object ToolButton2: TToolButton
      Left = 46
      Top = 2
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 35
      Style = tbsSeparator
    end
    object btnArrange: TToolButton
      Left = 54
      Top = 2
      Hint = 'Arrange item indexes'
      Caption = 'btnArrange'
      ImageIndex = 34
      ParentShowHint = False
      ShowHint = True
      OnClick = btnArrangeClick
    end
    object ToolButton4: TToolButton
      Left = 77
      Top = 2
      Width = 8
      Caption = 'ToolButton4'
      ImageIndex = 37
      Style = tbsSeparator
    end
    object btnRequest: TToolButton
      Left = 85
      Top = 2
      Hint = 'Request Info'
      Caption = 'btnRequest'
      ImageIndex = 69
      ParentShowHint = False
      ShowHint = True
      OnClick = btnRequestClick
    end
    object btnRequestAll: TToolButton
      Left = 108
      Top = 2
      Hint = 'Request All'
      Caption = 'btnRequestAll'
      ImageIndex = 70
      ParentShowHint = False
      ShowHint = True
      OnClick = btnRequestAllClick
    end
  end
  object GroupBox1: TGroupBox
    Left = 244
    Top = 4
    Width = 337
    Height = 213
    Caption = 'Item'
    TabOrder = 2
    object Label3: TLabel
      Left = 12
      Top = 48
      Width = 27
      Height = 13
      Caption = 'Type:'
    end
    object Label9: TLabel
      Left = 12
      Top = 20
      Width = 28
      Height = 13
      Caption = 'Code:'
    end
    object CodeLabel: TLabel
      Left = 48
      Top = 20
      Width = 33
      Height = 13
      Caption = 'BAXE'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label4: TLabel
      Left = 108
      Top = 20
      Width = 37
      Height = 13
      Caption = 'Weight:'
    end
    object Label59: TLabel
      Left = 12
      Top = 76
      Width = 30
      Height = 13
      Caption = 'Ability:'
    end
    object cmType: TComboBox
      Left = 48
      Top = 44
      Width = 101
      Height = 22
      Style = csOwnerDrawFixed
      DropDownCount = 11
      ItemHeight = 16
      TabOrder = 0
      OnChange = DataChange
      OnDrawItem = cmTypeDrawItem
      Items.Strings = (
        'Other'
        'Man'
        'Monster'
        'Weapon'
        'Armor'
        'Mount'
        'Wagon'
        'Silver'
        'Food'
        'Trade goods'
        'Ship')
    end
    object cbTool: TCheckBox
      Left = 216
      Top = 16
      Width = 49
      Height = 17
      Caption = 'Tool'
      TabOrder = 1
      OnClick = DataChange
    end
    object cbCantgive: TCheckBox
      Left = 216
      Top = 32
      Width = 93
      Height = 17
      Caption = 'Can'#39't be given'
      TabOrder = 2
      OnClick = DataChange
    end
    object cbAdvanced: TCheckBox
      Left = 216
      Top = 64
      Width = 77
      Height = 17
      Caption = 'Advanced'
      TabOrder = 3
      OnClick = DataChange
    end
    object WeightEdit: TIntEdit
      Left = 152
      Top = 16
      Width = 49
      Height = 22
      MaxValue = 9999
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = DataChange
    end
    object mDescription: TRichEdit
      Left = 8
      Top = 104
      Width = 321
      Height = 101
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 5
    end
    object cbResource: TCheckBox
      Left = 216
      Top = 48
      Width = 73
      Height = 17
      Caption = 'Resource'
      TabOrder = 6
      OnClick = DataChange
    end
    object cbMagic: TCheckBox
      Left = 216
      Top = 80
      Width = 105
      Height = 17
      Caption = 'Misc combat item'
      TabOrder = 7
      OnClick = DataChange
    end
    object cmAbility: TComboBox
      Left = 48
      Top = 72
      Width = 101
      Height = 22
      Style = csOwnerDrawFixed
      ItemHeight = 16
      Sorted = True
      TabOrder = 8
      OnChange = DataChange
      OnDrawItem = cmAbilityDrawItem
    end
    object eAbilityLv: TIntEdit
      Left = 152
      Top = 72
      Width = 49
      Height = 22
      MaxValue = 99
      MinValue = 0
      TabOrder = 9
      Value = 0
      OnChange = DataChange
    end
    object cmAlignment: TComboBox
      Left = 152
      Top = 44
      Width = 49
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 10
      OnChange = DataChange
      Items.Strings = (
        ''
        'good'
        'evil')
    end
  end
  object Button1: TButton
    Left = 503
    Top = 429
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object PControl: TPageControl
    Left = 244
    Top = 220
    Width = 337
    Height = 205
    ActivePage = tsFood
    MultiLine = True
    TabIndex = 12
    TabOrder = 4
    OnChange = PControlChange
    object tsProduction: TTabSheet
      Caption = 'Production'
      ImageIndex = 7
      object Label8: TLabel
        Left = 8
        Top = 16
        Width = 22
        Height = 13
        Caption = 'Skill:'
      end
      object Label2: TLabel
        Left = 48
        Top = 124
        Width = 15
        Height = 13
        Caption = 'per'
      end
      object Label6: TLabel
        Left = 8
        Top = 104
        Width = 26
        Height = 13
        Caption = 'Rate:'
      end
      object Label7: TLabel
        Left = 176
        Top = 16
        Width = 69
        Height = 13
        Caption = 'Raw materials:'
      end
      object Label11: TLabel
        Left = 8
        Top = 60
        Width = 121
        Height = 13
        Caption = 'Production bonus for tool:'
      end
      object Label1: TLabel
        Left = 108
        Top = 124
        Width = 57
        Height = 13
        Caption = 'man-months'
      end
      object cmMaterial: TComboBox
        Left = 172
        Top = 135
        Width = 113
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 0
        OnDrawItem = cmIDataDrawItem
      end
      object cmSkill: TComboBox
        Left = 8
        Top = 31
        Width = 121
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 1
        OnChange = DataChange
        OnDrawItem = cmSkillDrawItem
      end
      object eSkillLv: TIntEdit
        Left = 132
        Top = 31
        Width = 33
        Height = 22
        MaxValue = 5
        MinValue = 1
        TabOrder = 2
        Value = 1
        OnChange = DataChange
      end
      object eProdRate: TIntEdit
        Left = 8
        Top = 120
        Width = 37
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = DataChange
      end
      object eProdManMonths: TIntEdit
        Left = 68
        Top = 120
        Width = 37
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnChange = DataChange
      end
      object ToolBar3: TToolBar
        Left = 288
        Top = 137
        Width = 45
        Height = 21
        Align = alNone
        ButtonHeight = 18
        ButtonWidth = 19
        Caption = 'ToolBar3'
        EdgeBorders = []
        Flat = True
        Images = ResForm.SmallBtnList
        TabOrder = 5
        object btnProdAdd: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnProdAdd'
          ImageIndex = 0
          OnClick = btnProdAddClick
        end
        object btnProdDel: TToolButton
          Left = 19
          Top = 0
          Caption = 'btnProdDel'
          ImageIndex = 1
          OnClick = btnProdDelClick
        end
      end
      object cbAnyOf: TCheckBox
        Left = 272
        Top = 11
        Width = 53
        Height = 17
        Caption = 'any of'
        TabOrder = 6
        OnClick = DataChange
      end
      object cmProdTool: TComboBox
        Left = 8
        Top = 76
        Width = 121
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 7
        OnChange = DataChange
        OnDrawItem = cmIDataDrawItem
      end
      object eProdToolBonus: TIntEdit
        Left = 132
        Top = 76
        Width = 33
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 8
        Value = 0
        OnChange = DataChange
      end
      object gMaterials: TPowerGrid
        Left = 172
        Top = 32
        Width = 149
        Height = 101
        ColCount = 2
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 0
        ImageCol = 1
        Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 0
        Sorted = False
        SortBy = 0
        StickySelect = False
        TopRow = 0
        OnDrawIcon = DrawIDataIcon
        OnSetEditText = gMaterialsSetEditText
        ColWidths = (
          41
          104)
      end
    end
    object tsMagProduction: TTabSheet
      Caption = 'Mag. Production'
      ImageIndex = 7
      object Label25: TLabel
        Left = 8
        Top = 16
        Width = 26
        Height = 13
        Caption = 'Spell:'
      end
      object Label26: TLabel
        Left = 176
        Top = 16
        Width = 69
        Height = 13
        Caption = 'Raw materials:'
      end
      object Label24: TLabel
        Left = 88
        Top = 63
        Width = 34
        Height = 13
        Caption = 'at level'
      end
      object cmMProdSpell: TComboBox
        Left = 8
        Top = 31
        Width = 157
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 0
        OnChange = DataChange
        OnDrawItem = cmSkillDrawItem
      end
      object eMProdSpell: TIntEdit
        Left = 132
        Top = 59
        Width = 33
        Height = 22
        MaxValue = 5
        MinValue = 1
        TabOrder = 1
        Value = 1
        OnChange = DataChange
      end
      object cmMagMaterial: TComboBox
        Left = 172
        Top = 135
        Width = 113
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 2
        OnDrawItem = cmIDataDrawItem
      end
      object ToolBar2: TToolBar
        Left = 288
        Top = 137
        Width = 45
        Height = 21
        Align = alNone
        ButtonHeight = 18
        ButtonWidth = 19
        Caption = 'ToolBar2'
        EdgeBorders = []
        Flat = True
        Images = ResForm.SmallBtnList
        TabOrder = 3
        object btnAddMRaw: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnAddMRaw'
          ImageIndex = 0
          OnClick = btnAddMRawClick
        end
        object btnDelMRaw: TToolButton
          Left = 19
          Top = 0
          Caption = 'btnDelMRaw'
          ImageIndex = 1
          OnClick = btnDelMRawClick
        end
      end
      object gMagMaterials: TPowerGrid
        Left = 172
        Top = 32
        Width = 149
        Height = 101
        ColCount = 2
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 0
        ImageCol = 1
        Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 0
        Sorted = False
        SortBy = 0
        StickySelect = False
        TopRow = 0
        OnDrawIcon = DrawIDataIcon
        OnSetEditText = gMagMaterialsSetEditText
        ColWidths = (
          41
          104)
      end
    end
    object tsByproducts: TTabSheet
      Caption = 'Byproducts'
      ImageIndex = 10
      object Label60: TLabel
        Left = 176
        Top = 16
        Width = 56
        Height = 13
        Caption = 'Byproducts:'
      end
      object Label61: TLabel
        Left = 8
        Top = 16
        Width = 92
        Height = 13
        Caption = 'Unit must be inside:'
      end
      object gByproducts: TPowerGrid
        Left = 172
        Top = 32
        Width = 149
        Height = 101
        ColCount = 2
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 0
        ImageCol = 1
        Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 0
        Sorted = False
        SortBy = 0
        StickySelect = False
        TopRow = 0
        OnDrawIcon = DrawIDataIcon
        OnSetEditText = gByproductsSetEditText
        ColWidths = (
          40
          105)
      end
      object cmByproduct: TComboBox
        Left = 172
        Top = 135
        Width = 113
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 1
        OnDrawItem = cmIDataDrawItem
      end
      object ToolBar4: TToolBar
        Left = 288
        Top = 137
        Width = 45
        Height = 21
        Align = alNone
        ButtonHeight = 18
        ButtonWidth = 19
        Caption = 'ToolBar3'
        EdgeBorders = []
        Flat = True
        Images = ResForm.SmallBtnList
        TabOrder = 2
        object btnAddByprod: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnProdAdd'
          ImageIndex = 0
          OnClick = btnAddByprodClick
        end
        object btnDelByprod: TToolButton
          Left = 19
          Top = 0
          Caption = 'btnProdDel'
          ImageIndex = 1
          OnClick = btnDelByprodClick
        end
      end
      object cmRequiredStruct: TComboBox
        Left = 8
        Top = 31
        Width = 157
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 3
        OnChange = DataChange
        OnDrawItem = cmRequiredStructDrawItem
      end
    end
    object tsMan: TTabSheet
      Caption = 'Man'
      object Label12: TLabel
        Left = 136
        Top = 8
        Width = 63
        Height = 13
        Caption = 'Special skills:'
      end
      object cbLeader: TCheckBox
        Left = 12
        Top = 8
        Width = 61
        Height = 17
        Caption = 'Leader'
        TabOrder = 0
        OnClick = DataChange
      end
      object cmSpecSkills: TComboBox
        Left = 136
        Top = 136
        Width = 141
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 1
        OnDrawItem = cmSkillDrawItem
      end
      object GroupBox2: TGroupBox
        Left = 4
        Top = 36
        Width = 125
        Height = 121
        Caption = 'Skill level'
        TabOrder = 2
        object lMagDefLevel: TLabel
          Left = 20
          Top = 60
          Width = 32
          Height = 13
          Caption = 'Magic:'
        end
        object Label10: TLabel
          Left = 20
          Top = 28
          Width = 37
          Height = 13
          Caption = 'Default:'
        end
        object eMagDefLevel: TIntEdit
          Left = 64
          Top = 55
          Width = 37
          Height = 22
          MaxValue = 5
          MinValue = 0
          TabOrder = 0
          Value = 1
          OnChange = DataChange
        end
        object eDefLevel: TIntEdit
          Left = 64
          Top = 24
          Width = 37
          Height = 22
          MaxValue = 5
          MinValue = 0
          TabOrder = 1
          Value = 1
          OnChange = DataChange
        end
      end
      object gManSkills: TPowerGrid
        Left = 136
        Top = 24
        Width = 181
        Height = 109
        ColCount = 2
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 0
        Images = ResForm.IconList
        ImageCol = 0
        Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 0
        Sorted = False
        SortBy = 0
        StickySelect = False
        TopRow = 0
        OnSetEditText = gManSkillsSetEditText
        ColWidths = (
          132
          45)
      end
      object ToolBar5: TToolBar
        Left = 280
        Top = 138
        Width = 49
        Height = 21
        Align = alNone
        ButtonHeight = 18
        ButtonWidth = 19
        Caption = 'ToolBar5'
        EdgeBorders = []
        Flat = True
        Images = ResForm.SmallBtnList
        TabOrder = 4
        object btnAddSpecSkill: TToolButton
          Left = 0
          Top = 0
          Caption = 'btnAddSpecSkill'
          ImageIndex = 0
          OnClick = btnAddSpecSkillClick
        end
        object btnDelSpecSkill: TToolButton
          Left = 19
          Top = 0
          Caption = 'btnDelSpecSkill'
          ImageIndex = 1
          OnClick = btnDelSpecSkillClick
        end
      end
    end
    object tsMonster: TTabSheet
      Caption = 'Monster'
      ImageIndex = 8
      object Label47: TLabel
        Left = 4
        Top = 20
        Width = 39
        Height = 13
        Caption = 'Combat:'
      end
      object Label48: TLabel
        Left = 4
        Top = 48
        Width = 38
        Height = 13
        Caption = 'Tactics:'
      end
      object Label49: TLabel
        Left = 100
        Top = 76
        Width = 39
        Height = 13
        Caption = 'Attacks:'
      end
      object Label50: TLabel
        Left = 100
        Top = 20
        Width = 21
        Height = 13
        Caption = 'Hits:'
      end
      object Label51: TLabel
        Left = 100
        Top = 48
        Width = 35
        Height = 13
        Caption = 'Regen:'
      end
      object Label27: TLabel
        Left = 4
        Top = 76
        Width = 36
        Height = 13
        Caption = 'Stealth:'
      end
      object Label28: TLabel
        Left = 4
        Top = 104
        Width = 40
        Height = 13
        Caption = 'Observ.:'
      end
      object eMonAtk: TIntEdit
        Left = 52
        Top = 16
        Width = 37
        Height = 22
        MaxValue = 99
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = DataChange
      end
      object eMonTactics: TIntEdit
        Left = 52
        Top = 44
        Width = 37
        Height = 22
        MaxValue = 99
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = DataChange
      end
      object eMonNumAtk: TIntEdit
        Left = 144
        Top = 72
        Width = 45
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = DataChange
      end
      object eMonHits: TIntEdit
        Left = 144
        Top = 16
        Width = 45
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = DataChange
      end
      object eMonRegen: TIntEdit
        Left = 144
        Top = 44
        Width = 45
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 4
        Value = 0
        OnChange = DataChange
      end
      object GroupBox4: TGroupBox
        Left = 200
        Top = 0
        Width = 121
        Height = 159
        Caption = 'Defence'
        TabOrder = 5
        object Label52: TLabel
          Left = 24
          Top = 16
          Width = 29
          Height = 13
          Caption = 'Melee'
        end
        object Label53: TLabel
          Left = 24
          Top = 40
          Width = 33
          Height = 13
          Caption = 'Energy'
        end
        object Label54: TLabel
          Left = 24
          Top = 64
          Width = 23
          Height = 13
          Caption = 'Spirit'
        end
        object Label55: TLabel
          Left = 24
          Top = 88
          Width = 41
          Height = 13
          Caption = 'Weather'
        end
        object Label56: TLabel
          Left = 24
          Top = 136
          Width = 38
          Height = 13
          Caption = 'Ranged'
        end
        object Label57: TLabel
          Left = 24
          Top = 112
          Width = 30
          Height = 13
          Caption = 'Riding'
        end
        object Image1: TImage
          Left = 12
          Top = 18
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFF8000FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FF8000FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image2: TImage
          Left = 12
          Top = 42
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FF8000FF8000FF8000FF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image3: TImage
          Left = 12
          Top = 66
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFF8000FF8000FFFFFFFF8000FFFFFFFFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFF8000FF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FF8000FF8000FF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image4: TImage
          Left = 12
          Top = 90
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image5: TImage
          Left = 12
          Top = 114
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FF8000FFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFF8000FF8000FFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FF8000FFFFFFFF8000FFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image6: TImage
          Left = 12
          Top = 138
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFF8000FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF8000FF8000FF8000FFFFFFFFFFFF
            0000FFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFF8000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object eMonMelee: TIntEdit
          Left = 72
          Top = 12
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = DataChange
        end
        object eMonEnergy: TIntEdit
          Left = 72
          Top = 36
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = DataChange
        end
        object eMonSpirit: TIntEdit
          Left = 72
          Top = 60
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = DataChange
        end
        object eMonWeather: TIntEdit
          Left = 72
          Top = 84
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = DataChange
        end
        object eMonRiding: TIntEdit
          Left = 72
          Top = 108
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = DataChange
        end
        object eMonRanged: TIntEdit
          Left = 72
          Top = 132
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = DataChange
        end
      end
      object eMonObservation: TIntEdit
        Left = 52
        Top = 100
        Width = 37
        Height = 22
        MaxValue = 99
        MinValue = 0
        TabOrder = 6
        Value = 0
        OnChange = DataChange
      end
      object eMonStealth: TIntEdit
        Left = 52
        Top = 72
        Width = 37
        Height = 22
        MaxValue = 99
        MinValue = 0
        TabOrder = 7
        Value = 0
        OnChange = DataChange
      end
    end
    object tsWeapon: TTabSheet
      Caption = 'Weapon'
      ImageIndex = 2
      object Label13: TLabel
        Left = 4
        Top = 116
        Width = 22
        Height = 13
        Caption = 'Skill:'
      end
      object Label15: TLabel
        Left = 112
        Top = 36
        Width = 40
        Height = 13
        Caption = 'Attack +'
      end
      object Label16: TLabel
        Left = 112
        Top = 84
        Width = 50
        Height = 13
        Caption = 'Defence +'
      end
      object Label31: TLabel
        Left = 112
        Top = 60
        Width = 67
        Height = 13
        Caption = 'VS mounted +'
      end
      object Label32: TLabel
        Left = 140
        Top = 120
        Width = 39
        Height = 13
        Caption = 'Attacks:'
      end
      object Label14: TLabel
        Left = 220
        Top = 120
        Width = 5
        Height = 13
        Caption = '/'
      end
      object Label33: TLabel
        Left = 16
        Top = 140
        Width = 9
        Height = 13
        Caption = 'or'
      end
      object cmWpnSkill1: TComboBox
        Left = 32
        Top = 112
        Width = 97
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        Sorted = True
        TabOrder = 0
        OnChange = DataChange
        OnDrawItem = cmSkillDrawItem
      end
      object cmWpnSkill2: TComboBox
        Left = 32
        Top = 136
        Width = 97
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        Sorted = True
        TabOrder = 1
        OnChange = DataChange
        OnDrawItem = cmSkillDrawItem
      end
      object eAttBonus: TIntEdit
        Left = 188
        Top = 32
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = DataChange
      end
      object eDefBonus: TIntEdit
        Left = 188
        Top = 80
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = DataChange
      end
      object cbRanged: TCheckBox
        Left = 12
        Top = 8
        Width = 69
        Height = 17
        Caption = 'Ranged'
        TabOrder = 4
        OnClick = DataChange
      end
      object eMountBonus: TIntEdit
        Left = 188
        Top = 56
        Width = 41
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
        OnChange = DataChange
      end
      object cbLong: TCheckBox
        Left = 12
        Top = 24
        Width = 49
        Height = 17
        Caption = 'Long'
        TabOrder = 6
        OnClick = DataChange
      end
      object cbShort: TCheckBox
        Left = 12
        Top = 40
        Width = 53
        Height = 17
        Caption = 'Short'
        TabOrder = 7
        OnClick = DataChange
      end
      object cbNoFoot: TCheckBox
        Left = 12
        Top = 56
        Width = 61
        Height = 17
        Caption = 'No Foot'
        TabOrder = 8
        OnClick = DataChange
      end
      object cbNoMount: TCheckBox
        Left = 12
        Top = 72
        Width = 69
        Height = 17
        Caption = 'No Mount'
        TabOrder = 9
        OnClick = DataChange
      end
      object cbNoAttDefence: TCheckBox
        Left = 12
        Top = 88
        Width = 77
        Height = 17
        Caption = 'Defence 0'
        TabOrder = 10
        OnClick = DataChange
      end
      object cbAttRidingBonus: TCheckBox
        Left = 236
        Top = 34
        Width = 57
        Height = 17
        Caption = '+ riding'
        TabOrder = 11
        OnClick = DataChange
      end
      object cbDefRidingBonus: TCheckBox
        Left = 236
        Top = 82
        Width = 57
        Height = 17
        Caption = '+ riding'
        TabOrder = 12
        OnClick = DataChange
      end
      object eNumAttacks: TIntEdit
        Left = 184
        Top = 116
        Width = 33
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 13
        Value = 0
        OnChange = DataChange
      end
      object eNumRounds: TIntEdit
        Left = 228
        Top = 116
        Width = 33
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 14
        Value = 0
        OnChange = DataChange
      end
      object cmWpnClass: TComboBox
        Left = 112
        Top = 4
        Width = 101
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 15
        OnChange = DataChange
        OnDrawItem = cmWpnClassDrawItem
        Items.Strings = (
          'slashing'
          'piercing'
          'crushing'
          'cleaving'
          'armor piercing'
          'energy'
          'spirit'
          'weather')
      end
      object cbNumPlusSkill: TCheckBox
        Left = 268
        Top = 116
        Width = 49
        Height = 17
        Caption = '+ skill'
        TabOrder = 16
        OnClick = DataChange
      end
      object cbNumPlusHalf: TCheckBox
        Left = 268
        Top = 132
        Width = 73
        Height = 17
        Caption = '+ half'
        TabOrder = 17
        OnClick = DataChange
      end
      object cmWpnAttack: TComboBox
        Left = 220
        Top = 4
        Width = 97
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 18
        OnChange = DataChange
        OnDrawItem = cmWpnAttackDrawItem
        Items.Strings = (
          'melee'
          'energy'
          'spirit'
          'weather'
          'riding'
          'ranged')
      end
    end
    object tsArmor: TTabSheet
      Caption = 'Armor'
      ImageIndex = 3
      object cbUseInAss: TCheckBox
        Left = 12
        Top = 12
        Width = 121
        Height = 17
        Caption = 'Use in assassination'
        TabOrder = 0
        OnClick = DataChange
      end
      object GroupBox3: TGroupBox
        Left = 8
        Top = 36
        Width = 313
        Height = 117
        Caption = 'Defence'
        TabOrder = 1
        object Label19: TLabel
          Left = 28
          Top = 20
          Width = 38
          Height = 13
          Caption = 'slashing'
        end
        object Label17: TLabel
          Left = 28
          Top = 68
          Width = 40
          Height = 13
          Caption = 'crushing'
        end
        object Label18: TLabel
          Left = 28
          Top = 92
          Width = 40
          Height = 13
          Caption = 'cleaving'
        end
        object Label20: TLabel
          Left = 28
          Top = 44
          Width = 37
          Height = 13
          Caption = 'piercing'
        end
        object Label35: TLabel
          Left = 172
          Top = 20
          Width = 66
          Height = 13
          Caption = 'armor piercing'
        end
        object Label36: TLabel
          Left = 172
          Top = 44
          Width = 32
          Height = 13
          Caption = 'energy'
        end
        object Label37: TLabel
          Left = 172
          Top = 68
          Width = 21
          Height = 13
          Caption = 'spirit'
        end
        object Label38: TLabel
          Left = 172
          Top = 92
          Width = 38
          Height = 13
          Caption = 'weather'
        end
        object Label39: TLabel
          Left = 128
          Top = 20
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label40: TLabel
          Left = 128
          Top = 44
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label41: TLabel
          Left = 128
          Top = 68
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label42: TLabel
          Left = 128
          Top = 92
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label43: TLabel
          Left = 288
          Top = 20
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label44: TLabel
          Left = 288
          Top = 44
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label45: TLabel
          Left = 288
          Top = 68
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Label46: TLabel
          Left = 288
          Top = 92
          Width = 8
          Height = 13
          Caption = '%'
        end
        object Image7: TImage
          Left = 16
          Top = 22
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF808080808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image8: TImage
          Left = 16
          Top = 46
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image9: TImage
          Left = 160
          Top = 94
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080808080FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFF808080808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image10: TImage
          Left = 160
          Top = 70
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFF808080808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFF808080808080808080FFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080808080808080808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image11: TImage
          Left = 160
          Top = 46
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080808080808080808080808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image12: TImage
          Left = 160
          Top = 22
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080808080808080808080808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFF808080808080808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080808080808080808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image13: TImage
          Left = 16
          Top = 94
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF808080808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFF808080
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object Image14: TImage
          Left = 16
          Top = 70
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000C40E0000C40E0000000000000000
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080808080808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080808080FFFFFF808080FFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFF808080FFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF808080FFFFFFFFFFFFFFFFFF
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            0000}
          Transparent = True
        end
        object eArmPiercing: TIntEdit
          Left = 84
          Top = 40
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = DataChange
        end
        object eArmSlashing: TIntEdit
          Left = 84
          Top = 16
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = DataChange
        end
        object eArmCrushing: TIntEdit
          Left = 84
          Top = 64
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = DataChange
        end
        object eArmCleaving: TIntEdit
          Left = 84
          Top = 88
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = DataChange
        end
        object eArmAPiercing: TIntEdit
          Left = 244
          Top = 16
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = DataChange
        end
        object eArmEnergy: TIntEdit
          Left = 244
          Top = 40
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = DataChange
        end
        object eArmSpirit: TIntEdit
          Left = 244
          Top = 64
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 6
          Value = 0
          OnChange = DataChange
        end
        object eArmWeather: TIntEdit
          Left = 244
          Top = 88
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 7
          Value = 0
          OnChange = DataChange
        end
      end
    end
    object tsHorse: TTabSheet
      Caption = 'Mount'
      ImageIndex = 4
      object Label21: TLabel
        Left = 40
        Top = 28
        Width = 59
        Height = 13
        Caption = 'Combat skill:'
      end
      object Label22: TLabel
        Left = 40
        Top = 64
        Width = 52
        Height = 13
        Caption = 'Min bonus:'
      end
      object Label23: TLabel
        Left = 40
        Top = 92
        Width = 55
        Height = 13
        Caption = 'Max bonus:'
      end
      object Label29: TLabel
        Left = 40
        Top = 120
        Width = 84
        Height = 13
        Caption = 'Hampered bonus:'
      end
      object Label30: TLabel
        Left = 180
        Top = 120
        Width = 104
        Height = 13
        Caption = '(flying in no-flight land)'
      end
      object cmHorseSkill: TComboBox
        Left = 132
        Top = 24
        Width = 133
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        Sorted = True
        TabOrder = 0
        OnChange = DataChange
        OnDrawItem = cmSkillDrawItem
      end
      object eHorseMin: TIntEdit
        Left = 132
        Top = 60
        Width = 41
        Height = 22
        MaxValue = 5
        MinValue = 1
        TabOrder = 1
        Value = 1
        OnChange = DataChange
      end
      object eHorseMax: TIntEdit
        Left = 132
        Top = 88
        Width = 41
        Height = 22
        MaxValue = 5
        MinValue = 1
        TabOrder = 2
        Value = 1
        OnChange = DataChange
      end
      object eHampBonus: TIntEdit
        Left = 132
        Top = 116
        Width = 41
        Height = 22
        MaxValue = 5
        MinValue = 1
        TabOrder = 3
        Value = 1
        OnChange = DataChange
      end
    end
    object tsMovement: TTabSheet
      Caption = 'Movement'
      ImageIndex = 8
      object Label5: TLabel
        Left = 160
        Top = 16
        Width = 44
        Height = 13
        Caption = 'Capacity:'
      end
      object eWalk: TIntEdit
        Left = 160
        Top = 36
        Width = 60
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = DataChange
      end
      object eRide: TIntEdit
        Left = 160
        Top = 60
        Width = 60
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = DataChange
      end
      object eFly: TIntEdit
        Left = 160
        Top = 84
        Width = 60
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = DataChange
      end
      object eSwim: TIntEdit
        Left = 160
        Top = 108
        Width = 60
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 3
        Value = 0
        OnChange = DataChange
      end
      object cbWalk: TCheckBox
        Left = 84
        Top = 40
        Width = 45
        Height = 17
        Caption = 'Walk'
        TabOrder = 4
        OnClick = DataChange
      end
      object cbRide: TCheckBox
        Left = 84
        Top = 64
        Width = 45
        Height = 17
        Caption = 'Ride'
        TabOrder = 5
        OnClick = DataChange
      end
      object cbFly: TCheckBox
        Left = 84
        Top = 88
        Width = 37
        Height = 17
        Caption = 'Fly'
        TabOrder = 6
        OnClick = DataChange
      end
      object cbSwim: TCheckBox
        Left = 84
        Top = 112
        Width = 49
        Height = 17
        Caption = 'Swim'
        TabOrder = 7
        OnClick = DataChange
      end
    end
    object tsMagic: TTabSheet
      Caption = 'Combat'
      ImageIndex = 9
      object cbMageOnly: TCheckBox
        Left = 36
        Top = 24
        Width = 117
        Height = 17
        Caption = 'Used by mages only'
        TabOrder = 0
        OnClick = DataChange
      end
    end
    object tsWagon: TTabSheet
      Caption = 'Wagon'
      ImageIndex = 10
      object Label34: TLabel
        Left = 24
        Top = 24
        Width = 85
        Height = 13
        Caption = 'Walking capacity:'
      end
      object Label58: TLabel
        Left = 24
        Top = 52
        Width = 79
        Height = 13
        Caption = 'when hitched to:'
      end
      object cmWagonHitch: TComboBox
        Left = 116
        Top = 48
        Width = 113
        Height = 22
        Style = csOwnerDrawFixed
        ItemHeight = 16
        TabOrder = 0
        OnChange = DataChange
        OnDrawItem = cmIDataDrawItem
      end
      object eHitchWalk: TIntEdit
        Left = 116
        Top = 20
        Width = 61
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = DataChange
      end
    end
    object tsUpkeep: TTabSheet
      Caption = 'Upkeep'
      ImageIndex = 11
      object Label62: TLabel
        Left = 16
        Top = 16
        Width = 38
        Height = 13
        Caption = 'Upkeep'
      end
      object eUpkeepSilver: TIntEdit
        Left = 60
        Top = 12
        Width = 117
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = DataChange
      end
    end
    object tsFood: TTabSheet
      Caption = 'Food'
      ImageIndex = 12
      object Label63: TLabel
        Left = 16
        Top = 16
        Width = 27
        Height = 13
        Caption = 'Value'
      end
      object Label64: TLabel
        Left = 16
        Top = 40
        Width = 26
        Height = 13
        Caption = 'Order'
      end
      object eFoodValue: TIntEdit
        Left = 52
        Top = 12
        Width = 117
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 0
        Value = 0
        OnChange = DataChange
      end
      object eFoodOrder: TIntEdit
        Left = 52
        Top = 36
        Width = 117
        Height = 22
        MaxValue = 9999
        MinValue = 0
        TabOrder = 1
        Value = 0
        OnChange = DataChange
      end
    end
  end
end
