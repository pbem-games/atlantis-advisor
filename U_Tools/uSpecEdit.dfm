object SpecEditForm: TSpecEditForm
  Left = 202
  Top = 36
  BorderStyle = bsDialog
  Caption = 'Ability Editor'
  ClientHeight = 499
  ClientWidth = 517
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
    517
    499)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 518
    Height = 485
    ActivePage = TabSheet1
    Anchors = [akLeft, akTop, akRight, akBottom]
    Images = ResForm.BtnImages
    TabIndex = 0
    TabOrder = 1
    TabPosition = tpBottom
    object TabSheet1: TTabSheet
      Caption = 'Combat Abilities'
      ImageIndex = 47
      object Label9: TLabel
        Left = 8
        Top = 400
        Width = 126
        Height = 13
        Caption = 'Appears in battle report as:'
      end
      object Grid: TPowerGrid
        Left = 4
        Top = 8
        Width = 141
        Height = 381
        ColCount = 1
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = True
        TopRow = 1
        OnDrawCell = GridDrawCell
        OnSelectCell = GridSelectCell
        ColWidths = (
          137)
      end
      object cbNoBuilding: TCheckBox
        Left = 160
        Top = 8
        Width = 113
        Height = 17
        Caption = 'No building bonus'
        TabOrder = 1
        OnClick = SpecDataChange
      end
      object GroupBox1: TGroupBox
        Left = 344
        Top = 4
        Width = 157
        Height = 137
        Caption = 'Affects items:'
        TabOrder = 2
        object cbItmExcept: TCheckBox
          Left = 12
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Except'
          TabOrder = 0
          OnClick = SpecDataChange
        end
        object lbItems: TListBox
          Left = 8
          Top = 36
          Width = 141
          Height = 68
          Style = lbOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 1
          OnDrawItem = lbItemsDrawItem
        end
        object cmItems: TComboBox
          Left = 8
          Top = 107
          Width = 101
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          Sorted = True
          TabOrder = 2
          OnDrawItem = cmItemsDrawItem
        end
        object ToolBar1: TToolBar
          Left = 110
          Top = 109
          Width = 43
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 3
          object btnAddItem: TToolButton
            Left = 0
            Top = 0
            Caption = 'btnAddItem'
            ImageIndex = 0
            OnClick = btnAddItemClick
          end
          object btnDelItem: TToolButton
            Left = 19
            Top = 0
            Caption = 'btnDelItem'
            ImageIndex = 1
            OnClick = btnDelItemClick
          end
        end
      end
      object cbNoMonsters: TCheckBox
        Left = 160
        Top = 28
        Width = 121
        Height = 17
        Caption = 'Not affects monsters'
        TabOrder = 3
        OnClick = SpecDataChange
      end
      object cbIllusions: TCheckBox
        Left = 160
        Top = 48
        Width = 129
        Height = 17
        Caption = 'Affects only illusions'
        TabOrder = 4
        OnClick = SpecDataChange
      end
      object GroupBox2: TGroupBox
        Left = 344
        Top = 144
        Width = 157
        Height = 121
        Caption = 'Affects units inside buildings:'
        TabOrder = 5
        object cbBuildingExcept: TCheckBox
          Left = 12
          Top = 16
          Width = 57
          Height = 17
          Caption = 'Except'
          TabOrder = 0
          OnClick = SpecDataChange
        end
        object lbBuildings: TListBox
          Left = 8
          Top = 36
          Width = 141
          Height = 52
          Style = lbOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 1
          OnDrawItem = lbBuildingsDrawItem
        end
        object cmBuildings: TComboBox
          Left = 8
          Top = 91
          Width = 101
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          Sorted = True
          TabOrder = 2
          OnDrawItem = cmBuildingsDrawItem
        end
        object ToolBar2: TToolBar
          Left = 110
          Top = 93
          Width = 43
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 3
          object btnAddBuilding: TToolButton
            Left = 0
            Top = 0
            Caption = 'ToolButton1'
            ImageIndex = 0
            OnClick = btnAddBuildingClick
          end
          object btnDelBuilding: TToolButton
            Left = 19
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
            OnClick = btnDelBuildingClick
          end
        end
      end
      object GroupBox3: TGroupBox
        Left = 344
        Top = 268
        Width = 157
        Height = 121
        Caption = 'Affects units with effects:'
        TabOrder = 6
        object cbEffExcept: TCheckBox
          Left = 12
          Top = 16
          Width = 61
          Height = 17
          Caption = 'Except'
          TabOrder = 0
          OnClick = SpecDataChange
        end
        object lbEffects: TListBox
          Left = 8
          Top = 36
          Width = 141
          Height = 52
          Style = lbOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 1
          OnDrawItem = lbEffectsDrawItem
        end
        object cmEffects: TComboBox
          Left = 8
          Top = 91
          Width = 101
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          Sorted = True
          TabOrder = 2
          OnDrawItem = cmEffectsDrawItem
        end
        object ToolBar3: TToolBar
          Left = 110
          Top = 93
          Width = 43
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 3
          object btnAddEffect: TToolButton
            Left = 0
            Top = 0
            Caption = 'ToolButton1'
            ImageIndex = 0
            OnClick = btnAddEffectClick
          end
          object btnDelEffect: TToolButton
            Left = 19
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
            OnClick = btnDelEffectClick
          end
        end
      end
      object GroupBox4: TGroupBox
        Left = 156
        Top = 96
        Width = 181
        Height = 141
        Caption = 'Attacks'
        TabOrder = 7
        object Label1: TLabel
          Left = 8
          Top = 116
          Width = 31
          Height = 13
          Caption = 'Effect:'
        end
        object lbAttacks: TListBox
          Left = 8
          Top = 16
          Width = 165
          Height = 46
          ItemHeight = 13
          TabOrder = 0
        end
        object eAtkMin: TIntEdit
          Left = 8
          Top = 64
          Width = 37
          Height = 22
          MaxValue = 9999
          MinValue = 0
          TabOrder = 1
          Value = 0
        end
        object eAtkMax: TIntEdit
          Left = 48
          Top = 64
          Width = 45
          Height = 22
          MaxValue = 9999
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object cmAtkEffect: TComboBox
          Left = 48
          Top = 112
          Width = 85
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          Sorted = True
          TabOrder = 3
          OnDrawItem = cmEffectsDrawItem
        end
        object cmAttack: TComboBox
          Left = 96
          Top = 64
          Width = 77
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 4
          OnDrawItem = cmAttackDrawItem
        end
        object cbAtkAlwaysReady: TCheckBox
          Left = 104
          Top = 88
          Width = 61
          Height = 17
          Caption = '100% hit'
          TabOrder = 5
        end
        object cmAtkWeapon: TComboBox
          Left = 8
          Top = 88
          Width = 93
          Height = 22
          Style = csOwnerDrawFixed
          ItemHeight = 16
          TabOrder = 6
          OnDrawItem = cmAtkWeaponDrawItem
        end
        object ToolBar6: TToolBar
          Left = 138
          Top = 115
          Width = 39
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 7
          object btnAddAttack: TToolButton
            Left = 0
            Top = 0
            Caption = 'ToolButton1'
            ImageIndex = 0
            OnClick = btnAddAttackClick
          end
          object btnDelAttack: TToolButton
            Left = 19
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
            OnClick = btnDelAttackClick
          end
        end
      end
      object GroupBox5: TGroupBox
        Left = 156
        Top = 240
        Width = 181
        Height = 73
        Caption = 'Defence Bonuses'
        TabOrder = 8
        object lbDefences: TListBox
          Left = 8
          Top = 16
          Width = 85
          Height = 46
          ItemHeight = 13
          TabOrder = 0
        end
        object cmDefence: TComboBox
          Left = 100
          Top = 16
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object eDefLevel: TIntEdit
          Left = 100
          Top = 40
          Width = 37
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
        end
        object ToolBar5: TToolBar
          Left = 138
          Top = 42
          Width = 39
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 3
          object btnAddDefence: TToolButton
            Left = 0
            Top = 0
            Caption = 'ToolButton1'
            ImageIndex = 0
            OnClick = btnAddDefenceClick
          end
          object btnDelDefence: TToolButton
            Left = 19
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
            OnClick = btnDelDefenceClick
          end
        end
      end
      object GroupBox6: TGroupBox
        Left = 156
        Top = 316
        Width = 181
        Height = 73
        Caption = 'Shields'
        TabOrder = 9
        object lbShields: TListBox
          Left = 8
          Top = 16
          Width = 85
          Height = 46
          ItemHeight = 13
          TabOrder = 0
        end
        object cmShield: TComboBox
          Left = 100
          Top = 16
          Width = 73
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
        end
        object ToolBar4: TToolBar
          Left = 138
          Top = 42
          Width = 39
          Height = 22
          Align = alNone
          ButtonHeight = 18
          ButtonWidth = 19
          Caption = 'ToolBar1'
          EdgeBorders = []
          Flat = True
          Images = ResForm.SmallBtnList
          TabOrder = 2
          object btnAddShield: TToolButton
            Left = 0
            Top = 0
            Caption = 'ToolButton1'
            ImageIndex = 0
            OnClick = btnAddShieldClick
          end
          object btnDelShield: TToolButton
            Left = 19
            Top = 0
            Caption = 'ToolButton2'
            ImageIndex = 1
            OnClick = btnDelShieldClick
          end
        end
        object cbAllShields: TCheckBox
          Left = 100
          Top = 44
          Width = 37
          Height = 17
          Caption = 'All'
          TabOrder = 3
          OnClick = SpecDataChange
        end
      end
      object mDescription: TRichEdit
        Left = 156
        Top = 392
        Width = 345
        Height = 57
        ScrollBars = ssVertical
        TabOrder = 10
      end
      object eSpellText: TEdit
        Left = 4
        Top = 416
        Width = 141
        Height = 21
        TabOrder = 11
        OnExit = SpecDataChange
      end
      object cbUseLev: TCheckBox
        Left = 160
        Top = 68
        Width = 121
        Height = 17
        Caption = 'Use mage skill level'
        TabOrder = 12
        OnClick = SpecDataChange
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Effects'
      ImageIndex = 48
      object Label2: TLabel
        Left = 180
        Top = 64
        Width = 34
        Height = 13
        Caption = 'Attack:'
      end
      object EffGrid: TPowerGrid
        Left = 4
        Top = 8
        Width = 141
        Height = 441
        ColCount = 1
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = True
        TopRow = 1
        OnDrawCell = GridDrawCell
        OnSelectCell = EffGridSelectCell
        ColWidths = (
          137)
      end
      object cbEffOneShot: TCheckBox
        Left = 172
        Top = 28
        Width = 137
        Height = 17
        Caption = 'Affects only one attack'
        TabOrder = 1
        OnClick = EffDataChange
      end
      object eEffAttack: TIntEdit
        Left = 240
        Top = 60
        Width = 53
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 2
        Value = 0
        OnChange = EffDataChange
      end
      object GroupBox7: TGroupBox
        Left = 160
        Top = 100
        Width = 157
        Height = 229
        Caption = 'Defence'
        TabOrder = 3
        object Label3: TLabel
          Left = 20
          Top = 36
          Width = 32
          Height = 13
          Caption = 'Melee:'
        end
        object Label5: TLabel
          Left = 20
          Top = 64
          Width = 36
          Height = 13
          Caption = 'Energy:'
        end
        object Label6: TLabel
          Left = 20
          Top = 92
          Width = 26
          Height = 13
          Caption = 'Spirit:'
        end
        object Label7: TLabel
          Left = 20
          Top = 120
          Width = 44
          Height = 13
          Caption = 'Weather:'
        end
        object Label8: TLabel
          Left = 20
          Top = 148
          Width = 33
          Height = 13
          Caption = 'Riding:'
        end
        object Label4: TLabel
          Left = 20
          Top = 176
          Width = 41
          Height = 13
          Caption = 'Ranged:'
        end
        object eEffDef0: TIntEdit
          Left = 80
          Top = 32
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 0
          Value = 0
          OnChange = EffDataChange
        end
        object eEffDef1: TIntEdit
          Left = 80
          Top = 60
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 1
          Value = 0
          OnChange = EffDataChange
        end
        object eEffDef2: TIntEdit
          Left = 80
          Top = 88
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 2
          Value = 0
          OnChange = EffDataChange
        end
        object eEffDef3: TIntEdit
          Left = 80
          Top = 116
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 3
          Value = 0
          OnChange = EffDataChange
        end
        object eEffDef4: TIntEdit
          Left = 80
          Top = 144
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 4
          Value = 0
          OnChange = EffDataChange
        end
        object eEffDef5: TIntEdit
          Left = 80
          Top = 171
          Width = 53
          Height = 22
          MaxValue = 0
          MinValue = 0
          TabOrder = 5
          Value = 0
          OnChange = EffDataChange
        end
      end
    end
  end
  object Button1: TButton
    Left = 435
    Top = 471
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
