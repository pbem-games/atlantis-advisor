object BattleForm: TBattleForm
  Left = 680
  Top = 273
  Width = 718
  Height = 519
  BorderIcons = [biMaximize]
  Caption = 'Battles'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnActivate = FormActivate
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 0
    Top = 439
    Width = 702
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 0
    DesignSize = (
      702
      41)
    object btnClose: TButton
      Left = 619
      Top = 11
      Width = 75
      Height = 22
      Anchors = [akRight]
      Caption = 'Close'
      ModalResult = 1
      TabOrder = 0
    end
  end
  object pnMain: TPanel
    Left = 0
    Top = 0
    Width = 702
    Height = 439
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    Caption = 'pnMain'
    TabOrder = 1
    object Bevel1: TBevel
      Left = 324
      Top = 4
      Width = 8
      Height = 431
      Align = alLeft
      Shape = bsSpacer
    end
    object pnDetails: TPanel
      Left = 332
      Top = 4
      Width = 366
      Height = 431
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnDetails'
      TabOrder = 0
      object PControl: TPageControl
        Left = 0
        Top = 29
        Width = 366
        Height = 402
        ActivePage = tsReport
        Align = alClient
        Images = ResForm.BtnImages
        TabIndex = 0
        TabOrder = 0
        OnChange = PControlChange
        object tsReport: TTabSheet
          Caption = 'Report'
          ImageIndex = 4
          object Memo: TMemo
            Left = 0
            Top = 0
            Width = 358
            Height = 373
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ReadOnly = True
            ScrollBars = ssBoth
            TabOrder = 0
            WordWrap = False
          end
        end
        object tsUnit: TTabSheet
          Caption = 'Unit'
          object gUItems: TPowerGrid
            Left = 0
            Top = 73
            Width = 358
            Height = 159
            Align = alClient
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 1
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            ShowHint = True
            StickySelect = False
            TopRow = 0
            OnDrawIcon = ItemGridDrawIcon
            ColWidths = (
              48
              306)
          end
          object gUSkills: TPowerGrid
            Left = 0
            Top = 232
            Width = 358
            Height = 141
            Align = alBottom
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = SkillGridDrawCell
            ColWidths = (
              113
              241)
          end
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 358
            Height = 73
            Align = alTop
            Caption = 'Panel4'
            TabOrder = 2
            object Label2: TLabel
              Left = 4
              Top = 32
              Width = 38
              Height = 13
              Caption = 'Faction:'
            end
            object lFaction: TLabel
              Left = 60
              Top = 32
              Width = 145
              Height = 13
              AutoSize = False
              Caption = 'Forest Owls'
              ShowAccelChar = False
            end
            object imgFaction: TImage
              Left = 48
              Top = 32
              Width = 10
              Height = 10
              Picture.Data = {
                07544269746D617076010000424D760100000000000036000000280000000A00
                00000A000000010018000000000040010000CE0E0000C40E0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FF
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FF
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FF
                0000FF00FFFF00FF222222222222222222222222222222000000FF00FFFF00FF
                0000FF00FFFF00FF00000000FF0000FF0000FF0000FF00000000FF00FFFF00FF
                0000FF00FFFF00FF00000000FF0000FF0000FF0000FF00000000FF00FFFF00FF
                0000FF00FFFF00FF00000000FF0000FF0000FF0000FF00000000FF00FFFF00FF
                0000FF00FFFF00FF222222222222222222222222222222000000FF00FFFF00FF
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                0000}
              Transparent = True
            end
            object lNum: TLabel
              Left = 164
              Top = 4
              Width = 40
              Height = 13
              Alignment = taRightJustify
              AutoSize = False
              Caption = '6353'
            end
            object lCast: TLabel
              Left = 48
              Top = 48
              Width = 157
              Height = 13
              AutoSize = False
              Caption = 'shoots a Fireball'
            end
            object Label7: TLabel
              Left = 4
              Top = 48
              Width = 29
              Height = 13
              Caption = 'Casts:'
            end
            object lUName: TLabel
              Left = 4
              Top = 4
              Width = 161
              Height = 21
              AutoSize = False
              Caption = 'Family of Ogres'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'Black Chancery'
              Font.Style = []
              ParentFont = False
            end
          end
        end
        object tsSpoils: TTabSheet
          Caption = 'Spoils'
          ImageIndex = 63
          object gSpoils: TPowerGrid
            Left = 0
            Top = 0
            Width = 358
            Height = 373
            Align = alClient
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            Images = ResForm.IconList
            ImageCol = 1
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = True
            SortBy = 1
            StickySelect = False
            TopRow = 0
            OnDrawIcon = ItemGridDrawIcon
            ColWidths = (
              48
              306)
          end
        end
        object tsSimUnit: TTabSheet
          Caption = 'Unit'
          object pUnitControls: TPanel
            Left = 0
            Top = 0
            Width = 358
            Height = 345
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            DesignSize = (
              358
              345)
            object bvName: TBevel
              Left = 3
              Top = 3
              Width = 142
              Height = 23
            end
            object Label4: TLabel
              Left = 4
              Top = 36
              Width = 31
              Height = 13
              Caption = 'Inside:'
            end
            object eName: TEdit
              Left = 4
              Top = 4
              Width = 137
              Height = 21
              AutoSize = False
              BorderStyle = bsNone
              Color = clBtnFace
              Font.Charset = ANSI_CHARSET
              Font.Color = clWindowText
              Font.Height = -19
              Font.Name = 'Black Chancery'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              Text = 'Family of Ogres'
              OnExit = eNameExit
              OnKeyPress = eNameKeyPress
            end
            object cbBehind: TCheckBox
              Left = 152
              Top = 7
              Width = 57
              Height = 17
              Caption = 'Behind'
              TabOrder = 1
              OnClick = DataChange
            end
            object ItemGrid: TPowerGrid
              Left = 4
              Top = 60
              Width = 319
              Height = 121
              Anchors = [akLeft, akTop, akRight, akBottom]
              ColCount = 2
              DefaultRowColor = clBlack
              Editing = False
              FixedRows = 0
              Images = ResForm.IconList
              ImageCol = 1
              Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
              RowCount = 0
              Sorted = False
              SortBy = 0
              ShowHint = True
              StickySelect = False
              TopRow = 0
              OnDrawIcon = ItemGridDrawIcon
              OnSetEditText = ItemGridSetEditText
              ColWidths = (
                48
                267)
            end
            object cmItems: TComboBox
              Left = 4
              Top = 200
              Width = 161
              Height = 22
              Style = csOwnerDrawFixed
              DropDownCount = 12
              ItemHeight = 16
              TabOrder = 3
              OnDrawItem = cmItemsDrawItem
            end
            object ToolBar2: TToolBar
              Left = 168
              Top = 202
              Width = 45
              Height = 21
              Align = alNone
              ButtonHeight = 18
              ButtonWidth = 19
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              Images = ResForm.SmallBtnList
              TabOrder = 4
              object btnAddItem: TToolButton
                Left = 0
                Top = 0
                Caption = 'ToolButton1'
                ImageIndex = 0
                OnClick = btnAddItemClick
              end
              object btnDelItem: TToolButton
                Left = 19
                Top = 0
                Caption = 'ToolButton2'
                ImageIndex = 1
                OnClick = btnDelItemClick
              end
            end
            object SkillGrid: TPowerGrid
              Left = 4
              Top = 232
              Width = 173
              Height = 89
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
              OnSetEditText = SkillGridSetEditText
              OnSelectCell = SkillGridSelectCell
              ColWidths = (
                125
                44)
            end
            object cmSkills: TComboBox
              Left = 4
              Top = 324
              Width = 161
              Height = 22
              Style = csOwnerDrawFixed
              ItemHeight = 16
              TabOrder = 6
              OnDrawItem = cmSkillsDrawItem
            end
            object ToolBar1: TToolBar
              Left = 168
              Top = 326
              Width = 45
              Height = 21
              Align = alNone
              ButtonHeight = 18
              ButtonWidth = 19
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              Images = ResForm.SmallBtnList
              TabOrder = 7
              object btnAddSkill: TToolButton
                Left = 0
                Top = 0
                Caption = 'btnAddSkill'
                ImageIndex = 0
                OnClick = btnAddSkillClick
              end
              object btnDelSkill: TToolButton
                Left = 19
                Top = 0
                Caption = 'btnDelSkill'
                ImageIndex = 1
                OnClick = btnDelSkillClick
              end
            end
            object cmInside: TComboBox
              Left = 40
              Top = 32
              Width = 137
              Height = 22
              Style = csOwnerDrawFixed
              ItemHeight = 16
              TabOrder = 8
              OnChange = DataChange
              OnDrawItem = cmInsideDrawItem
            end
            object ToolBar5: TToolBar
              Left = 180
              Top = 232
              Width = 25
              Height = 49
              Align = alNone
              Caption = 'ToolBar5'
              EdgeBorders = []
              Images = ResForm.BtnImages
              TabOrder = 9
              object tbCombatSpell: TToolButton
                Left = 0
                Top = 2
                Hint = 'Combat spell'
                Caption = 'tbCombatSpell'
                Enabled = False
                ImageIndex = 6
                ParentShowHint = False
                ShowHint = True
                Style = tbsCheck
                OnClick = tbCombatSpellClick
              end
            end
            object ToolBar6: TToolBar
              Left = 180
              Top = 60
              Width = 25
              Height = 141
              Align = alNone
              Caption = 'ToolBar6'
              EdgeBorders = []
              Images = ResForm.BtnImages
              ParentShowHint = False
              ShowHint = True
              TabOrder = 10
              object ToolButton1: TToolButton
                Left = 0
                Top = 2
                Hint = 'Add Men'
                AllowAllUp = True
                Caption = 'ToolButton1'
                Grouped = True
                ImageIndex = 0
                Wrap = True
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
              object ToolButton2: TToolButton
                Tag = 1
                Left = 0
                Top = 24
                Hint = 'Add Monsters'
                AllowAllUp = True
                Caption = 'ToolButton2'
                Grouped = True
                ImageIndex = 50
                Wrap = True
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
              object ToolButton3: TToolButton
                Tag = 2
                Left = 0
                Top = 46
                Hint = 'Add Mounts'
                AllowAllUp = True
                Caption = 'ToolButton3'
                Grouped = True
                ImageIndex = 27
                Wrap = True
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
              object ToolButton6: TToolButton
                Tag = 4
                Left = 0
                Top = 68
                Hint = 'Add Armor'
                AllowAllUp = True
                Caption = 'ToolButton6'
                Grouped = True
                ImageIndex = 52
                Wrap = True
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
              object ToolButton5: TToolButton
                Tag = 3
                Left = 0
                Top = 90
                Hint = 'Add Weapons'
                AllowAllUp = True
                Caption = 'ToolButton5'
                Grouped = True
                ImageIndex = 51
                Wrap = True
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
              object ToolButton7: TToolButton
                Tag = 5
                Left = 0
                Top = 112
                Hint = 'Add Magic/Healing items'
                AllowAllUp = True
                Caption = 'ToolButton7'
                Grouped = True
                ImageIndex = 4
                Style = tbsCheck
                OnClick = ItemFilterClick
              end
            end
            object ToolBar4: TToolBar
              Left = 180
              Top = 30
              Width = 37
              Height = 29
              Align = alNone
              Caption = 'ToolBar4'
              EdgeBorders = []
              Images = ResForm.BtnImages
              TabOrder = 11
              object btnSoldiers: TToolButton
                Left = 0
                Top = 2
                Hint = 'View Soldiers in Unit'
                Caption = 'btnSoldiers'
                ImageIndex = 45
                ParentShowHint = False
                ShowHint = True
                OnClick = btnSoldiersClick
              end
            end
          end
          object pUnitButtons: TPanel
            Left = 0
            Top = 345
            Width = 358
            Height = 28
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object btnAddAttacker: TBitBtn
              Left = 4
              Top = 4
              Width = 73
              Height = 21
              Caption = 'Attacker'
              TabOrder = 0
              OnClick = btnAddUnitClick
              Glyph.Data = {
                E6010000424DE60100000000000036000000280000000C0000000C0000000100
                180000000000B0010000C40E0000C40E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000
                000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000
                00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF}
              Margin = 2
            end
            object btnAddDefender: TBitBtn
              Tag = 1
              Left = 80
              Top = 4
              Width = 73
              Height = 21
              Caption = 'Defender'
              TabOrder = 1
              OnClick = btnAddUnitClick
              Glyph.Data = {
                E6010000424DE60100000000000036000000280000000C0000000C0000000100
                180000000000B0010000C40E0000C40E00000000000000000000FFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000000000000000000000
                000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFF0000000000000000
                00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFF000000000000FFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
                FFFFFFFFFFFFFFFFFFFF}
              Margin = 2
            end
            object btnDelUnit: TButton
              Left = 164
              Top = 4
              Width = 43
              Height = 21
              Caption = 'Delete'
              TabOrder = 2
              OnClick = btnDelUnitClick
            end
          end
        end
        object tsSimRegion: TTabSheet
          Caption = 'Region'
          ImageIndex = 17
          object Label1: TLabel
            Left = 8
            Top = 28
            Width = 36
            Height = 13
            Caption = 'Terrain:'
          end
          object lMounts: TLabel
            Left = 60
            Top = 52
            Width = 136
            Height = 13
            Caption = 'riding and flying mounts used'
          end
          object Label3: TLabel
            Left = 12
            Top = 84
            Width = 39
            Height = 13
            Caption = 'Objects:'
          end
          object cmTerrain: TComboBox
            Left = 52
            Top = 24
            Width = 153
            Height = 22
            Style = csOwnerDrawFixed
            ItemHeight = 16
            TabOrder = 0
            OnChange = cmTerrainChange
            OnDrawItem = cmTerrainDrawItem
          end
          object cmStructs: TComboBox
            Left = 4
            Top = 352
            Width = 157
            Height = 22
            Style = csOwnerDrawFixed
            DropDownCount = 12
            ItemHeight = 16
            Sorted = True
            TabOrder = 1
            OnDrawItem = cmStructsDrawItem
          end
          object barStructControl: TToolBar
            Left = 164
            Top = 354
            Width = 45
            Height = 21
            Align = alNone
            ButtonHeight = 18
            ButtonWidth = 19
            Caption = 'barStructControl'
            EdgeBorders = []
            Flat = True
            Images = ResForm.SmallBtnList
            TabOrder = 2
            object tbAddStruct: TToolButton
              Left = 0
              Top = 0
              Caption = 'tbAddStruct'
              ImageIndex = 0
              OnClick = tbAddStructClick
            end
            object tbDelStruct: TToolButton
              Left = 19
              Top = 0
              Caption = 'tbDelStruct'
              ImageIndex = 1
              OnClick = tbDelStructClick
            end
          end
          object gStructs: TPowerGrid
            Left = 4
            Top = 104
            Width = 201
            Height = 241
            ColCount = 3
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 1
            ImageCol = 0
            Options = [pgoLines, pgoRowSelect, pgoStretchLastCol]
            RowCount = 1
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 1
            OnDrawCell = gStructsDrawCell
            ColWidths = (
              27
              119
              51)
          end
        end
      end
      object Panel5: TPanel
        Left = 0
        Top = 0
        Width = 366
        Height = 29
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          366
          29)
        object cmBattles: TComboBox
          Left = 1
          Top = 0
          Width = 365
          Height = 21
          Style = csDropDownList
          Anchors = [akLeft, akTop, akRight]
          ItemHeight = 13
          TabOrder = 0
          OnChange = cmBattlesChange
        end
      end
    end
    object pnOverview: TPanel
      Left = 4
      Top = 4
      Width = 320
      Height = 431
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Bevel2: TBevel
        Left = 0
        Top = 85
        Width = 320
        Height = 8
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 0
        Top = 52
        Width = 320
        Height = 8
        Align = alTop
        Shape = bsSpacer
      end
      object pnSides: TPanel
        Left = 0
        Top = 0
        Width = 320
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object pnAttacker: TPanel
          Left = 0
          Top = 0
          Width = 156
          Height = 52
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 0
          object lAtkWin: TLabel
            Left = 60
            Top = 39
            Width = 85
            Height = 13
            AutoSize = False
            Caption = '95% victory'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lAtkLeaderName: TLabel
            Left = 60
            Top = 0
            Width = 81
            Height = 13
            AutoSize = False
            Caption = 'Goblin General'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lAtkTactics: TLabel
            Left = 60
            Top = 16
            Width = 81
            Height = 13
            AutoSize = False
            Caption = 'tactics 5'
          end
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 52
            Height = 52
            BevelOuter = bvLowered
            TabOrder = 0
            object imgAtkLeader: TImage
              Left = 1
              Top = 1
              Width = 50
              Height = 50
            end
          end
        end
        object pnDefender: TPanel
          Left = 164
          Top = 0
          Width = 156
          Height = 52
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object lDefWin: TLabel
            Left = 16
            Top = 39
            Width = 80
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = '5% victory'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object lDefLeaderName: TLabel
            Left = 16
            Top = 0
            Width = 80
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'St. Benedict'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object lDefTactics: TLabel
            Left = 16
            Top = 16
            Width = 80
            Height = 13
            Alignment = taRightJustify
            AutoSize = False
            Caption = 'tactics 4'
          end
          object Panel2: TPanel
            Left = 104
            Top = 0
            Width = 52
            Height = 52
            BevelOuter = bvLowered
            TabOrder = 0
            object imgDefLeader: TImage
              Left = 1
              Top = 1
              Width = 50
              Height = 50
            end
          end
        end
      end
      object pnToolbar: TPanel
        Left = 0
        Top = 60
        Width = 320
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object ToolBar: TToolBar
          Left = 0
          Top = 0
          Width = 105
          Height = 25
          Align = alLeft
          EdgeBorders = []
          Images = ResForm.BtnImages
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          object btnSimulation: TToolButton
            Left = 0
            Top = 2
            Hint = 'Simulation'
            ImageIndex = 19
            Style = tbsCheck
            OnClick = btnSimulationClick
          end
          object btnStartSim: TToolButton
            Left = 23
            Top = 2
            Hint = 'Calculate damage'
            Caption = 'Start Simulation'
            DropdownMenu = SimPopup
            ImageIndex = 28
            Style = tbsDropDown
            OnClick = btnStartSimClick
          end
          object ToolButton8: TToolButton
            Left = 61
            Top = 2
            Width = 8
            Caption = 'ToolButton8'
            ImageIndex = 69
            Style = tbsSeparator
          end
          object btnPlay: TToolButton
            Left = 69
            Top = 2
            Hint = 'Play battle'
            DropdownMenu = PlayPopup
            ImageIndex = 67
            ParentShowHint = False
            ShowHint = True
            Style = tbsDropDown
            OnClick = btnPlayClick
          end
        end
        object pRound: TPanel
          Left = 263
          Top = 0
          Width = 57
          Height = 25
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object lRound: TLabel
            Left = 7
            Top = 5
            Width = 48
            Height = 13
            Alignment = taRightJustify
            Caption = 'free round'
          end
        end
        object trRounds: TTrackBar
          Left = 105
          Top = 0
          Width = 158
          Height = 25
          Align = alClient
          Orientation = trHorizontal
          PageSize = 1
          Frequency = 1
          Position = 0
          SelEnd = 0
          SelStart = 0
          TabOrder = 2
          ThumbLength = 15
          TickMarks = tmBottomRight
          TickStyle = tsAuto
          OnChange = trRoundsChange
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 93
        Width = 320
        Height = 338
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        OnResize = Panel3Resize
        object Painter: TPainter
          Left = 0
          Top = 0
          Width = 307
          Height = 338
          Align = alClient
          CanvasWidth = 0
          CanvasHeight = 0
          RightBtnScroll = False
          TabStop = True
          OnKeyDown = PainterKeyDown
          OnMouseDown = PainterMouseDown
          OnMouseMove = PainterMouseMove
          OnMouseOver = PainterMouseOver
          OnMouseOut = PainterMouseOut
        end
        object ScrollBar: TScrollBar
          Left = 307
          Top = 0
          Width = 13
          Height = 338
          Align = alRight
          Kind = sbVertical
          LargeChange = 50
          Min = 1
          PageSize = 50
          Position = 1
          SmallChange = 32
          TabOrder = 1
          TabStop = False
          OnChange = ScrollBarChange
        end
      end
    end
  end
  object PlayPopup: TPopupMenu
    Left = 244
    Top = 104
    object Slow1: TMenuItem
      Tag = 500
      Caption = 'Slow'
      OnClick = btnPlayClick
    end
    object Fast1: TMenuItem
      Tag = 100
      Caption = 'Normal'
      Default = True
      OnClick = btnPlayClick
    end
    object Normal1: TMenuItem
      Tag = 25
      Caption = 'Fast'
      OnClick = btnPlayClick
    end
  end
  object SimPopup: TPopupMenu
    Left = 212
    Top = 104
    object N10battles1: TMenuItem
      Tag = 10
      Caption = '10 battles'
      OnClick = btnStartSimClick
    end
    object N50battles1: TMenuItem
      Tag = 50
      Caption = '50 battles'
      OnClick = btnStartSimClick
    end
    object N100battles1: TMenuItem
      Tag = 100
      Caption = '100 battles'
      Default = True
      OnClick = btnStartSimClick
    end
  end
end
