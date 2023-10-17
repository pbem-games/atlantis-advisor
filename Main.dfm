object MainForm: TMainForm
  Left = 432
  Top = 136
  Width = 1932
  Height = 1093
  Caption = 'Atlantis Advisor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Visible = True
  WindowState = wsMaximized
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 1350
    Top = 28
    Width = 6
    Height = 1006
    Cursor = crHSplit
    Align = alRight
    Beveled = True
  end
  object MapUnitsPanel: TPanel
    Left = 0
    Top = 28
    Width = 1350
    Height = 1006
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object UnitSplitter: TSplitter
      Left = 0
      Top = 886
      Width = 1350
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
    end
    object MapToolPanel: TPanel
      Left = 0
      Top = 0
      Width = 1350
      Height = 29
      HelpContext = 3
      Align = alTop
      TabOrder = 0
      object MapToolbar: TToolBar
        Left = 1
        Top = 1
        Width = 1301
        Height = 27
        Align = alClient
        ButtonHeight = 23
        Caption = 'MapToolbar'
        EdgeBorders = []
        Images = ResForm.BtnImages
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        object ToolButton4: TToolButton
          Left = 0
          Top = 2
          Width = 8
          Caption = 'ToolButton4'
          Style = tbsSeparator
        end
        object ListBtn: TToolButton
          Left = 8
          Top = 2
          Hint = 'Region List'
          AllowAllUp = True
          Caption = 'Region List'
          Grouped = True
          ImageIndex = 9
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = ListBtnClick
        end
        object ListDownBtn: TSpeedButton
          Left = 31
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton31: TToolButton
          Left = 43
          Top = 2
          Width = 8
          Caption = 'ToolButton31'
          ImageIndex = 32
          Style = tbsSeparator
        end
        object ToolButton30: TToolButton
          Left = 51
          Top = 2
          Hint = 'Define List filter'
          Action = ListFilterAction
          ParentShowHint = False
          ShowHint = True
        end
        object ToolButton5: TToolButton
          Left = 74
          Top = 2
          Width = 11
          Caption = 'ToolButton5'
          ImageIndex = 20
          Style = tbsSeparator
        end
        object FogBtn: TToolButton
          Left = 85
          Top = 2
          Hint = 'Fog Type'
          AllowAllUp = True
          Caption = 'Fog Type'
          ImageIndex = 10
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = FogBtnClick
        end
        object FogDownBtn: TSpeedButton
          Tag = 1
          Left = 108
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton6: TToolButton
          Left = 120
          Top = 2
          Width = 8
          Caption = 'ToolButton6'
          ImageIndex = 21
          Style = tbsSeparator
        end
        object FlagBtn: TToolButton
          Left = 128
          Top = 2
          Hint = 'Factions in Region'
          AllowAllUp = True
          Caption = 'Factions in Region'
          ImageIndex = 12
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = FlagBtnClick
        end
        object FlagDownBtn: TSpeedButton
          Tag = 2
          Left = 151
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton11: TToolButton
          Left = 163
          Top = 2
          Width = 8
          Caption = 'ToolButton11'
          ImageIndex = 25
          Style = tbsSeparator
        end
        object MIcBtn: TToolButton
          Left = 171
          Top = 2
          Hint = 'Event Icons'
          Caption = 'Event Icons'
          ImageIndex = 13
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = MIcBtnClick
        end
        object MIcDownBtn: TSpeedButton
          Tag = 3
          Left = 194
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton13: TToolButton
          Left = 206
          Top = 2
          Width = 8
          Caption = 'ToolButton13'
          ImageIndex = 26
          Style = tbsSeparator
        end
        object SIcBtn: TToolButton
          Left = 214
          Top = 2
          Hint = 'Status Icon'
          Caption = 'Status Icon'
          ImageIndex = 14
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = SIcBtnClick
        end
        object SIcDownBtn: TSpeedButton
          Tag = 4
          Left = 237
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton15: TToolButton
          Left = 249
          Top = 2
          Width = 8
          Caption = 'ToolButton15'
          ImageIndex = 27
          Style = tbsSeparator
        end
        object STxBtn: TToolButton
          Left = 257
          Top = 2
          Hint = 'Status Text'
          Caption = 'Status Text'
          ImageIndex = 15
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = STxBtnClick
        end
        object STxDownBtn: TSpeedButton
          Tag = 5
          Left = 280
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton17: TToolButton
          Left = 292
          Top = 2
          Width = 8
          Caption = 'ToolButton17'
          ImageIndex = 28
          Style = tbsSeparator
        end
        object StructBtn: TToolButton
          Left = 300
          Top = 2
          Hint = 'Objects'
          Caption = 'Objects'
          ImageIndex = 1
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = StructBtnClick
        end
        object StructDownBtn: TSpeedButton
          Tag = 8
          Left = 323
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton19: TToolButton
          Left = 335
          Top = 2
          Width = 8
          Caption = 'ToolButton19'
          ImageIndex = 9
          Style = tbsSeparator
        end
        object LevelBtn: TToolButton
          Left = 343
          Top = 2
          Hint = 'Level'
          Action = LevelAction
          AllowAllUp = True
          ParentShowHint = False
          ShowHint = True
        end
        object LevelDownBtn: TSpeedButton
          Tag = 6
          Left = 366
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
        object ToolButton7: TToolButton
          Left = 378
          Top = 2
          Width = 8
          Caption = 'ToolButton7'
          ImageIndex = 17
          Style = tbsSeparator
        end
        object BookmarkBtn: TToolButton
          Left = 386
          Top = 2
          Hint = 'Bookmark'
          Action = BookmarkAction
          ImageIndex = 34
          ParentShowHint = False
          ShowHint = True
        end
        object BmkDownBtn: TSpeedButton
          Tag = 7
          Left = 409
          Top = 2
          Width = 12
          Height = 23
          Glyph.Data = {
            86000000424D8600000000000000360000002800000005000000050000000100
            18000000000050000000CE0E0000C40E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00FFFFFFFFFFFF000000FFFFFFFFFFFF00FFFFFF000000
            000000000000FFFFFF0000000000000000000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFF00}
          Margin = 1
          OnClick = DownBtnClick
        end
      end
      object CoordLabel: TPanel
        Left = 1302
        Top = 1
        Width = 47
        Height = 27
        Align = alRight
        Alignment = taLeftJustify
        BevelOuter = bvNone
        Caption = '256, 256'
        TabOrder = 1
      end
    end
    object UnitsPanel: TPanel
      Left = 0
      Top = 892
      Width = 1350
      Height = 114
      HelpContext = 5
      Align = alBottom
      BevelOuter = bvNone
      Constraints.MinHeight = 30
      TabOrder = 1
      object UnitGrid: TPowerGrid
        Left = 0
        Top = 30
        Width = 1350
        Height = 84
        Align = alClient
        Color = clBlack
        ColCount = 16
        DefaultRowColor = clWhite
        Editing = False
        FixedRows = 1
        ImageCol = 0
        PopupMenu = PopMenu
        RowCount = 1
        Sorted = True
        SortBy = 0
        StickySelect = True
        TopRow = 1
        OnDblClick = UnitGridDblClick
        OnDrawCell = UnitGridDrawCell
        OnDragOver = UnitGridDragOver
        OnMouseDown = UnitGridMouseDown
        OnSelectCell = UnitGridSelectCell
        ColWidths = (
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64
          64)
      end
      object tbUnitTools: TToolBar
        Left = 0
        Top = 0
        Width = 1350
        Height = 30
        BorderWidth = 1
        Caption = 'tbUnitTools'
        EdgeBorders = []
        EdgeInner = esNone
        EdgeOuter = esNone
        Images = ResForm.BtnImages
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        object ToolButton38: TToolButton
          Left = 0
          Top = 2
          Width = 8
          Caption = 'ToolButton38'
          Style = tbsSeparator
        end
        object cmFilterFaction: TComboBox
          Left = 8
          Top = 2
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
          OnChange = cmFilterFactionChange
        end
        object ToolButton39: TToolButton
          Left = 153
          Top = 2
          Width = 8
          Caption = 'ToolButton39'
          ImageIndex = 0
          Style = tbsSeparator
        end
        object btnMyFaction: TToolButton
          Left = 161
          Top = 2
          Hint = 'Player'#39's faction'
          Caption = 'btnMyFaction'
          ImageIndex = 11
          OnClick = btnMyFactionClick
        end
        object btnAllFactions: TToolButton
          Left = 184
          Top = 2
          Hint = 'All factions'
          Caption = 'btnAllFactions'
          ImageIndex = 12
          OnClick = btnAllFactionsClick
        end
        object ToolButton51: TToolButton
          Left = 207
          Top = 2
          Width = 22
          Caption = 'ToolButton51'
          ImageIndex = 2
          Style = tbsSeparator
        end
        object btnFactionMode: TToolButton
          Left = 229
          Top = 2
          Hint = 'Faction Mode'
          Action = GridModeAction
        end
        object btnUnitFilter: TToolButton
          Left = 252
          Top = 2
          Action = UnitFilterAction
        end
        object ToolButton44: TToolButton
          Left = 275
          Top = 2
          Action = FindUnitAction
        end
        object ToolButton49: TToolButton
          Left = 298
          Top = 2
          Width = 8
          Caption = 'ToolButton49'
          ImageIndex = 1
          Style = tbsSeparator
        end
        object ToolButton52: TToolButton
          Left = 306
          Top = 2
          Action = NextErrorAction
        end
      end
    end
    object MapPanel: TPanel
      Left = 0
      Top = 29
      Width = 1350
      Height = 857
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object StructSplitter: TSplitter
        Left = 304
        Top = 0
        Width = 3
        Height = 857
        Cursor = crHSplit
        Beveled = True
      end
      object HexMap: TCylinderMap
        Left = 307
        Top = 0
        Width = 754
        Height = 857
        HelpContext = 3
        Align = alClient
        Color = clBlack
        FirstOdd = False
        GridColor = clGray
        HexSize = 48
        CellWidth = 48
        CellHeight = 42
        Margin = 15
        OnDragOver = HexMapDragOver
        OnDrawHex = HexMapDrawHex
        OnDrawExtra = HexMapDrawExtra
        OnMouseDown = HexMapMouseDown
        OnMouseMove = HexMapMouseMove
        OnMouseUp = HexMapMouseUp
        OnSelectHex = HexMapSelectHex
        OnMoveMap = HexMapMoveMap
      end
      object gAllItems: TPowerGrid
        Left = 1061
        Top = 0
        Width = 289
        Height = 857
        Align = alRight
        ColCount = 3
        DefaultRowColor = clBlack
        Editing = False
        FixedRows = 1
        ImageCol = 0
        Options = [pgoLines, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol, pgoMultiselect]
        RowCount = 1
        Sorted = True
        SortBy = 1
        StickySelect = False
        TopRow = 1
        OnDblClick = ItemGridDblClick
        OnDrawCell = ItemGridDrawCell
        OnDragOver = gAllItemsDragOver
        OnEndDrag = gAllItemsEndDrag
        OnEnter = ItemGridEnter
        OnMouseDown = ItemGridMouseDown
        OnSelectCell = ItemGridSelectCell
        ColWidths = (
          50
          133
          102)
      end
      object StructPanel: TPanel
        Left = 0
        Top = 0
        Width = 304
        Height = 857
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        object StructGrid: TPowerGrid
          Left = 0
          Top = 297
          Width = 304
          Height = 560
          Align = alClient
          Color = clBtnFace
          ColCount = 2
          DefaultRowColor = clBlack
          DefaultRowHeight = 21
          Editing = False
          FixedRows = 0
          ImageCol = 0
          LinesColor = clMedGray
          Options = [pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
          RowCount = 0
          Sorted = True
          SortBy = 0
          ShowHint = True
          StickySelect = True
          TopRow = 0
          OnDrawCell = StructGridDrawCell
          OnMouseDown = StructGridMouseDown
          OnMouseMove = StructGridMouseMove
          ColWidths = (
            2
            298)
        end
        object pStructure: TPanel
          Left = 0
          Top = 0
          Width = 304
          Height = 297
          Align = alTop
          BevelOuter = bvNone
          BorderWidth = 4
          TabOrder = 1
          object pnStructureName: TPanel
            Left = 4
            Top = 4
            Width = 296
            Height = 32
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object StructNumLabel: TLabel
              Left = 275
              Top = 0
              Width = 21
              Height = 32
              Align = alRight
              Caption = '127'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -13
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentFont = False
              Layout = tlCenter
            end
            object spPanel1: TPanel
              Left = 267
              Top = 0
              Width = 8
              Height = 32
              Align = alRight
              BevelOuter = bvNone
              TabOrder = 0
            end
            object Panel6: TPanel
              Left = 0
              Top = 0
              Width = 267
              Height = 32
              Align = alClient
              BevelOuter = bvNone
              Caption = 'Panel6'
              TabOrder = 1
              DesignSize = (
                267
                32)
              object StructNameBevel: TBevel
                Left = 0
                Top = 0
                Width = 267
                Height = 32
                Align = alClient
                Visible = False
              end
              object StructNameEdit: TEdit
                Tag = 2
                Left = 4
                Top = 4
                Width = 259
                Height = 24
                Anchors = [akLeft, akTop, akRight, akBottom]
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
                Text = 'Gold Fish'
                OnEnter = FormEditEnter
                OnExit = FormEditExit
                OnKeyPress = FormEditKeyPress
              end
            end
          end
          object pnStrucDescription: TPanel
            Left = 4
            Top = 40
            Width = 296
            Height = 16
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            DesignSize = (
              296
              16)
            object StructDescrBevel: TBevel
              Left = 0
              Top = 0
              Width = 296
              Height = 16
              Align = alClient
              Visible = False
            end
            object StructDescrEdit: TEdit
              Tag = 3
              Left = 6
              Top = 1
              Width = 287
              Height = 15
              Anchors = [akLeft, akTop, akRight, akBottom]
              AutoSize = False
              BorderStyle = bsNone
              Color = clBtnFace
              TabOrder = 0
              Text = 'Small fast longboat'
              OnEnter = FormEditEnter
              OnExit = FormEditExit
              OnKeyPress = FormEditKeyPress
            end
          end
          object spPanel2: TPanel
            Left = 4
            Top = 36
            Width = 296
            Height = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
          object Panel4: TPanel
            Left = 4
            Top = 289
            Width = 296
            Height = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 3
          end
          object Panel5: TPanel
            Left = 4
            Top = 56
            Width = 296
            Height = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 4
          end
          object Panel7: TPanel
            Left = 4
            Top = 213
            Width = 296
            Height = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 5
          end
          object Panel8: TPanel
            Left = 4
            Top = 144
            Width = 296
            Height = 4
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 6
          end
          object pnStrucInfo: TPanel
            Left = 4
            Top = 60
            Width = 296
            Height = 84
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 7
            object OwnerFlagImage: TImage
              Left = 60
              Top = 16
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
            object StructGroupLabel: TLabel
              Left = 64
              Top = 0
              Width = 45
              Height = 13
              Caption = 'Longboat'
            end
            object Label19: TLabel
              Left = 0
              Top = 0
              Width = 27
              Height = 13
              Caption = 'Type:'
            end
            object Label22: TLabel
              Left = 0
              Top = 16
              Width = 34
              Height = 13
              Caption = 'Owner:'
            end
            object StructOwnerLabel: TLabel
              Left = 72
              Top = 16
              Width = 52
              Height = 13
              Caption = 'Sailors (12)'
            end
            object StructDefLabel: TLabel
              Left = 64
              Top = 32
              Width = 18
              Height = 13
              Caption = '100'
            end
            object Label11: TLabel
              Left = 0
              Top = 32
              Width = 44
              Height = 13
              Caption = 'Defence:'
            end
            object Label13: TLabel
              Left = 0
              Top = 48
              Width = 54
              Height = 13
              Caption = 'Men inside:'
            end
            object StructMenLabel: TLabel
              Left = 64
              Top = 48
              Width = 12
              Height = 13
              Caption = '10'
            end
            object Label16: TLabel
              Left = 0
              Top = 63
              Width = 23
              Height = 13
              Caption = 'Size:'
            end
            object StructSizeLabel: TLabel
              Left = 64
              Top = 63
              Width = 45
              Height = 13
              AutoSize = False
              Caption = '2 / 10'
            end
            object IncomplStructImage: TImage
              Left = 112
              Top = 60
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000000000000000
                000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
                000000FF0000FF0000000000000000FF0000FF000000C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF0000FF0000000000000000FF
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF000000C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF0000000000000000FF
                0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0}
              Transparent = True
            end
          end
          object GroupBox1: TGroupBox
            Left = 4
            Top = 148
            Width = 296
            Height = 65
            Align = alTop
            Caption = 'Transport'
            TabOrder = 8
            object Label14: TLabel
              Left = 20
              Top = 20
              Width = 27
              Height = 13
              Caption = 'Load:'
            end
            object Label15: TLabel
              Left = 20
              Top = 36
              Width = 36
              Height = 13
              Caption = 'Control:'
            end
            object StructLoadLabel: TLabel
              Left = 68
              Top = 20
              Width = 59
              Height = 13
              Caption = '1200 / 1800'
            end
            object StructControlLabel: TLabel
              Left = 68
              Top = 36
              Width = 29
              Height = 13
              Caption = '5 / 15'
            end
          end
          object GroupBox2: TGroupBox
            Left = 4
            Top = 217
            Width = 296
            Height = 72
            Align = alTop
            Caption = 'Passage To'
            TabOrder = 9
            DesignSize = (
              296
              72)
            object lWayTo: TLabel
              Left = 8
              Top = 20
              Width = 201
              Height = 45
              Anchors = [akLeft, akTop, akRight]
              AutoSize = False
              Caption = 'cave (31, 20, underworld) in Underworld'
              Layout = tlCenter
              WordWrap = True
            end
            object btnLinkShaft: TButton
              Left = 218
              Top = 32
              Width = 63
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Link'
              Enabled = False
              TabOrder = 0
              OnClick = btnLinkShaftClick
            end
          end
        end
      end
    end
  end
  object ToolPanel: TPanel
    Left = 0
    Top = 0
    Width = 1916
    Height = 28
    Align = alTop
    TabOrder = 1
    object ToolBar: TToolBar
      Left = 33
      Top = 1
      Width = 620
      Height = 25
      Align = alNone
      Caption = 'ToolBar'
      EdgeBorders = []
      Images = ResForm.BtnImages
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      object ToolButton10: TToolButton
        Left = 0
        Top = 2
        Hint = 'View Order'
        Action = OrdersAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton24: TToolButton
        Left = 23
        Top = 2
        Hint = 'Save Order'
        Action = SaveOrderAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton25: TToolButton
        Left = 46
        Top = 2
        Hint = 'Copy Order to Clipboard'
        Action = CopyOrderAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton46: TToolButton
        Left = 69
        Top = 2
        Hint = 'Send Order'
        Action = MailOrderAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton12: TToolButton
        Left = 92
        Top = 2
        Width = 8
        Caption = 'ToolButton12'
        ImageIndex = 47
        Style = tbsSeparator
      end
      object ToolButton8: TToolButton
        Left = 100
        Top = 2
        Hint = 'Factions'
        Action = FactionAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton23: TToolButton
        Left = 123
        Top = 2
        Width = 8
        Caption = 'ToolButton23'
        ImageIndex = 28
        Style = tbsSeparator
      end
      object ToolButton1: TToolButton
        Left = 131
        Top = 2
        Action = ItemEditAction
      end
      object ToolButton2: TToolButton
        Left = 154
        Top = 2
        Action = SkillEditAction
      end
      object ToolButton14: TToolButton
        Left = 177
        Top = 2
        Action = StructEditAction
      end
      object ToolButton27: TToolButton
        Left = 200
        Top = 2
        Width = 8
        Caption = 'ToolButton27'
        ImageIndex = 43
        Style = tbsSeparator
      end
      object ToolButton47: TToolButton
        Left = 208
        Top = 2
        Hint = 'Scripts'
        Action = ScriptEditAction
        ParentShowHint = False
        ShowHint = True
      end
      object ScriptSplit: TToolButton
        Left = 231
        Top = 2
        Width = 8
        Caption = 'ScriptSplit'
        ImageIndex = 47
        Style = tbsSeparator
      end
      object ToolButton9: TToolButton
        Left = 239
        Top = 2
        Hint = 'Turn Events'
        Action = TurnEventsAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton43: TToolButton
        Left = 262
        Top = 2
        Hint = 'Advisor Warnings'
        Action = AdvisorWarnAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton50: TToolButton
        Left = 285
        Top = 2
        Width = 8
        Caption = 'ToolButton50'
        ImageIndex = 47
        Style = tbsSeparator
      end
      object ToolButton32: TToolButton
        Left = 293
        Top = 2
        Hint = 'Battles in Region'
        Action = BattlesAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton16: TToolButton
        Left = 316
        Top = 2
        Action = TradeStructAction
      end
      object ToolButton28: TToolButton
        Left = 339
        Top = 2
        Width = 8
        Caption = 'ToolButton28'
        ImageIndex = 24
        Style = tbsSeparator
      end
      object ToolButton42: TToolButton
        Left = 347
        Top = 2
        Hint = 'Soldiers in Unit'
        Action = SoldiersAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton34: TToolButton
        Left = 370
        Top = 2
        Action = UnitProductionAction
      end
      object ToolButton41: TToolButton
        Left = 393
        Top = 2
        Hint = 'Make Unit Avatar'
        Action = MakeAvatarAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton40: TToolButton
        Left = 416
        Top = 2
        Width = 8
        Caption = 'ToolButton40'
        ImageIndex = 47
        Style = tbsSeparator
      end
      object ToolButton26: TToolButton
        Left = 424
        Top = 2
        Hint = 'Mini Map'
        Action = MiniMapAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton29: TToolButton
        Left = 447
        Top = 2
        Width = 22
        Caption = 'ToolButton29'
        ImageIndex = 48
        Style = tbsSeparator
      end
      object ToolButton18: TToolButton
        Left = 469
        Top = 2
        Hint = 'Previous Turn'
        Action = PrevTurnAction
        ParentShowHint = False
        ShowHint = True
      end
      object ToolButton21: TToolButton
        Left = 492
        Top = 2
        Width = 8
        Caption = 'ToolButton21'
        ImageIndex = 43
        Style = tbsSeparator
      end
      object TurnCombo: TComboBox
        Left = 500
        Top = 2
        Width = 61
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
        OnChange = TurnComboChange
      end
      object ToolButton22: TToolButton
        Left = 561
        Top = 2
        Width = 8
        Caption = 'ToolButton22'
        ImageIndex = 43
        Style = tbsSeparator
      end
      object ToolButton20: TToolButton
        Left = 569
        Top = 2
        Hint = 'Next Turn'
        Action = NextTurnAction
        ParentShowHint = False
        ShowHint = True
      end
    end
    object Panel3: TPanel
      Left = 1869
      Top = 1
      Width = 46
      Height = 26
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      DesignSize = (
        46
        26)
      object btnStopProcessor: TSpeedButton
        Left = 19
        Top = 2
        Width = 23
        Height = 22
        Hint = 'Stop Order Processor'
        Action = StopProcessorAction
        Anchors = [akTop, akRight]
        Glyph.Data = {
          36030000424D3603000000000000360000002800000010000000100000000100
          18000000000000030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00000000000000000000000000
          0000000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF0000FF0000FF00
          00FF0000FF0000FF0000FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFF0000FF0000FF0000FF0000FF0000FF0000FFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF0000FF0000FF00
          00FF0000FF0000FF0000FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFF0000FF0000FF0000FF0000FF0000FF0000FFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFF0000FF0000FF00
          00FF0000FF0000FF0000FFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF000000FFFFFF0000FF0000FF0000FF0000FF0000FF0000FFFFFFFF0000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFF0000000000000000000000000000000000000000000000000000000000
          00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        Margin = 1
        ParentShowHint = False
        ShowHint = True
      end
    end
    object ToolBar3: TToolBar
      Left = 5
      Top = 1
      Width = 28
      Height = 29
      Align = alNone
      ButtonHeight = 23
      Caption = 'ToolBar3'
      EdgeBorders = []
      Images = ResForm.BtnImages
      TabOrder = 2
      object ToolButton3: TToolButton
        Left = 0
        Top = 2
        Action = ManagerAction
      end
    end
  end
  object pnRightSidebar: TPanel
    Left = 1356
    Top = 28
    Width = 560
    Height = 1006
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object spRegionUnit: TSplitter
      Left = 0
      Top = 465
      Width = 560
      Height = 3
      Cursor = crVSplit
      Align = alTop
    end
    object pnRegion: TPanel
      Left = 0
      Top = 0
      Width = 560
      Height = 465
      Align = alTop
      TabOrder = 0
      object WeatherLabel: TLabel
        Left = 1
        Top = 158
        Width = 558
        Height = 13
        Align = alTop
        Caption = 'It was clear last month; it will be clear next month.'
        WordWrap = True
      end
      object Label24: TLabel
        Left = 1
        Top = 378
        Width = 558
        Height = 13
        Align = alTop
        Caption = 'Notes:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Bevel1: TBevel
        Left = 1
        Top = 370
        Width = 558
        Height = 8
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel2: TBevel
        Left = 1
        Top = 171
        Width = 558
        Height = 8
        Align = alTop
        Shape = bsSpacer
      end
      object Bevel3: TBevel
        Left = 1
        Top = 150
        Width = 558
        Height = 8
        Align = alTop
        Shape = bsSpacer
      end
      object RegionInfoPanel: TPanel
        Left = 1
        Top = 1
        Width = 558
        Height = 149
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object GateImage: TImage
          Left = 16
          Top = 4
          Width = 16
          Height = 16
          Picture.Data = {
            07544269746D617036030000424D360300000000000036000000280000001000
            000010000000010018000000000000030000CE0E0000C40E0000000000000000
            0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0
            C0C0C0000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
            00000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000C0C0C0C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C000000000FFFF000000000000000000000000000000
            00000000000000B3B3000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000000000FF
            FF00000080000080000080000080000080000000000000B3B3000000C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C000000000FFFF000000800000800000800000800000
            80000000000000B3B3000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000000000FF
            FF000000800000FF0000FF000080000080000000000000B3B3000000C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C000000000FFFF000000FF0000800000800000FF0000
            80000000000000B3B3000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000000000FF
            FF000000800000800000FF000080000080000000000000B3B3000000C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C000000000FFFF000000800000FF0000800000800000
            FF000000000000B3B3000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C000000000FF
            FF000000800000800000FF0000FF000080000000000000B3B3000000C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C000000000FFFF000000800000800000800000800000
            80000000000000B3B3000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
            00000000800000800000800000800000800000000000000000C0C0C0C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000800000800000800000
            000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
            C0000000C0C0C0000000000000000000C0C0C0000000C0C0C0C0C0C0C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
            C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
            C0C0}
          Transparent = True
        end
        object GateLabel: TLabel
          Left = 32
          Top = 12
          Width = 25
          Height = 13
          AutoSize = False
          Caption = '12'
        end
        object LandLabel: TLabel
          Left = 81
          Top = 4
          Width = 92
          Height = 24
          Alignment = taRightJustify
          Caption = 'Skottskog'
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Black Chancery'
          Font.Style = []
          ParentFont = False
        end
        object HexLabel: TLabel
          Left = 80
          Top = 32
          Width = 93
          Height = 13
          Caption = 'plain (12,41), jun 12'
        end
        object SettlLabel: TLabel
          Left = 80
          Top = 48
          Width = 87
          Height = 13
          Caption = 'Drumcollogher city'
        end
        object PeasantsLabel: TLabel
          Left = 80
          Top = 64
          Width = 81
          Height = 13
          Caption = '1234 wood elves'
        end
        object TaxRateLabel: TLabel
          Left = 80
          Top = 80
          Width = 41
          Height = 13
          AutoSize = False
          Caption = '342'
        end
        object WagesLabel: TLabel
          Left = 80
          Top = 96
          Width = 41
          Height = 13
          AutoSize = False
          Caption = '12'
        end
        object EnterLabel: TLabel
          Left = 80
          Top = 128
          Width = 41
          Height = 13
          AutoSize = False
          Caption = '13'
        end
        object Label1: TLabel
          Left = 8
          Top = 32
          Width = 22
          Height = 13
          Caption = 'Hex:'
        end
        object Label4: TLabel
          Left = 8
          Top = 48
          Width = 53
          Height = 13
          Caption = 'Settlement:'
        end
        object Label5: TLabel
          Left = 8
          Top = 64
          Width = 47
          Height = 13
          Caption = 'Peasants:'
        end
        object Label6: TLabel
          Left = 8
          Top = 80
          Width = 47
          Height = 13
          Caption = 'Tax Rate:'
        end
        object Label7: TLabel
          Left = 8
          Top = 96
          Width = 37
          Height = 13
          Caption = 'Wages:'
        end
        object Label8: TLabel
          Left = 8
          Top = 128
          Width = 68
          Height = 13
          Caption = 'Entertainment:'
        end
        object imgWeatherNext: TImage
          Left = 4
          Top = 4
          Width = 10
          Height = 10
          Picture.Data = {
            07544269746D617076010000424D760100000000000036000000280000000A00
            00000A000000010018000000000040010000120B0000120B0000000000000000
            0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            0000FF00FFFF00FFFF00FF000000FF00FFFF00FF000000FF00FFFF00FFFF00FF
            0000FF00FF000000FF00FF04FDEF000000FF00FF04FDEF000000FF00FFFF00FF
            0000FF00FF04FDEF000000000000000000000000000000000000FF00FFFF00FF
            0000FF00FFFF00FF00000004FDEF04FDEF04FDEF00000004FDEF000000FF00FF
            0000FF00FF00000000000004FDEF04FDEF04FDEF000000FF00FFFF00FFFF00FF
            0000FF00FF04FDEF00000004FDEF04FDEF04FDEF000000000000FF00FFFF00FF
            0000FF00FFFF00FF00000000000000000000000000000004FDEF000000FF00FF
            0000FF00FFFF00FF04FDEF000000FF00FF04FDEF000000FF00FFFF00FFFF00FF
            0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
            0000}
          Transparent = True
        end
        object TaxMenLabel: TLabel
          Left = 124
          Top = 80
          Width = 23
          Height = 13
          Caption = '7 / 7'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object EnterMenLabel: TLabel
          Left = 124
          Top = 128
          Width = 23
          Height = 13
          Caption = '7 / 7'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object WorkLabel: TLabel
          Left = 124
          Top = 112
          Width = 23
          Height = 13
          Caption = '7 / 7'
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGrayText
          Font.Height = -11
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object Label20: TLabel
          Left = 8
          Top = 112
          Width = 57
          Height = 13
          Caption = 'Max wages:'
        end
        object MaxWagesLabel: TLabel
          Left = 80
          Top = 112
          Width = 41
          Height = 13
          AutoSize = False
          Caption = '108923'
        end
      end
      object NotesMemo: TMemo
        Left = 1
        Top = 391
        Width = 558
        Height = 73
        Align = alClient
        TabOrder = 1
        OnExit = NotesMemoExit
      end
      object TradePanel: TPanel
        Left = 1
        Top = 179
        Width = 558
        Height = 191
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 2
        OnResize = TradePanelResize
        object pnForSale: TPanel
          Left = 105
          Top = 0
          Width = 365
          Height = 191
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label17: TLabel
            Left = 0
            Top = 0
            Width = 365
            Height = 15
            Align = alTop
            AutoSize = False
            Caption = ' For Sale:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object ForSaleGrid: TPowerGrid
            Left = 0
            Top = 15
            Width = 365
            Height = 176
            Align = alClient
            ColCount = 3
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
            OnDblClick = ItemGridDblClick
            OnDrawCell = ItemGridDrawCell
            ColWidths = (
              45
              96
              220)
          end
        end
        object pnProducts: TPanel
          Left = 0
          Top = 0
          Width = 105
          Height = 191
          Align = alLeft
          BevelOuter = bvNone
          Caption = 'pnProducts'
          TabOrder = 1
          object Label12: TLabel
            Left = 0
            Top = 0
            Width = 105
            Height = 13
            Align = alTop
            Caption = ' Products:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object ProductGrid: TPowerGrid
            Left = 0
            Top = 13
            Width = 105
            Height = 178
            Align = alClient
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
            OnDblClick = ItemGridDblClick
            OnDrawCell = ItemGridDrawCell
            ColWidths = (
              45
              56)
          end
        end
        object pnWanted: TPanel
          Left = 470
          Top = 0
          Width = 88
          Height = 191
          Align = alRight
          BevelOuter = bvNone
          Caption = 'pnWanted'
          TabOrder = 2
          object Label18: TLabel
            Left = 0
            Top = 0
            Width = 88
            Height = 15
            Align = alTop
            AutoSize = False
            Caption = ' Wanted:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object WantedGrid: TPowerGrid
            Left = 0
            Top = 15
            Width = 88
            Height = 176
            Align = alClient
            ColCount = 3
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            RowCount = 0
            Sorted = False
            SortBy = 1
            StickySelect = False
            TopRow = 0
            OnDblClick = ItemGridDblClick
            OnDrawCell = ItemGridDrawCell
            ColWidths = (
              45
              96
              -57)
          end
        end
      end
    end
    object pnUnit: TPanel
      Left = 0
      Top = 468
      Width = 560
      Height = 538
      Align = alClient
      TabOrder = 1
      object UnitPageSplit: TSplitter
        Left = 1
        Top = 254
        Width = 558
        Height = 3
        Cursor = crVSplit
        Align = alBottom
      end
      object UnitPControl: TPageControl
        Tag = 2
        Left = 1
        Top = 257
        Width = 558
        Height = 280
        ActivePage = OrderSheet
        Align = alBottom
        DockSite = True
        MultiLine = True
        TabIndex = 0
        TabOrder = 0
        TabPosition = tpBottom
        object OrderSheet: TTabSheet
          Caption = 'Orders'
          ImageIndex = 3
          object Panel2: TPanel
            Left = 0
            Top = 0
            Width = 550
            Height = 21
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object lMonthOut: TLabel
              Left = 24
              Top = 2
              Width = 125
              Height = 13
              AutoSize = False
              Caption = '12 / 20'
            end
            object imgMonthOrder: TImage
              Left = 3
              Top = 0
              Width = 16
              Height = 18
              Transparent = True
            end
            object ToolBar1: TToolBar
              Left = 511
              Top = 0
              Width = 39
              Height = 21
              Align = alRight
              ButtonHeight = 18
              ButtonWidth = 19
              Caption = 'ToolBar1'
              EdgeBorders = []
              Flat = True
              Images = ResForm.SmallBtnList
              TabOrder = 0
              object btnCheckOrder: TToolButton
                Left = 0
                Top = 0
                Hint = 'Check orders'
                Caption = 'btnCheckOrder'
                ImageIndex = 5
                ParentShowHint = False
                ShowHint = True
                OnClick = CheckOrderBtnClick
              end
              object btnClearOrder: TToolButton
                Left = 19
                Top = 0
                Hint = 'Clear orders'
                Caption = 'btnClearOrder'
                ImageIndex = 4
                ParentShowHint = False
                ShowHint = True
                OnClick = ClearOrderBtnClick
              end
            end
          end
          object OrderMemo: TMemo
            Left = 0
            Top = 21
            Width = 550
            Height = 161
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Consolas'
            Font.Style = []
            ParentFont = False
            ScrollBars = ssBoth
            TabOrder = 1
            WordWrap = False
            OnExit = OrderMemoExit
            OnKeyUp = OrderMemoKeyUp
          end
          object LoadPanel: TPanel
            Left = 0
            Top = 182
            Width = 550
            Height = 72
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 2
            object MoveTypeLabel: TLabel
              Left = 61
              Top = 6
              Width = 36
              Height = 13
              Caption = 'walking'
            end
            object WingWeightLabel: TLabel
              Left = 168
              Top = 28
              Width = 67
              Height = 13
              Caption = '100 (max 200)'
            end
            object WingWeightWarning: TImage
              Left = 132
              Top = 27
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000000000000000
                000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
                000000FF0000FF0000000000000000FF0000FF000000C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF0000FF0000000000000000FF
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF000000C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF0000000000000000FF
                0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0}
              Transparent = True
            end
            object Image5: TImage
              Left = 148
              Top = 27
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000D80E0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00
                FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FF000000FF00FF
                FF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00
                FFFF00FFFF00FFFF00FF000000808080FF00FFFF00FF000000FF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF000000000000000000FF00FF000000000000000000
                FF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF0000008080
                80FFFFFF000000000000D0D0D0D0D0D0000000000000FF00FF000000FF00FFFF
                00FFFF00FF000000FF00FF000000FFFFFFD0D0D0D0D0D0FFFFFFFFFFFFD0D0D0
                000000D0D0D0000000000000FF00FFFF00FFFF00FFFF00FF000000FF00FF0000
                00FFFFFFFFFFFF000000000000FFFFFFFFFFFFD0D0D0000000808080FF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000FFFFFFD0D0D0FFFFFF
                FFFFFFD0D0D0000000FF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FF0000
                00000000FFFFFFC0C0C0000000000000FFFFFFD0D0D0000000FF00FFFF00FFFF
                00FF000000000000FFFFFF000000C0C0C0FFFFFFFFFFFF000000FF00FF000000
                FFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFFC0C0
                C0FFFFFFFFFFFF000000000000C0C0C0C0C0C0000000FFFFFF000000FF00FFFF
                00FFFF00FFFF00FF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
                C0C0C0000000FFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FF000000FFFF
                FFFFFFFFFFFFFF000000000000C0C0C0FFFFFF000000FFFFFF000000FF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFF000000FF00FF000000
                000000000000FF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FF000000000000FF00FFFF00FF000000FF00FF000000FF00FF000000FF00FFFF
                00FF}
              Transparent = True
            end
            object TransportWeightWarning: TImage
              Left = 132
              Top = 47
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000000000000000
                000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
                000000FF0000FF0000000000000000FF0000FF000000C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF0000FF0000000000000000FF
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF000000C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF0000000000000000FF
                0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0}
              Transparent = True
            end
            object HorsesWeightWarning: TImage
              Left = 4
              Top = 47
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000000000000000
                000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
                000000FF0000FF0000000000000000FF0000FF000000C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF0000FF0000000000000000FF
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF000000C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF0000000000000000FF
                0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0}
              Transparent = True
            end
            object UnitWeightWarning: TImage
              Left = 4
              Top = 27
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0000000000000000000000000000000
                000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000
                000000FF0000FF0000000000000000FF0000FF000000C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF0000FF0000000000000000FF
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF0000FF000000C0C0C0C0
                C0C0C0C0C0C0C0C00000000000FF0000FF0000FF000000FFFFFFFFFFFF000000
                0000FF0000FF0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000
                FF0000FF000000FFFFFFFFFFFF0000000000FF0000FF000000C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C00000000000FF0000FF0000000000000000FF
                0000FF000000C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0000000000000000000000000000000000000C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0C0
                C0C0}
              Transparent = True
            end
            object TransportWeightLabel: TLabel
              Left = 168
              Top = 48
              Width = 96
              Height = 13
              Caption = '100/200 (max 1000)'
            end
            object HorsesWeightLabel: TLabel
              Left = 40
              Top = 48
              Width = 67
              Height = 13
              Caption = '100 (max 200)'
            end
            object UnitWeightLabel: TLabel
              Left = 40
              Top = 28
              Width = 67
              Height = 13
              Caption = '100 (max 200)'
              Color = clBtnFace
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'MS Sans Serif'
              Font.Style = []
              ParentColor = False
              ParentFont = False
            end
            object Image3: TImage
              Left = 148
              Top = 47
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FF808080FF00FFFF00FFFF00FF00000000FFFF00FFFF008080
                000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00
                FF80808000FFFF00FFFF00FFFF00FFFF008080000000FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FF808080FFFFFF00000080808080808080808000FFFF00FFFF
                000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFF
                FF000000FF00FFFF00FF80808000FFFF000000FF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFF000000000000FF00FF000000
                FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8080
                80FFFFFFFFFFFFFFFFFF000000000000000000000000000000FF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFF000000
                C0C0C0C0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8080
                80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFC0C0C0
                C0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FF808080FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0000000
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF808080FFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Image2: TImage
              Left = 20
              Top = 47
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000CE0E0000C40E0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FF
                FF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00
                FFFF00FFFF00FFFF00FF000000808080FF00FFFF00FF000000FF00FFFF00FFFF
                00FFFF00FFFF00FF000000808080FF00FFFF00FF000000FF00FF000000000000
                FF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FF0000008080
                80000000000000000000D0D0D0D0D0D0000000000000FF00FF000000FF00FFFF
                00FFFF00FFFF00FFFF00FF000000000000D0D0D0D0D0D0FFFFFFFFFFFFD0D0D0
                000000D0D0D0000000000000FF00FFFF00FFFF00FFFF00FFFF00FF000000FFFF
                FFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFD0D0D0000000808080FF00FFFF
                00FFFF00FF000000FF00FF000000FFFFFFFFFFFFFFFFFFFFFFFFD0D0D0000000
                FFFFFFD0D0D0000000FF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FF0000
                00FFFFFFFFFFFF000000000000000000FFFFFFD0D0D0000000FF00FFFF00FFFF
                00FFFF00FF000000FF00FF000000FF00FF000000000000FF00FFFF00FF000000
                FFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000000000C0C0C0C0C0C0000000808080FF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FFFFFFFFFFFFFFFFFF
                C0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FF000000000000000000C0C0C0FFFFFF000000FF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
                000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FF000000FF00FF000000FF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Image1: TImage
              Left = 20
              Top = 27
              Width = 16
              Height = 16
              Picture.Data = {
                07544269746D617036030000424D360300000000000036000000280000001000
                000010000000010018000000000000030000C40E0000C40E0000000000000000
                0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000FF00FFFF00FFFF00FFFF00
                FF000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FFFF00FF000000000000000000000000FFFF00B5B500000000FF00FFFF00FF
                FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF00000080FFFF80FFFF0000
                00FFFF00B5B500000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF000000000000000000000000FFFF00FFFF00FFFF00B5B500000000FF00FF
                FF00FFFF00FF000000FF00FFFF00FFFF00FF000000FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00B5B500000000000000000000000000FF00FFFF00FFFF
                00FF000000FFFF00FFFF00FFFF00FFFF00000000000000000000000000000000
                FFFFFFFFFFFFFFFFFF000000FF00FFFF00FF000000FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00000000FFFFFFFFFFFFFFFFFF000000000000FF00FFFF
                00FF000000FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00000000FFFFFF
                FFFFFF000000FFFFFFFFFFFF000000000000000000FFFF00FFFF00FFFF00FFFF
                00FFFF00FFFF00FFFF00000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00
                0000FF00FF000000000000000000000000000000000000000000000000000000
                FFFFFFFFFFFF808080808080000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FF000000000000000000000000FFFFFFFFFFFFFFFFFF000000FF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000000000000000
                000000000000000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FF000000000000000000000000000000000000000000FF
                00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF000000
                000000000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
                FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
                00FF}
              Transparent = True
            end
            object Label2: TLabel
              Left = 3
              Top = 6
              Width = 53
              Height = 13
              Caption = 'Movement:'
            end
          end
        end
        object MsgSheet: TTabSheet
          Caption = 'Messages'
          ImageIndex = 2
          object MsgGrid: TPowerGrid
            Left = 0
            Top = 0
            Width = 288
            Height = 254
            Align = alClient
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoColSizing, pgoColMoving, pgoRowSelect, pgoMultilineCells]
            RowCount = 1
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDrawCell = MsgGridDrawCell
            ColWidths = (
              19
              153)
          end
        end
      end
      object pnItemsAndSkills: TPanel
        Left = 1
        Top = 114
        Width = 558
        Height = 140
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object ItemSplit: TSplitter
          Left = 370
          Top = 0
          Width = 3
          Height = 140
          Cursor = crHSplit
          Align = alRight
        end
        object pItemGrid: TPanel
          Left = 0
          Top = 0
          Width = 370
          Height = 140
          Align = alClient
          BevelOuter = bvNone
          Constraints.MinHeight = 40
          TabOrder = 0
          object Label9: TLabel
            Left = 0
            Top = 0
            Width = 370
            Height = 13
            Align = alTop
            Caption = 'Items:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object ItemGrid: TPowerGrid
            Left = 0
            Top = 13
            Width = 370
            Height = 101
            Align = alClient
            ColCount = 2
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol, pgoMultiselect]
            RowCount = 0
            Sorted = True
            SortBy = 1
            ShowHint = True
            StickySelect = False
            TopRow = 0
            OnDblClick = ItemGridDblClick
            OnDrawCell = ItemGridDrawCell
            OnDragOver = ItemGridDragOver
            OnEndDrag = ItemGridEndDrag
            OnEnter = ItemGridEnter
            OnMouseDown = ItemGridMouseDown
            OnSelectCell = ItemGridSelectCell
            ColWidths = (
              48
              318)
          end
          object pGiveTools: TPanel
            Left = 0
            Top = 114
            Width = 370
            Height = 26
            Align = alBottom
            BevelOuter = bvNone
            TabOrder = 1
            object Label21: TLabel
              Left = 106
              Top = 7
              Width = 25
              Height = 13
              Caption = 'Give:'
            end
            object ToolBar2: TToolBar
              Left = 5
              Top = 4
              Width = 96
              Height = 24
              Align = alNone
              Caption = 'MapToolbar'
              EdgeBorders = []
              Flat = True
              Images = ResForm.BtnImages
              ParentShowHint = False
              ShowHint = False
              TabOrder = 0
              object tbGiveAll: TToolButton
                Tag = 1
                Left = 0
                Top = 0
                Hint = 'Give everything'
                AllowAllUp = True
                Caption = 'Level'
                DragMode = dmAutomatic
                ImageIndex = 0
                ParentShowHint = False
                ShowHint = True
                OnEndDrag = tbMaskGiveEndDrag
              end
              object tbGiveBattle: TToolButton
                Tag = 2
                Left = 23
                Top = 0
                Hint = 'Give battle items'
                Caption = 'BookmarkAction'
                DragMode = dmAutomatic
                ImageIndex = 28
                ParentShowHint = False
                ShowHint = True
                OnEndDrag = tbMaskGiveEndDrag
              end
              object tbGiveSpoils: TToolButton
                Tag = 4
                Left = 46
                Top = 0
                Hint = 'Give spoils'
                Caption = 'tbGiveSpoils'
                DragMode = dmAutomatic
                ImageIndex = 63
                ParentShowHint = False
                ShowHint = True
                OnEndDrag = tbMaskGiveEndDrag
              end
              object tbGiveTrade: TToolButton
                Tag = 3
                Left = 69
                Top = 0
                Hint = 'Give trade goods'
                Caption = 'tbGiveTrade'
                DragMode = dmAutomatic
                ImageIndex = 43
                ParentShowHint = False
                ShowHint = True
                OnEndDrag = tbMaskGiveEndDrag
              end
            end
            object eGiveAmt: TIntEdit
              Left = 136
              Top = 4
              Width = 63
              Height = 22
              MaxValue = 9999999
              MinValue = 0
              TabOrder = 1
              Value = 0
            end
          end
        end
        object pnSkills: TPanel
          Left = 373
          Top = 0
          Width = 185
          Height = 140
          Align = alRight
          BevelOuter = bvNone
          TabOrder = 1
          object Label23: TLabel
            Left = 0
            Top = 0
            Width = 185
            Height = 13
            Align = alTop
            Caption = 'Skills:'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = [fsBold]
            ParentFont = False
          end
          object SkillGrid: TPowerGrid
            Left = 0
            Top = 13
            Width = 185
            Height = 127
            Align = alClient
            ColCount = 3
            DefaultRowColor = clBlack
            Editing = False
            FixedRows = 0
            Images = ResForm.IconList
            ImageCol = 0
            Options = [pgoLines, pgoColSizing, pgoColMoving, pgoColHiding, pgoSortOnClick, pgoRowSelect, pgoStretchLastCol]
            PopupMenu = PopMenu
            RowCount = 0
            Sorted = False
            SortBy = 0
            StickySelect = False
            TopRow = 0
            OnDblClick = SkillGridDblClick
            OnMouseDown = SkillGridMouseDown
            ColWidths = (
              126
              25
              30)
          end
        end
      end
      object pnUnitName: TPanel
        Left = 1
        Top = 1
        Width = 558
        Height = 36
        Align = alTop
        BevelOuter = bvNone
        BorderWidth = 4
        TabOrder = 2
        object NumLabel: TLabel
          Left = 526
          Top = 4
          Width = 28
          Height = 28
          Align = alRight
          Alignment = taRightJustify
          Caption = '6353'
          Constraints.MaxWidth = 32
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'MS Sans Serif'
          Font.Style = []
          ParentFont = False
          Layout = tlCenter
        end
        object Bevel4: TBevel
          Left = 518
          Top = 4
          Width = 8
          Height = 28
          Align = alRight
          Shape = bsSpacer
        end
        object pnName: TPanel
          Left = 4
          Top = 4
          Width = 514
          Height = 28
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            514
            28)
          object UnitNameEdit: TEdit
            Left = 0
            Top = 0
            Width = 514
            Height = 28
            Anchors = [akLeft, akTop, akRight]
            AutoSize = False
            BevelInner = bvNone
            BevelKind = bkSoft
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
            OnEnter = FormEditEnter
            OnExit = FormEditExit
            OnKeyPress = FormEditKeyPress
          end
        end
      end
      object UnitMainDataPanel: TPanel
        Left = 1
        Top = 37
        Width = 558
        Height = 52
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 3
        object UnitMainDataTextPanel: TPanel
          Left = 0
          Top = 0
          Width = 506
          Height = 52
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          OnResize = UnitMainDataTextPanelResize
          object Label3: TLabel
            Left = 0
            Top = 20
            Width = 38
            Height = 13
            Caption = 'Faction:'
          end
          object FactionLabel: TLabel
            Left = 56
            Top = 20
            Width = 133
            Height = 13
            AutoSize = False
            Caption = 'Forest Owls'
            ShowAccelChar = False
          end
          object Label10: TLabel
            Left = 0
            Top = 36
            Width = 34
            Height = 13
            Caption = 'Object:'
          end
          object StructImage: TImage
            Left = 38
            Top = 34
            Width = 16
            Height = 16
            Picture.Data = {
              07544269746D617036030000424D360300000000000036000000280000001000
              000010000000010018000000000000030000CE0E0000C40E0000000000000000
              0000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FFFF00FF000000000000000000FF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FF808080FF00FFFF00FFFF00FF00000000FFFF00FFFF008080
              000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00
              FF80808000FFFF00FFFF00FFFF00FFFF008080000000FF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FF808080FFFFFF00000080808080808080808000FFFF00FFFF
              000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFF
              FF000000FF00FFFF00FF80808000FFFF000000FF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFF000000000000FF00FF000000
              FF00FFFF00FF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8080
              80FFFFFFFFFFFFFFFFFF000000000000000000000000000000FF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFFFFFFF000000
              C0C0C0C0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF8080
              80FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFFFFFFFC0C0C0
              C0C0C0000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FF808080FFFFFFFFFFFFFFFFFFC0C0C0000000FF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080FFFFFFFFFFFFC0C0C0000000
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FF808080FFFFFF000000FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FF808080000000FF00FFFF00FF
              FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00
              FFFF00FF808080FF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF00FFFF
              00FF}
            Transparent = True
          end
          object StructLabel: TLabel
            Left = 56
            Top = 36
            Width = 133
            Height = 13
            AutoSize = False
            Caption = 'Ship [331] : Galleon'
            ShowAccelChar = False
          end
          object UnitDescrBevel: TBevel
            Left = 20
            Top = 0
            Width = 118
            Height = 17
            Visible = False
          end
          object FactionFlagImage: TImage
            Left = 44
            Top = 20
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
          object btnLocal: TSpeedButton
            Left = 0
            Top = 0
            Width = 18
            Height = 18
            Hint = 'Local Description (hide from other players)'
            AllowAllUp = True
            GroupIndex = -1
            Flat = True
            Glyph.Data = {
              E6010000424DE60100000000000036000000280000000C0000000C0000000100
              180000000000B0010000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFF000000000000000000000000000000000000FFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFF00000000FF0000FF0000FF0069D20069D20069D200000000FF
              FFFFFFFFFFFFFFFF00000000FF0000FF0000000000FF0000000000000069D200
              000000FFFFFFFFFFFFFFFFFF00000000FF0000FF0000FF0000000000FF0000FF
              00000000000000FFFFFFFFFFFF00000000FF0000000000000000FF0000FF0069
              D200000000FFFFFF000000FFFFFFFFFFFF000000000000FFFF00FFFF00000000
              000000FFFF00000000FFFFFFFFFFFFFFFFFFFFFFFF00000000FF000000000000
              0000FF0000FF00000000000000FFFFFFFFFFFFFFFFFFFFFFFF00000000FF0000
              FF0000FF0000FF0000FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000
              00FF0000FF0000000000000000FF00000000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FF00000000FF00000000FFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFF}
            ParentShowHint = False
            ShowHint = True
            OnClick = btnLocalClick
          end
          object UnitDescrEdit: TEdit
            Tag = 1
            Left = 24
            Top = 1
            Width = 113
            Height = 15
            AutoSize = False
            BorderStyle = bsNone
            Color = clBtnFace
            TabOrder = 0
            Text = 'Ugly Stupid Ogres'
            OnEnter = FormEditEnter
            OnExit = FormEditExit
            OnKeyPress = FormEditKeyPress
          end
        end
        object UnitImagePanel: TPanel
          Left = 506
          Top = 0
          Width = 52
          Height = 52
          Align = alRight
          BevelInner = bvLowered
          BevelOuter = bvNone
          TabOrder = 1
          object UnitImage: TImage
            Left = 1
            Top = 1
            Width = 50
            Height = 50
            OnDblClick = UnitImageDblClick
          end
        end
      end
      object FlagBar: TToolBar
        Left = 1
        Top = 89
        Width = 558
        Height = 25
        ButtonHeight = 19
        ButtonWidth = 19
        Caption = 'FlagBar'
        EdgeBorders = []
        Images = ResForm.FlagImages
        TabOrder = 4
        object GuardFlagBtn: TToolButton
          Left = 0
          Top = 2
          Hint = 'On guard'
          AllowAllUp = True
          Caption = 'GuardFlagBtn'
          ImageIndex = 0
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton33: TToolButton
          Tag = 1
          Left = 19
          Top = 2
          Hint = 'Taxing'
          AllowAllUp = True
          Caption = 'ToolButton33'
          ImageIndex = 1
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object AvoidFlagBtn: TToolButton
          Tag = 2
          Left = 38
          Top = 2
          Hint = 'Avoiding'
          AllowAllUp = True
          Caption = 'AvoidFlagBtn'
          ImageIndex = 2
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton35: TToolButton
          Tag = 3
          Left = 57
          Top = 2
          Hint = 'Behind'
          AllowAllUp = True
          Caption = 'ToolButton35'
          ImageIndex = 3
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton36: TToolButton
          Tag = 4
          Left = 76
          Top = 2
          Hint = 'Holding'
          AllowAllUp = True
          Caption = 'ToolButton36'
          ImageIndex = 4
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton37: TToolButton
          Tag = 5
          Left = 95
          Top = 2
          Hint = 'Receive no aid'
          AllowAllUp = True
          Caption = 'ToolButton37'
          ImageIndex = 5
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton45: TToolButton
          Tag = 6
          Left = 114
          Top = 2
          Hint = 'Will not cross water'
          AllowAllUp = True
          Caption = 'ToolButton45'
          ImageIndex = 6
          ParentShowHint = False
          ShowHint = True
          Style = tbsCheck
          OnClick = UnitFlagBtnClick
        end
        object ToolButton48: TToolButton
          Left = 133
          Top = 2
          Width = 4
          Caption = 'ToolButton48'
          ImageIndex = 8
          Style = tbsSeparator
        end
        object btnConsume: TToolButton
          Left = 137
          Top = 2
          Caption = 'btnConsume'
          ImageIndex = 7
          ParentShowHint = False
          ShowHint = True
          OnClick = ExtFlagBtnClick
        end
        object btnReveal: TToolButton
          Tag = 1
          Left = 156
          Top = 2
          Caption = 'btnReveal'
          ImageIndex = 8
          ParentShowHint = False
          ShowHint = True
          OnClick = ExtFlagBtnClick
        end
        object btnSpoils: TToolButton
          Tag = 2
          Left = 175
          Top = 2
          Caption = 'btnSpoils'
          ImageIndex = 9
          ParentShowHint = False
          ShowHint = True
          OnClick = ExtFlagBtnClick
        end
      end
    end
  end
  object MainMenu: TMainMenu
    Images = ResForm.BtnImages
    Left = 476
    Top = 85
    object File1: TMenuItem
      Caption = 'File'
      object GameManagerItm: TMenuItem
        Action = ManagerAction
      end
      object Runnextturn1: TMenuItem
        Action = EngineRunAction
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object SaveOrder1: TMenuItem
        Action = SaveOrderAction
      end
      object SaveOrderAs: TMenuItem
        Action = SaveAsAction
      end
      object CopyOrder1: TMenuItem
        Action = CopyOrderAction
        Caption = 'Copy Order to Clipboard'
      end
      object SendOrders1: TMenuItem
        Action = MailOrderAction
      end
      object N7: TMenuItem
        Caption = '-'
      end
      object itmExportRuleset: TMenuItem
        Action = ExportRulesetAction
      end
      object ExportMapItm: TMenuItem
        Action = ExportMapAction
      end
      object Addcurrentregion1: TMenuItem
        Action = AddExportAction
      end
      object Importmap1: TMenuItem
        Action = ImportMapAction
      end
      object Skills1: TMenuItem
        Caption = 'Export/Import skills'
        object ExportskillstoClipboard1: TMenuItem
          Action = ExportSkillsAction
        end
        object ExportallmagestoClipboard1: TMenuItem
          Action = ExportMagesAction
        end
        object ImportfromClipboard1: TMenuItem
          Action = ImportSkillsAction
        end
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object Options1: TMenuItem
        Action = OptionsAction
        Caption = 'Options...'
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = 'Exit'
        OnClick = Exit1Click
      end
    end
    object View1: TMenuItem
      Caption = 'View'
      object Toolbars1: TMenuItem
        Caption = 'Controls'
        object ShowToolbarItm: TMenuItem
          Caption = 'Main toolbar'
          ShortCut = 49236
          OnClick = ShowToolbarItmClick
        end
        object Maptools1: TMenuItem
          Caption = 'Map tools'
          ShortCut = 49229
          OnClick = Maptools1Click
        end
        object itmUnitTools: TMenuItem
          Caption = 'Unit tools'
          OnClick = itmUnitToolsClick
        end
        object Infopanel1: TMenuItem
          Caption = 'Info panel'
          ShortCut = 49225
          OnClick = Infopanel1Click
        end
      end
      object Showunmodifieditemamounts1: TMenuItem
        Action = UnmodItemsAction
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object PreviousTurn1: TMenuItem
        Action = PrevTurnAction
      end
      object NextTurn1: TMenuItem
        Action = NextTurnAction
      end
      object TurnsItm: TMenuItem
        Caption = 'Turns'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object GridModeAction1: TMenuItem
        Action = GridModeAction
      end
      object UnitFilter1: TMenuItem
        Action = UnitFilterAction
      end
      object FindUnit1: TMenuItem
        Action = FindUnitAction
      end
      object Showarrivingunits1: TMenuItem
        Action = ArrivingUnitsAction
      end
      object NextError1: TMenuItem
        Action = NextErrorAction
      end
      object N20: TMenuItem
        Caption = '-'
      end
      object Factions1: TMenuItem
        Action = FactionAction
      end
      object urnEvents1: TMenuItem
        Action = TurnEventsAction
      end
      object PreprocessorErrors1: TMenuItem
        Action = AdvisorWarnAction
      end
      object Orders1: TMenuItem
        Action = OrdersAction
      end
      object Avatars1: TMenuItem
        Action = AvatarsAction
      end
      object Armies1: TMenuItem
        Action = ForcesAction
      end
      object Battles1: TMenuItem
        Action = BattlesAction
        ShortCut = 114
      end
      object ItemsinRegion1: TMenuItem
        Action = AllItemsAction
      end
      object RegionReport1: TMenuItem
        Action = RegionRepAction
      end
    end
    object Map1: TMenuItem
      Caption = 'Map'
      object MiniMap2: TMenuItem
        Caption = 'Mini Map'
        object itmMiniMap: TMenuItem
          Action = MiniMapAction
          Caption = 'Toggle'
        end
        object N21: TMenuItem
          Caption = '-'
        end
        object itmMiniGeo: TMenuItem
          Caption = 'Geographical'
          RadioItem = True
          OnClick = MiniMapModeClick
        end
        object itmMiniPolitical: TMenuItem
          Caption = 'Political'
          RadioItem = True
          OnClick = MiniMapModeClick
        end
        object itmMiniVisible: TMenuItem
          Caption = 'Visible Regions'
          RadioItem = True
          OnClick = MiniMapModeClick
        end
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object ListItm: TMenuItem
        Caption = 'Region List'
        ImageIndex = 9
        object ListEnableItm: TMenuItem
          Caption = 'Enabled'
          ShortCut = 16460
          OnClick = MapToolEnableClick
        end
        object N11: TMenuItem
          Caption = '-'
        end
        object WantedListItm: TMenuItem
          Caption = '&Wanted'
          Checked = True
          RadioItem = True
          OnClick = GroupItmClick
        end
        object ForSaleListItm: TMenuItem
          Caption = 'For &Sale'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object ProductsListItm: TMenuItem
          Caption = '&Products'
          RadioItem = True
          OnClick = GroupItmClick
        end
      end
      object FogItm: TMenuItem
        Caption = 'Fog Type'
        ImageIndex = 10
        object FogEnableItm: TMenuItem
          Tag = 1
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N12: TMenuItem
          Caption = '-'
        end
        object FogVisibleItm: TMenuItem
          Tag = 1
          Caption = '&Visible Regions'
          Checked = True
          RadioItem = True
          OnClick = GroupItmClick
        end
        object FogVisitedItm: TMenuItem
          Tag = 1
          Caption = 'V&isited Regions'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object FogCustomItm: TMenuItem
          Tag = 1
          Caption = 'Custom'
          RadioItem = True
          OnClick = GroupItmClick
        end
      end
      object FlagItm: TMenuItem
        Caption = 'Factions in Region'
        ImageIndex = 12
        object FlagEnableItm: TMenuItem
          Tag = 2
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N13: TMenuItem
          Caption = '-'
        end
        object SelfFlagItm: TMenuItem
          Tag = 2
          Caption = '&Self Faction'
          OnClick = GroupItmClick
        end
        object OtherFlagsItm: TMenuItem
          Tag = 2
          Caption = '&Other Factions'
          OnClick = GroupItmClick
        end
        object OldFlagsItm: TMenuItem
          Tag = 2
          Caption = '&Previous turns'#39' info'
          OnClick = GroupItmClick
        end
        object ScoutsItm: TMenuItem
          Tag = 2
          Caption = 'Hide faction scouts'
          ShortCut = 49235
          OnClick = GroupItmClick
        end
        object ScoutingFactionsItm: TMenuItem
          Tag = 2
          Caption = '&Hide other factions'#39' scouts'
          OnClick = GroupItmClick
        end
        object N14: TMenuItem
          Caption = '-'
        end
        object OldstyleFlagsItm: TMenuItem
          Tag = 2
          Caption = '&Flags'
          Checked = True
          RadioItem = True
          OnClick = GroupItmClick
        end
        object UnitDiagramItm: TMenuItem
          Tag = 2
          Caption = '&Unit Diagram'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object MenDiagramItm: TMenuItem
          Tag = 2
          Caption = '&Men Diagram'
          RadioItem = True
          OnClick = GroupItmClick
        end
      end
      object MIcItm: TMenuItem
        Caption = 'Event Icons'
        ImageIndex = 13
        object MIcEnableItm: TMenuItem
          Tag = 3
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N15: TMenuItem
          Caption = '-'
        end
        object MIcMonstersItm: TMenuItem
          Tag = 3
          Caption = '&Monsters'
          OnClick = GroupItmClick
        end
        object MIcGatesItm: TMenuItem
          Tag = 3
          Caption = '&Gates'
          OnClick = GroupItmClick
        end
        object MIcBattlesItm: TMenuItem
          Tag = 3
          Caption = '&Battles'
          OnClick = GroupItmClick
        end
        object MIcWarningsItm: TMenuItem
          Tag = 3
          Caption = '&Warnings'
          OnClick = GroupItmClick
        end
        object MicNotesItm: TMenuItem
          Tag = 3
          Caption = 'Notes'
          OnClick = GroupItmClick
        end
      end
      object SIcItm: TMenuItem
        Caption = 'Status Icon'
        ImageIndex = 14
        object SIcEnableItm: TMenuItem
          Tag = 4
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N16: TMenuItem
          Caption = '-'
        end
        object SIcSettlItm: TMenuItem
          Tag = 4
          Caption = '&Settlement Type'
          Checked = True
          RadioItem = True
          ShortCut = 16496
          OnClick = GroupItmClick
        end
        object SIcWeatherItm: TMenuItem
          Tag = 4
          Caption = '&Weather'
          RadioItem = True
          ShortCut = 16497
          OnClick = GroupItmClick
        end
        object SIcPeasantsItm: TMenuItem
          Tag = 4
          Caption = '&Peasants'
          RadioItem = True
          ShortCut = 16498
          OnClick = GroupItmClick
        end
        object SIcGuardItm: TMenuItem
          Tag = 4
          Caption = '&Guarding Faction'
          RadioItem = True
          ShortCut = 16499
          OnClick = GroupItmClick
        end
        object SIcTaxTradeItm: TMenuItem
          Tag = 4
          Caption = 'Tax / Trade'
          RadioItem = True
          ShortCut = 16500
          OnClick = GroupItmClick
        end
        object N2: TMenuItem
          Caption = '-'
        end
        object itmHideInvisRegions: TMenuItem
          Caption = 'Hide in Invisible Regions'
          ShortCut = 16456
          OnClick = HideInvisibleClick
        end
      end
      object STxItm: TMenuItem
        Caption = 'Status Text'
        ImageIndex = 15
        object STxEnableItm: TMenuItem
          Tag = 5
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N17: TMenuItem
          Caption = '-'
        end
        object STxSettlItm: TMenuItem
          Tag = 5
          Caption = '&Settlement Name'
          Checked = True
          RadioItem = True
          OnClick = GroupItmClick
        end
        object STxPeasantsItm: TMenuItem
          Tag = 5
          Caption = '&Peasants'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object STxCountPeasantsItm: TMenuItem
          Tag = 5
          Caption = 'Peasants with &count'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object STxTaxRateItm: TMenuItem
          Tag = 5
          Caption = '&Tax Rate'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object STxWagesItm: TMenuItem
          Tag = 5
          Caption = '&Wages'
          RadioItem = True
          OnClick = GroupItmClick
        end
        object STxEntertainItm: TMenuItem
          Tag = 5
          Caption = '&Entertainment'
          RadioItem = True
          OnClick = GroupItmClick
        end
      end
      object StructItm: TMenuItem
        Caption = 'Objects'
        ImageIndex = 1
        object StructEnabledItm: TMenuItem
          Tag = 8
          Caption = 'Enabled'
          OnClick = MapToolEnableClick
        end
        object N8: TMenuItem
          Caption = '-'
        end
        object StructTradeItm: TMenuItem
          Tag = 8
          Caption = 'Trade structures'
          OnClick = GroupItmClick
        end
        object StructDefItm: TMenuItem
          Tag = 8
          Caption = 'Defance structures'
          OnClick = GroupItmClick
        end
        object StructTranspItm: TMenuItem
          Tag = 8
          Caption = 'Transports'
          OnClick = GroupItmClick
        end
        object StructClosedItm: TMenuItem
          Tag = 8
          Caption = 'Closed lairs'
          OnClick = GroupItmClick
        end
        object StructInnerItm: TMenuItem
          Tag = 8
          Caption = 'Shafts'
          OnClick = GroupItmClick
        end
        object StructRoadsItm: TMenuItem
          Tag = 8
          Caption = 'Roads'
          OnClick = GroupItmClick
        end
      end
      object LevelItm: TMenuItem
        Caption = 'Level'
        ImageIndex = 16
      end
      object Level1: TMenuItem
        Action = LevelAction
      end
      object BookmarksItm: TMenuItem
        Caption = 'Bookmarks'
      end
      object N18: TMenuItem
        Caption = '-'
      end
      object itmTerrain: TMenuItem
        Caption = 'Change terrain'
      end
      object DefineListfilter1: TMenuItem
        Action = ListFilterAction
      end
      object DefineRegionwarning1: TMenuItem
        Action = RegionWarningAction
      end
      object RegionNotesItm: TMenuItem
        Caption = 'Show region notes'
        OnClick = RegionNotesItmClick
      end
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object ItemEditor1: TMenuItem
        Action = ItemEditAction
      end
      object SkillEditor1: TMenuItem
        Action = SkillEditAction
      end
      object ObjectEditor1: TMenuItem
        Action = StructEditAction
      end
      object AbilityEditor1: TMenuItem
        Action = SpecEditAction
      end
      object SkillEditor2: TMenuItem
        Action = TerrEditAction
      end
      object N19: TMenuItem
        Caption = '-'
      end
      object axTradeRegions1: TMenuItem
        Action = TaxTradeAction
      end
      object ownTrade1: TMenuItem
        Action = TownTradeAction
      end
      object WantedItems1: TMenuItem
        Action = WantedItemsAction
      end
      object radeStructures1: TMenuItem
        Action = TradeStructAction
      end
      object RegionStats1: TMenuItem
        Action = RegionStatsAction
      end
      object SpyReports1: TMenuItem
        Action = SpyRepsAction
      end
      object UnitProduction1: TMenuItem
        Action = UnitProductionAction
      end
      object Gates1: TMenuItem
        Action = GatesAction
      end
      object ItemStats1: TMenuItem
        Action = ItemStatsAction
      end
      object SoldiersinUnit1: TMenuItem
        Action = SoldiersAction
      end
      object BattleSimulator1: TMenuItem
        Action = BattleSimAction
      end
    end
    object ScriptsItm: TMenuItem
      Caption = 'Scripts'
      object Scripts3: TMenuItem
        Action = ScriptEditAction
      end
      object StopOrderProcessing1: TMenuItem
        Action = StopProcessorAction
      end
      object ReloadScripts1: TMenuItem
        Action = ReloadScriptsAction
      end
      object N6: TMenuItem
        Caption = '-'
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object HelpContents1: TMenuItem
        Caption = 'Advisor Help'
        ShortCut = 8304
        OnClick = HelpContents1Click
      end
      object GameRulesItm: TMenuItem
        Caption = 'Game Rules'
        ShortCut = 112
        OnClick = GameRulesItmClick
      end
      object About1: TMenuItem
        Caption = 'About'
        OnClick = About1Click
      end
      object RegSplit: TMenuItem
        Caption = '-'
      end
      object RegisterItm: TMenuItem
        Caption = 'Register'
        OnClick = RegisterItmClick
      end
    end
  end
  object ActionList: TActionList
    Images = ResForm.BtnImages
    Left = 436
    Top = 80
    object ManagerAction: TAction
      Caption = 'Game Manager'
      ImageIndex = 18
      OnExecute = ManagerActionExecute
    end
    object OptionsAction: TAction
      Caption = 'Options'
      ImageIndex = 19
      OnExecute = OptionsActionExecute
    end
    object SaveOrderAction: TAction
      Caption = 'Save Order'
      ImageIndex = 20
      ShortCut = 16467
      OnExecute = SaveOrderActionExecute
    end
    object CopyOrderAction: TAction
      Caption = 'Copy Order'
      ImageIndex = 21
      OnExecute = CopyOrderActionExecute
    end
    object PrevTurnAction: TAction
      Caption = 'Previous Turn'
      ImageIndex = 22
      ShortCut = 16464
      OnExecute = PrevTurnActionExecute
    end
    object NextTurnAction: TAction
      Caption = 'Next Turn'
      ImageIndex = 23
      ShortCut = 16462
      OnExecute = NextTurnActionExecute
    end
    object MiniMapAction: TAction
      Caption = 'Mini Map'
      ImageIndex = 24
      ShortCut = 16461
      OnExecute = MiniMapActionExecute
    end
    object LevelAction: TAction
      Caption = 'Level down'
      ImageIndex = 16
      ShortCut = 16452
      OnExecute = LevelActionExecute
    end
    object ListFilterAction: TAction
      Caption = 'Define List filter'
      ImageIndex = 25
      OnExecute = ListFilterActionExecute
    end
    object BookmarkAction: TAction
      Caption = 'BookmarkAction'
      ImageIndex = 17
      OnExecute = BookmarkActionExecute
    end
    object FactionAction: TAction
      Caption = 'Factions'
      Hint = 'Faction Mode'
      ImageIndex = 8
      OnExecute = FactionActionExecute
    end
    object TurnEventsAction: TAction
      Caption = 'Turn Events'
      ImageIndex = 29
      ShortCut = 16468
      OnExecute = TurnEventsActionExecute
    end
    object OrdersAction: TAction
      Caption = 'Orders'
      ImageIndex = 30
      OnExecute = OrdersActionExecute
    end
    object StructEditAction: TAction
      Caption = 'Object Editor'
      ImageIndex = 33
      OnExecute = StructEditActionExecute
    end
    object ItemEditAction: TAction
      Caption = 'Item Editor'
      ImageIndex = 31
      OnExecute = ItemEditActionExecute
    end
    object SkillEditAction: TAction
      Caption = 'Skill Editor'
      ImageIndex = 32
      OnExecute = SkillEditActionExecute
    end
    object BattlesAction: TAction
      Caption = 'Battles in Region'
      ImageIndex = 28
      OnExecute = BattlesActionExecute
    end
    object AvatarsAction: TAction
      Caption = 'Avatars'
      ImageIndex = 35
      OnExecute = AvatarsActionExecute
    end
    object MakeAvatarAction: TAction
      Caption = 'Make unit'#39's avatar'
      ImageIndex = 36
      ShortCut = 49217
      OnExecute = MakeAvatarActionExecute
    end
    object AdvisorWarnAction: TAction
      Caption = 'Advisor Warnings'
      ImageIndex = 38
      ShortCut = 16471
      OnExecute = AdvisorWarnActionExecute
    end
    object GridModeAction: TAction
      Caption = 'Faction Mode'
      ImageIndex = 8
      ShortCut = 16454
      OnExecute = GridModeActionExecute
    end
    object MailOrderAction: TAction
      Caption = 'Send Order'
      ImageIndex = 39
      OnExecute = MailOrderActionExecute
    end
    object ScriptEditAction: TAction
      Caption = 'Script Editor'
      ImageIndex = 41
      OnExecute = ScriptEditActionExecute
    end
    object StopProcessorAction: TAction
      Caption = 'Stop Order Processing'
      ImageIndex = 42
      OnExecute = StopProcessorActionExecute
    end
    object SaveAsAction: TAction
      Caption = 'Save Order As...'
      OnExecute = SaveOrderAsClick
    end
    object ExportMapAction: TAction
      Caption = 'Export Map...'
      OnExecute = ExportMapItmClick
    end
    object RegionWarningAction: TAction
      Caption = 'Define region warning'
      OnExecute = DefineRegionwarning1Click
    end
    object AddExportAction: TAction
      Caption = 'Add Current Region'
      ShortCut = 16453
      OnExecute = AddExportActionExecute
    end
    object TownTradeAction: TAction
      Caption = 'Town Trade'
      OnExecute = TownTradeActionExecute
    end
    object SoldiersAction: TAction
      Caption = 'Soldiers in Unit'
      ImageIndex = 45
      ShortCut = 115
      OnExecute = SoldiersActionExecute
    end
    object ImportMapAction: TAction
      Caption = 'Import Map...'
      OnExecute = ImportMapActionExecute
    end
    object BattleSimAction: TAction
      Caption = 'Battle Simulator'
      Hint = 'Battle Simulator'
      ImageIndex = 28
      ShortCut = 49218
      OnExecute = BattleSimActionExecute
    end
    object SpecEditAction: TAction
      Caption = 'Ability Editor'
      Hint = 'Ability Editor'
      ImageIndex = 46
      OnExecute = SpecEditActionExecute
    end
    object TerrEditAction: TAction
      Caption = 'Terrain Editor'
      Hint = 'Terrain Editor'
      ImageIndex = 49
      OnExecute = TerrEditActionExecute
    end
    object AllItemsAction: TAction
      Caption = 'Receive Items'
      Hint = 'Receive Items'
      ImageIndex = 43
      ShortCut = 116
      OnExecute = AllItemsActionExecute
    end
    object TradeStructAction: TAction
      Caption = 'Production Chart'
      Hint = 'Trade Structures'
      ImageIndex = 1
      OnExecute = TradeStructActionExecute
    end
    object TaxTradeAction: TAction
      Caption = 'Tax / Trade Regions'
      Hint = 'Tax / Trade Regions'
      ImageIndex = 3
      OnExecute = TaxTradeActionExecute
    end
    object ExportRulesetAction: TAction
      Caption = 'Export Ruleset'
      OnExecute = ExportRulesetActionExecute
    end
    object ForcesAction: TAction
      Caption = 'Armies'
      Hint = 'Armies'
      ImageIndex = 53
      OnExecute = ForcesActionExecute
    end
    object RegionStatsAction: TAction
      Caption = 'Region Stats'
      Hint = 'Region Stats'
      ImageIndex = 17
      OnExecute = RegionStatsActionExecute
    end
    object SpyRepsAction: TAction
      Caption = 'Spy Reports'
      Hint = 'Spy Reports'
      OnExecute = SpyRepsActionExecute
    end
    object EngineRunAction: TAction
      Caption = 'Run next local turn'
      ImageIndex = 57
      OnExecute = EngineRunActionExecute
    end
    object UnitProductionAction: TAction
      Caption = 'Unit Production'
      Hint = 'Unit Production'
      ImageIndex = 7
      OnExecute = UnitProductionActionExecute
    end
    object UnitFilterAction: TAction
      Caption = 'Unit Filter'
      Hint = 'Unit Filter'
      ImageIndex = 59
      ShortCut = 16469
      OnExecute = UnitFilterActionExecute
    end
    object ReloadScriptsAction: TAction
      Caption = 'Reload Scripts'
      Hint = 'Reload Scripts'
      ImageIndex = 62
      OnExecute = ReloadScriptsActionExecute
    end
    object UnmodItemsAction: TAction
      Caption = 'Unmodified item amounts'
      OnExecute = UnmodItemsActionExecute
    end
    object RegionRepAction: TAction
      Caption = 'Region Report'
      OnExecute = RegionRepActionExecute
    end
    object GatesAction: TAction
      Caption = 'Gates'
      ImageIndex = 64
      OnExecute = GatesActionExecute
    end
    object ExportSkillsAction: TAction
      Caption = 'Export unit to Clipboard'
      ImageIndex = 65
      OnExecute = ExportSkillsActionExecute
    end
    object ImportSkillsAction: TAction
      Caption = 'Import from Clipboard'
      ImageIndex = 66
      OnExecute = ImportSkillsActionExecute
    end
    object ExportMagesAction: TAction
      Caption = 'Export all mages to Clipboard'
      OnExecute = ExportMagesActionExecute
    end
    object ArrivingUnitsAction: TAction
      Caption = 'Arriving units'
      OnExecute = ArrivingUnitsActionExecute
    end
    object ItemStatsAction: TAction
      Caption = 'Item Stats'
      OnExecute = ItemStatsActionExecute
    end
    object WantedItemsAction: TAction
      Caption = 'Wanted Items'
      OnExecute = WantedItemsActionExecute
    end
    object FindUnitAction: TAction
      Caption = 'Find Unit'
      Hint = 'Find Unit'
      ImageIndex = 0
      OnExecute = FindUnitActionExecute
    end
    object NextErrorAction: TAction
      Caption = 'Next Warning'
      Hint = 'Next Warning'
      ImageIndex = 37
      OnExecute = NextErrorActionExecute
    end
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '*.ord'
    Filter = 'Orders (*.ord)|*.ord|All files|*.*'
    Left = 336
    Top = 73
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '*.map'
    Filter = 'Advisor maps (*.map)|*.map|All files|*.*'
    Left = 368
    Top = 77
  end
  object PopMenu: TPopupMenu
    AutoHotkeys = maManual
    Images = ResForm.IconList
    Left = 400
    Top = 80
  end
end
