object OptionForm: TOptionForm
  Left = 246
  Top = 161
  HelpContext = 2
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 436
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    487
    436)
  PixelsPerInch = 96
  TextHeight = 13
  object CancelBtn: TButton
    Left = 401
    Top = 410
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object OKBtn: TButton
    Left = 321
    Top = 410
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    TabOrder = 1
    OnClick = OKBtnClick
  end
  object OptionPages: TPageControl
    Left = 0
    Top = 0
    Width = 487
    Height = 401
    ActivePage = TabSheet1
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 2
    object TabSheet1: TTabSheet
      Caption = 'Current game'
      ImageIndex = 32
      object pcGame: TPageControl
        Left = 4
        Top = 8
        Width = 469
        Height = 361
        ActivePage = TabSheet9
        TabIndex = 1
        TabOrder = 0
        object TabSheet7: TTabSheet
          Caption = 'Details'
          object Label14: TLabel
            Left = 12
            Top = 20
            Width = 31
            Height = 13
            Caption = 'Name:'
          end
          object PasswLabel: TLabel
            Left = 12
            Top = 48
            Width = 49
            Height = 13
            Caption = 'Password:'
          end
          object Label16: TLabel
            Left = 256
            Top = 20
            Width = 100
            Height = 13
            Caption = 'Leader maintenance:'
          end
          object Label17: TLabel
            Left = 256
            Top = 44
            Width = 129
            Height = 13
            Caption = 'Common man mantenance:'
          end
          object Label19: TLabel
            Left = 256
            Top = 68
            Width = 105
            Height = 13
            Caption = 'Entertainment income:'
          end
          object Label20: TLabel
            Left = 256
            Top = 92
            Width = 58
            Height = 13
            Caption = 'Tax income:'
          end
          object Label21: TLabel
            Left = 256
            Top = 116
            Width = 102
            Height = 13
            Caption = 'Students per teacher:'
          end
          object Label22: TLabel
            Left = 256
            Top = 140
            Width = 71
            Height = 13
            Caption = 'Heals per man:'
          end
          object Label23: TLabel
            Left = 256
            Top = 216
            Width = 88
            Height = 13
            Caption = 'Army routed when:'
          end
          object Label25: TLabel
            Left = 256
            Top = 268
            Width = 92
            Height = 13
            Caption = 'Code modifications:'
          end
          object GameNameEdit: TEdit
            Left = 68
            Top = 16
            Width = 133
            Height = 21
            TabOrder = 0
            Text = 'new game'
          end
          object PasswEdit: TEdit
            Left = 68
            Top = 44
            Width = 133
            Height = 21
            TabOrder = 1
          end
          object GroupBox3: TGroupBox
            Left = 8
            Top = 76
            Width = 229
            Height = 97
            Caption = 'E-Mail'
            TabOrder = 2
            object Label11: TLabel
              Left = 12
              Top = 44
              Width = 16
              Height = 13
              Caption = 'To:'
            end
            object Label15: TLabel
              Left = 12
              Top = 68
              Width = 39
              Height = 13
              Caption = 'Subject:'
            end
            object Label12: TLabel
              Left = 12
              Top = 20
              Width = 26
              Height = 13
              Caption = 'From:'
            end
            object ServEmailEdit: TEdit
              Left = 56
              Top = 40
              Width = 133
              Height = 21
              TabOrder = 1
              Text = 'atlantis@mtu.ru'
            end
            object ServSubjEdit: TEdit
              Left = 56
              Top = 64
              Width = 89
              Height = 21
              TabOrder = 2
              Text = 'game 2'
            end
            object FromEdit: TEdit
              Left = 56
              Top = 16
              Width = 133
              Height = 21
              TabOrder = 0
              Text = 'FromEdit'
            end
            object cbNoMailDialog: TCheckBox
              Left = 152
              Top = 68
              Width = 69
              Height = 17
              Caption = 'No Dialog'
              TabOrder = 3
            end
          end
          object cbFlyingCross: TCheckBox
            Left = 256
            Top = 168
            Width = 157
            Height = 17
            Caption = 'Flying units can cross water'
            TabOrder = 3
          end
          object cbMonthTax: TCheckBox
            Left = 256
            Top = 188
            Width = 125
            Height = 17
            Caption = 'Monthlong TAX order'
            TabOrder = 4
          end
          object eLeaderMainentance: TIntEdit
            Left = 396
            Top = 16
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 5
            Value = 0
          end
          object ePeasantMainentance: TIntEdit
            Left = 396
            Top = 40
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 6
            Value = 0
          end
          object eEntertainIncome: TIntEdit
            Left = 396
            Top = 64
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 7
            Value = 0
          end
          object eTaxIncome: TIntEdit
            Left = 396
            Top = 88
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 8
            Value = 0
          end
          object eStudents: TIntEdit
            Left = 396
            Top = 112
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 9
            Value = 0
          end
          object eHeals: TIntEdit
            Left = 396
            Top = 136
            Width = 49
            Height = 22
            MaxValue = 9999
            MinValue = 0
            TabOrder = 10
            Value = 0
          end
          object cmArmyRout: TComboBox
            Left = 256
            Top = 232
            Width = 189
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 11
            Items.Strings = (
              '50% soldiers died'
              '50% of total hits lost'
              'Total of died soldiers'#39' hits is 50%')
          end
          object cmMod: TComboBox
            Left = 356
            Top = 264
            Width = 89
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 12
            OnChange = FPChange
            Items.Strings = (
              'none'
              'Tarmellion'
              'MagicDeep'
              'New Origins')
          end
        end
        object TabSheet9: TTabSheet
          Caption = 'Faction Points'
          ImageIndex = 2
          object lFpAmount: TLabel
            Left = 312
            Top = 199
            Width = 67
            Height = 13
            Caption = 'Amount of FP:'
          end
          object gPoints: TStringGrid
            Left = 16
            Top = 48
            Width = 421
            Height = 137
            ColCount = 2
            DefaultColWidth = 70
            DefaultRowHeight = 16
            RowCount = 3
            Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing, goAlwaysShowEditor]
            TabOrder = 0
          end
          object eMaxFP: TIntEdit
            Left = 384
            Top = 196
            Width = 45
            Height = 22
            MaxValue = 99
            MinValue = 1
            TabOrder = 1
            Value = 5
            OnChange = eMaxFPChange
            OnExit = eMaxFPChange
            OnKeyDown = eMaxFPKeyDown
          end
        end
        object TabSheet8: TTabSheet
          Caption = 'Map settings'
          ImageIndex = 1
          object GroupBox2: TGroupBox
            Left = 8
            Top = 10
            Width = 229
            Height = 147
            Caption = 'Map dimensions'
            TabOrder = 0
            DesignSize = (
              229
              147)
            object Label9: TLabel
              Left = 151
              Top = 88
              Width = 10
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'Y:'
            end
            object Label8: TLabel
              Left = 151
              Top = 64
              Width = 10
              Height = 13
              Anchors = [akTop, akRight]
              Caption = 'X:'
            end
            object btnDetect: TButton
              Left = 152
              Top = 116
              Width = 57
              Height = 21
              Anchors = [akTop, akRight]
              Caption = 'Detect'
              Enabled = False
              TabOrder = 1
              OnClick = btnDetectClick
            end
            object lbDimensions: TListBox
              Left = 12
              Top = 20
              Width = 125
              Height = 117
              ItemHeight = 13
              TabOrder = 0
              OnClick = lbDimensionsClick
            end
            object rbAutodetect: TRadioButton
              Left = 144
              Top = 20
              Width = 73
              Height = 17
              Anchors = [akTop, akRight]
              Caption = 'Autodetect'
              TabOrder = 2
              OnClick = rbDimClick
            end
            object rbManual: TRadioButton
              Left = 144
              Top = 36
              Width = 65
              Height = 17
              Anchors = [akTop, akRight]
              Caption = 'Manual'
              TabOrder = 3
              OnClick = rbDimClick
            end
            object eMapX: TIntEdit
              Left = 164
              Top = 60
              Width = 45
              Height = 22
              Anchors = [akTop, akRight]
              Enabled = False
              MaxValue = 9999
              MinValue = 0
              TabOrder = 4
              Value = 0
              OnChange = eMapDimChange
            end
            object eMapY: TIntEdit
              Left = 164
              Top = 84
              Width = 45
              Height = 22
              Anchors = [akTop, akRight]
              Enabled = False
              MaxValue = 9999
              MinValue = 0
              TabOrder = 5
              Value = 0
              OnChange = eMapDimChange
            end
          end
          object GroupBox7: TGroupBox
            Left = 8
            Top = 170
            Width = 229
            Height = 147
            Caption = 'Bad seasons'
            TabOrder = 1
            object tcWeather: TTabControl
              Left = 12
              Top = 20
              Width = 205
              Height = 117
              TabOrder = 0
              Tabs.Strings = (
                'North'
                'Tropical'
                'South')
              TabIndex = 0
              OnChange = tcWeatherChange
              object imgBadSeason: TImage
                Left = 24
                Top = 32
                Width = 10
                Height = 10
                Transparent = True
              end
              object lBadSeason: TLabel
                Left = 40
                Top = 32
                Width = 80
                Height = 13
                Caption = 'monsoon season'
              end
              object CheckBox1: TCheckBox
                Left = 16
                Top = 68
                Width = 41
                Height = 17
                Caption = 'jan'
                TabOrder = 1
                OnClick = WeatherClick
              end
              object CheckBox2: TCheckBox
                Tag = 1
                Left = 16
                Top = 84
                Width = 41
                Height = 17
                Caption = 'feb'
                TabOrder = 2
                OnClick = WeatherClick
              end
              object CheckBox3: TCheckBox
                Tag = 2
                Left = 64
                Top = 52
                Width = 41
                Height = 17
                Caption = 'mar'
                TabOrder = 3
                OnClick = WeatherClick
              end
              object CheckBox4: TCheckBox
                Tag = 11
                Left = 16
                Top = 52
                Width = 41
                Height = 17
                Caption = 'dec'
                TabOrder = 0
                OnClick = WeatherClick
              end
              object CheckBox5: TCheckBox
                Tag = 3
                Left = 64
                Top = 68
                Width = 41
                Height = 17
                Caption = 'apr'
                TabOrder = 4
                OnClick = WeatherClick
              end
              object CheckBox6: TCheckBox
                Tag = 4
                Left = 64
                Top = 84
                Width = 41
                Height = 17
                Caption = 'may'
                TabOrder = 5
                OnClick = WeatherClick
              end
              object CheckBox7: TCheckBox
                Tag = 5
                Left = 108
                Top = 52
                Width = 41
                Height = 17
                Caption = 'jun'
                TabOrder = 6
                OnClick = WeatherClick
              end
              object CheckBox8: TCheckBox
                Tag = 6
                Left = 108
                Top = 68
                Width = 41
                Height = 17
                Caption = 'jul'
                TabOrder = 7
                OnClick = WeatherClick
              end
              object CheckBox9: TCheckBox
                Tag = 7
                Left = 108
                Top = 84
                Width = 41
                Height = 17
                Caption = 'aug'
                TabOrder = 8
                OnClick = WeatherClick
              end
              object CheckBox10: TCheckBox
                Tag = 8
                Left = 152
                Top = 52
                Width = 41
                Height = 17
                Caption = 'sep'
                TabOrder = 9
                OnClick = WeatherClick
              end
              object CheckBox11: TCheckBox
                Tag = 9
                Left = 152
                Top = 68
                Width = 41
                Height = 17
                Caption = 'oct'
                TabOrder = 10
                OnClick = WeatherClick
              end
              object CheckBox12: TCheckBox
                Tag = 10
                Left = 152
                Top = 84
                Width = 41
                Height = 17
                Caption = 'nov'
                TabOrder = 11
                OnClick = WeatherClick
              end
            end
          end
          object cbWeather: TCheckBox
            Left = 256
            Top = 20
            Width = 97
            Height = 17
            Caption = 'Guess Weather'
            TabOrder = 2
          end
          object cbWinterRegions: TCheckBox
            Left = 256
            Top = 40
            Width = 125
            Height = 17
            Caption = 'Show Winter Terrains'
            TabOrder = 3
          end
        end
      end
    end
    object TabSheet4: TTabSheet
      Caption = 'Environment'
      ImageIndex = 5
      object pcEnvironment: TPageControl
        Left = 4
        Top = 8
        Width = 469
        Height = 361
        ActivePage = TabSheet5
        TabIndex = 3
        TabOrder = 0
        object TabSheet2: TTabSheet
          Caption = 'Main Window'
          object GroupBox4: TGroupBox
            Left = 4
            Top = 8
            Width = 233
            Height = 233
            Caption = 'Unit grid'
            TabOrder = 0
            object Label10: TLabel
              Left = 16
              Top = 20
              Width = 93
              Height = 13
              Caption = 'Custom string items:'
            end
            object Label1: TLabel
              Left = 128
              Top = 20
              Width = 27
              Height = 13
              Caption = 'Skills:'
            end
            object LongITCheck: TCheckBox
              Left = 12
              Top = 160
              Width = 97
              Height = 17
              Caption = 'Long item lists'
              TabOrder = 0
            end
            object clCustomStr: TCheckListBox
              Left = 12
              Top = 36
              Width = 101
              Height = 117
              ItemHeight = 13
              Items.Strings = (
                'Unit name'
                'Faction name'
                'Flags'
                'Items'
                'Skills'
                'Combat spell'
                'Can study'
                'Description')
              TabOrder = 1
            end
            object cbNameAsLocal: TCheckBox
              Left = 12
              Top = 180
              Width = 213
              Height = 17
              Caption = 'Show Name if no Local Description given'
              TabOrder = 2
            end
            object cmSkillColumn: TComboBox
              Left = 124
              Top = 36
              Width = 97
              Height = 21
              Style = csDropDownList
              ItemHeight = 13
              TabOrder = 3
              Items.Strings = (
                'Full name'
                'Short name'
                'First letter')
            end
            object cbArmyColors: TCheckBox
              Left = 12
              Top = 200
              Width = 105
              Height = 17
              Caption = 'Show army colors'
              TabOrder = 4
            end
          end
          object GroupBox5: TGroupBox
            Left = 4
            Top = 244
            Width = 233
            Height = 85
            Caption = 'Ask confirmation when'
            TabOrder = 1
            object SaveConfCheck: TCheckBox
              Left = 12
              Top = 24
              Width = 169
              Height = 17
              Caption = 'Saving configuration before quit'
              TabOrder = 0
            end
            object cbUnsavedOrders: TCheckBox
              Left = 12
              Top = 44
              Width = 149
              Height = 17
              Caption = 'Quit with unsaved orders'
              TabOrder = 1
            end
          end
          object GroupBox6: TGroupBox
            Left = 244
            Top = 8
            Width = 213
            Height = 233
            Caption = 'Misc'
            TabOrder = 2
            object Label18: TLabel
              Left = 12
              Top = 204
              Width = 146
              Height = 13
              Caption = 'Custom Fog: regions older than'
            end
            object cbCompactOrders: TCheckBox
              Left = 12
              Top = 76
              Width = 189
              Height = 17
              Caption = 'Compact orders before save/send'
              TabOrder = 0
            end
            object cbOverwriteMonthlong: TCheckBox
              Left = 12
              Top = 116
              Width = 161
              Height = 17
              Caption = 'Overwrite month-long orders'
              TabOrder = 1
            end
            object cbTurnEvents: TCheckBox
              Left = 12
              Top = 56
              Width = 189
              Height = 17
              Caption = 'Show Turn Events on game startup'
              TabOrder = 2
            end
            object cbBattleImages: TCheckBox
              Left = 12
              Top = 36
              Width = 177
              Height = 17
              Caption = 'Show Battle Images as Avatars'
              TabOrder = 3
            end
            object cbMonthNeeds: TCheckBox
              Left = 12
              Top = 136
              Width = 177
              Height = 17
              Caption = 'Auto-set Needs with priority'
              TabOrder = 4
            end
            object eNeedsPriority: TIntEdit
              Left = 164
              Top = 133
              Width = 41
              Height = 22
              MaxValue = 99
              MinValue = 0
              TabOrder = 5
              Value = 0
            end
            object cbRedirNew: TCheckBox
              Left = 12
              Top = 156
              Width = 149
              Height = 17
              Caption = 'Redirect "new" orders'
              TabOrder = 6
            end
            object cbCompactRepeating: TCheckBox
              Left = 32
              Top = 96
              Width = 165
              Height = 17
              Caption = 'Also compact repeating orders'
              TabOrder = 7
            end
            object eCustomFog: TIntEdit
              Left = 164
              Top = 200
              Width = 41
              Height = 22
              MaxValue = 0
              MinValue = 0
              TabOrder = 8
              Value = 0
            end
            object cbTurnInTitle: TCheckBox
              Left = 12
              Top = 16
              Width = 141
              Height = 17
              Caption = 'Show turn number in title'
              TabOrder = 9
            end
          end
          object AttBox: TGroupBox
            Left = 244
            Top = 244
            Width = 213
            Height = 85
            Caption = 'Default faction colors'
            TabOrder = 3
            object Label3: TLabel
              Left = 8
              Top = 24
              Width = 32
              Height = 13
              Caption = 'Hostile'
            end
            object Label4: TLabel
              Left = 48
              Top = 24
              Width = 47
              Height = 13
              Caption = 'Unfriendly'
            end
            object Label5: TLabel
              Left = 100
              Top = 24
              Width = 34
              Height = 13
              Caption = 'Neutral'
            end
            object Label6: TLabel
              Left = 140
              Top = 24
              Width = 36
              Height = 13
              Caption = 'Friendly'
            end
            object Label7: TLabel
              Left = 184
              Top = 24
              Width = 16
              Height = 13
              Caption = 'Ally'
            end
            object ColorBtn1: TColorBtn
              Tag = 1
              Left = 12
              Top = 40
              Width = 34
              Height = 17
              Color = clRed
              ColorDialog = ColorDialog
              Transparency = False
            end
            object ColorBtn2: TColorBtn
              Tag = 2
              Left = 52
              Top = 40
              Width = 34
              Height = 17
              Color = 8421631
              ColorDialog = ColorDialog
              Transparency = False
            end
            object ColorBtn3: TColorBtn
              Tag = 3
              Left = 92
              Top = 40
              Width = 34
              Height = 17
              Color = clWhite
              ColorDialog = ColorDialog
              Transparency = False
            end
            object ColorBtn4: TColorBtn
              Tag = 4
              Left = 132
              Top = 40
              Width = 34
              Height = 17
              Color = 8454016
              ColorDialog = ColorDialog
              Transparency = False
            end
            object ColorBtn5: TColorBtn
              Tag = 5
              Left = 172
              Top = 40
              Width = 34
              Height = 17
              Color = clLime
              ColorDialog = ColorDialog
              Transparency = False
            end
          end
        end
        object TabSheet3: TTabSheet
          Caption = 'Bookmarks'
          ImageIndex = 1
          object lbBookmarks: TListBox
            Left = 8
            Top = 16
            Width = 213
            Height = 309
            ItemHeight = 13
            TabOrder = 0
          end
          object btnRemove: TButton
            Left = 236
            Top = 16
            Width = 77
            Height = 25
            Caption = 'Remove'
            TabOrder = 1
            OnClick = btnRemoveClick
          end
        end
        object TabSheet6: TTabSheet
          Caption = 'FORM Templates'
          ImageIndex = 3
          object mFormTemplate: TMemo
            Left = 8
            Top = 16
            Width = 213
            Height = 309
            TabOrder = 0
            OnExit = mFormTemplateExit
          end
          object cmFormTemplate: TComboBox
            Left = 232
            Top = 16
            Width = 221
            Height = 21
            Style = csDropDownList
            ItemHeight = 0
            TabOrder = 1
            OnChange = cmFormTemplateChange
          end
          object btnRemoveTemplate: TButton
            Left = 232
            Top = 48
            Width = 75
            Height = 25
            Caption = 'Remove'
            TabOrder = 2
            OnClick = btnRemoveTemplateClick
          end
        end
        object TabSheet5: TTabSheet
          Caption = 'Shortcuts'
          ImageIndex = 2
          object lMessage: TLabel
            Left = 208
            Top = 280
            Width = 196
            Height = 13
            Caption = 'Select an item and press key combination'
          end
          object Label2: TLabel
            Left = 208
            Top = 296
            Width = 239
            Height = 13
            Caption = 'Hold Shift to add @ to one-click orders (bold items)'
          end
          object Label24: TLabel
            Left = 208
            Top = 312
            Width = 217
            Height = 13
            Caption = 'Numpad keys will work right for unit menu only'
          end
          object gShortcuts: TPowerGrid
            Left = 8
            Top = 16
            Width = 441
            Height = 257
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
            OnKeyDown = gShortcutsKeyDown
            ColWidths = (
              335
              102)
          end
          object btnClear: TButton
            Left = 12
            Top = 292
            Width = 77
            Height = 25
            Caption = 'Clear'
            TabOrder = 1
            OnClick = btnClearClick
          end
        end
      end
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Left = 4
    Top = 404
  end
end
