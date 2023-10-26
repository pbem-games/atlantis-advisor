unit uItemEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, PowerGrid, Resources, ComCtrls, StdCtrls, DataStructs,
  Math, IntEdit, Buttons, uGameSubs, ToolWin, uInterface, ExtCtrls,
  MyStrings;

const
  ItemGridCols = 4;
  ItemGridHeaders: array[0..ItemGridCols-1] of string = ('Type', 'Code',
    'Name', 'Weight');
  ItemGridFormats: array[0..ItemGridCols-1] of TColFormat = (cfNumber, cfString,
    cfString, cfNumber);
  TypeIcons: array[0..10] of integer = (bmpUnknownItem, bmpMen, bmpMonsters,
    bmpWeapon, bmpArmor, bmpMounts, bmpWagons, bmpSilver,
    bmpFood, bmpTradeGoods, bmpShip);

type
  TItemEditForm = class(TForm)
    ItemGrid: TPowerGrid;
    ToolBar1: TToolBar;
    btnNoFilter: TToolButton;
    btnFilter: TToolButton;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Label3: TLabel;
    cmType: TComboBox;
    Label9: TLabel;
    CodeLabel: TLabel;
    cbTool: TCheckBox;
    cbCantgive: TCheckBox;
    cbAdvanced: TCheckBox;
    Label4: TLabel;
    WeightEdit: TIntEdit;
    PControl: TPageControl;
    tsMan: TTabSheet;
    cbLeader: TCheckBox;
    Label12: TLabel;
    cmSpecSkills: TComboBox;
    tsWeapon: TTabSheet;
    tsArmor: TTabSheet;
    tsHorse: TTabSheet;
    tsProduction: TTabSheet;
    tsMovement: TTabSheet;
    eWalk: TIntEdit;
    eRide: TIntEdit;
    eFly: TIntEdit;
    eSwim: TIntEdit;
    Label8: TLabel;
    cmMaterial: TComboBox;
    mDescription: TRichEdit;
    GroupBox2: TGroupBox;
    eMagDefLevel: TIntEdit;
    lMagDefLevel: TLabel;
    Label10: TLabel;
    eDefLevel: TIntEdit;
    cmWpnSkill1: TComboBox;
    cmWpnSkill2: TComboBox;
    eAttBonus: TIntEdit;
    eDefBonus: TIntEdit;
    cbRanged: TCheckBox;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    cbUseInAss: TCheckBox;
    cmHorseSkill: TComboBox;
    eHorseMin: TIntEdit;
    eHorseMax: TIntEdit;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    cbWalk: TCheckBox;
    cbRide: TCheckBox;
    cbFly: TCheckBox;
    cbSwim: TCheckBox;
    cmSkill: TComboBox;
    eSkillLv: TIntEdit;
    eProdRate: TIntEdit;
    eProdManMonths: TIntEdit;
    Label2: TLabel;
    tsMagProduction: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cmMProdSpell: TComboBox;
    Label25: TLabel;
    eMProdSpell: TIntEdit;
    Label26: TLabel;
    cmMagMaterial: TComboBox;
    eHampBonus: TIntEdit;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    eMountBonus: TIntEdit;
    cbLong: TCheckBox;
    cbShort: TCheckBox;
    cbNoFoot: TCheckBox;
    cbNoMount: TCheckBox;
    cbNoAttDefence: TCheckBox;
    cbAttRidingBonus: TCheckBox;
    cbDefRidingBonus: TCheckBox;
    Label32: TLabel;
    Label14: TLabel;
    Label33: TLabel;
    eNumAttacks: TIntEdit;
    eNumRounds: TIntEdit;
    cmWpnClass: TComboBox;
    cbNumPlusSkill: TCheckBox;
    cbNumPlusHalf: TCheckBox;
    GroupBox3: TGroupBox;
    Label19: TLabel;
    eArmPiercing: TIntEdit;
    Label17: TLabel;
    eArmSlashing: TIntEdit;
    eArmCrushing: TIntEdit;
    Label18: TLabel;
    eArmCleaving: TIntEdit;
    Label20: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    eArmAPiercing: TIntEdit;
    eArmEnergy: TIntEdit;
    eArmSpirit: TIntEdit;
    eArmWeather: TIntEdit;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    tsMonster: TTabSheet;
    Label47: TLabel;
    eMonAtk: TIntEdit;
    eMonTactics: TIntEdit;
    eMonNumAtk: TIntEdit;
    eMonHits: TIntEdit;
    eMonRegen: TIntEdit;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    GroupBox4: TGroupBox;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    eMonMelee: TIntEdit;
    eMonEnergy: TIntEdit;
    eMonSpirit: TIntEdit;
    eMonWeather: TIntEdit;
    eMonRiding: TIntEdit;
    eMonRanged: TIntEdit;
    cbResource: TCheckBox;
    tsMagic: TTabSheet;
    cbMagic: TCheckBox;
    cmWpnAttack: TComboBox;
    cbMageOnly: TCheckBox;
    ToolBar2: TToolBar;
    btnAddMRaw: TToolButton;
    btnDelMRaw: TToolButton;
    eMonObservation: TIntEdit;
    eMonStealth: TIntEdit;
    Label27: TLabel;
    Label28: TLabel;
    ToolBar3: TToolBar;
    btnProdAdd: TToolButton;
    btnProdDel: TToolButton;
    cmAbility: TComboBox;
    eAbilityLv: TIntEdit;
    Label59: TLabel;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Image14: TImage;
    tsWagon: TTabSheet;
    cmWagonHitch: TComboBox;
    eHitchWalk: TIntEdit;
    Label34: TLabel;
    Label58: TLabel;
    cbAnyOf: TCheckBox;
    gManSkills: TPowerGrid;
    ToolBar5: TToolBar;
    btnAddSpecSkill: TToolButton;
    btnDelSpecSkill: TToolButton;
    Label11: TLabel;
    cmProdTool: TComboBox;
    eProdToolBonus: TIntEdit;
    gMaterials: TPowerGrid;
    Label1: TLabel;
    gMagMaterials: TPowerGrid;
    Label24: TLabel;
    cmAlignment: TComboBox;
    tsByproducts: TTabSheet;
    Label60: TLabel;
    gByproducts: TPowerGrid;
    cmByproduct: TComboBox;
    ToolBar4: TToolBar;
    btnAddByprod: TToolButton;
    btnDelByprod: TToolButton;
    Label61: TLabel;
    cmRequiredStruct: TComboBox;
    btnArrange: TToolButton;
    ToolButton2: TToolButton;
    btnRequest: TToolButton;
    btnRequestAll: TToolButton;
    ToolButton4: TToolButton;
    tsUpkeep: TTabSheet;
    Label62: TLabel;
    tsFood: TTabSheet;
    Label63: TLabel;
    eUpkeepSilver: TIntEdit;
    eFoodValue: TIntEdit;
    eFoodOrder: TIntEdit;
    Label64: TLabel;
    procedure cmTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormCreate(Sender: TObject);
    procedure ItemGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DataChange(Sender: TObject);
    procedure cmIDataDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PControlChange(Sender: TObject);
    procedure cmSkillDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnDelSpecSkillClick(Sender: TObject);
    procedure btnAddSpecSkillClick(Sender: TObject);
    procedure btnFilterClick(Sender: TObject);
    procedure btnProdAddClick(Sender: TObject);
    procedure btnProdDelClick(Sender: TObject);
    procedure btnAddMRawClick(Sender: TObject);
    procedure btnDelMRawClick(Sender: TObject);
    procedure btnRequestClick(Sender: TObject);
    procedure cmAbilityDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ItemGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ItemGridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure ItemGridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure cmWpnClassDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmWpnAttackDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnRequestAllClick(Sender: TObject);
    procedure DrawIDataIcon(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Data: Pointer);
    procedure gMaterialsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure gMagMaterialsSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: String);
    procedure gManSkillsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure cmRequiredStructDrawItem(Control: TWinControl;
      Index: Integer; Rect: TRect; State: TOwnerDrawState);
    procedure btnDelByprodClick(Sender: TObject);
    procedure btnAddByprodClick(Sender: TObject);
    procedure btnArrangeClick(Sender: TObject);
    procedure gByproductsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    Filling, ItemModified: boolean;
    Filter: integer;
    procedure FillBoxes;
    procedure FillItemGrid;
    function Selected: TItemData;
  public
    Modified: boolean;
    procedure SelectItem(AItemData: TItemData);
  end;

var
  ItemEditForm: TItemEditForm;

implementation

{$R *.DFM}

function Explored(Data: TItemData): boolean;
begin
  Result := (Data.Description <> '');
end;

procedure TItemEditForm.FormCreate(Sender: TObject);
var tarm: boolean;
begin
  Filter := -1;
  Filling := TRUE;

  PControl.Enabled := False;
  ItemGrid.LoadColumns(Config);
  gMaterials.Cols[0].AutoEdit := True;
  gMagMaterials.Cols[0].AutoEdit := True;
  gManSkills.Cols[1].AutoEdit := True;
  gByproducts.Cols[0].AutoEdit := True;

  // Hide Tarmellion-specific controls
  tarm := (GameConfig.ReadInteger('Settings', 'Mod', modStandard) = modTarmellion);
  lMagDefLevel.Visible := tarm;
  eMagDefLevel.Visible := tarm;
  cmAlignment.Visible := tarm;

  FillSpecCombo(cmAbility, True);

  // Fill skills
  FillSkillCombo(cmSkill, SK_MAGIC, True, True, False);
  FillSkillCombo(cmSpecSkills, SK_ALL, False, False, False);
  FillSkillCombo(cmMProdSpell, SK_ALL - SK_MAGIC - SK_CAST - SK_COMBATSPELL,
    True, True, False);
  FillSkillCombo(cmHorseSkill, SK_MAGIC, True, True, False);
  FillSkillCombo(cmWpnSkill1, SK_MAGIC, True, True, False);
  FillSkillCombo(cmWpnSkill2, SK_MAGIC, True, True, False);

  FillStructDataCombo(cmRequiredStruct, ST_TRANSPORT + ST_CLOSED, True, True);

  FillBoxes;
  FillItemGrid;

  Filling := FALSE;
end;

procedure TItemEditForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  ItemGrid.SaveColumns(Config);
end;

procedure TItemEditForm.SelectItem(AItemData: TItemData);
var i: integer;
begin
  ItemGrid.Filters.Clear;
  ItemGrid.Fixup;
  for i := 0 to ItemGrid.ImgRowCount-1 do
    if ItemGrid.ImgRows[i].Data = AItemData then ItemGrid.Row := i;
end;

procedure TItemEditForm.FillBoxes;
begin
  FillResourceCombo(cmMaterial, False);
  FillItemDataCombo(cmProdTool, IT_TOOL, False, True, False);
  FillItemDataCombo(cmByproduct, IT_MAN + IT_MONSTER, True, False);
  FillItemDataCombo(cmMagMaterial, IT_MAN + IT_MONSTER, True, False);
  FillItemDataCombo(cmWagonHitch, IT_MOUNT, False, True, False);
end;

procedure TItemEditForm.FillItemGrid;
var i, row: integer;
begin
  ItemGrid.RowCount := 0;
  row := 1;
  for i := 0 to Game.ItemData.Count-1 do begin
    if (Filter >= 0) and (ItemIconIndex(Game.ItemData[i]) <> Filter) then
      Continue;
    // Fill item grid
    ItemGrid.Cells[0, row] := IntToStr(i);
    ItemGrid.SortKeys[1, row] := IntToStr(ItemIconIndex(Game.ItemData[i]));
    ItemGrid.Cells[2, row] := Game.ItemData[i].Short;
    ItemGrid.Cells[3, row] := Game.ItemData[i].Name(False);
    ItemGrid.Cells[4, row] := IntToStr(Game.ItemData[i].Weight);
    ItemGrid.Rows[row].Data := Game.ItemData[i];
    if not Explored(Game.ItemData[i]) then
      ItemGrid.Rows[row].Color := clGrayText;
    ItemGrid.Rows[row].ImageIndex := ItemIconIndex(Game.ItemData[i]);
    Inc(row);
  end;
  ItemGrid.Fixup;
end;

procedure TItemEditForm.ItemGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
const tabcount = 13;
var i, page: integer;
    tabs: array[0..tabcount-1] of boolean;
    s: string;
begin
  with TItemData(ItemGrid.ImgRows[ARow].Data) do begin
    PControl.Enabled := True;
    btnRequest.Enabled := not Requested;
    if (CodeLabel.Caption = Short) and not ItemModified then Exit;

    Filling := TRUE;
    if CodeLabel.Caption <> Short then begin
      CodeLabel.Caption := Short;
      if Requested then s := 'Awaiting info from server'
      else s := Description;
      s := UpperCase(Copy(s, 1, 1)) + Copy(s, 2, Length(s)); 
      mDescription.Lines.Text := s;
    end;
    WeightEdit.Value := Weight;
    // Type
    cmType.ItemIndex := 0;
    for i := 0 to tabcount-1 do tabs[i] := false;
    if (Flags and IT_MAN <> 0) then begin
      cmType.ItemIndex := 1;
      tabs[tsMan.PageIndex] := True;
      tabs[tsMovement.PageIndex] := True;
      tabs[tsUpkeep.PageIndex] := True;
    end
    else if (Flags and IT_MONSTER <> 0) then begin
      cmType.ItemIndex := 2;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsMovement.PageIndex] := True;
      tabs[tsMonster.PageIndex] := True;
    end
    else if (Flags and IT_WEAPON <> 0) then begin
      cmType.ItemIndex := 3;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsWeapon.PageIndex] := True;
    end
    else if (Flags and IT_ARMOR <> 0) then begin
      cmType.ItemIndex := 4;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsArmor.PageIndex] := True;
    end
    else if (Flags and IT_MOUNT <> 0) then begin
      cmType.ItemIndex := 5;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsHorse.PageIndex] := True;
      tabs[tsMovement.PageIndex] := True;
    end
    else if (Flags and IT_WAGON <> 0) then begin
      cmType.ItemIndex := 6;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsWagon.PageIndex] := True;
    end
    else if (Flags and IT_SILVER <> 0) then begin
      cmType.ItemIndex := 7;
      tabs[tsFood.PageIndex] := True;
    end
    else if (Flags and IT_FOOD <> 0) then begin
      cmType.ItemIndex := 8;
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsMovement.PageIndex] := True;
      tabs[tsFood.PageIndex] := True;
    end
    else if (Flags and IT_TRADE <> 0) then begin
      cmType.ItemIndex := 9;
    end
    else if (Flags and IT_SHIP <> 0) then begin
      cmType.ItemIndex := 10;
    end
    else begin
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      tabs[tsMovement.PageIndex] := True;
    end;

    tabs[tsByproducts.PageIndex] := (GameConfig.ReadInteger('Settings', 'Mod',
      modStandard) = modTarmellion) and tabs[tsProduction.PageIndex];

    if (Flags and IT_MAGIC <> 0) then begin
      tabs[tsProduction.PageIndex] := True;
      tabs[tsMagProduction.PageIndex] := True;
      if Flags = IT_MAGIC then tabs[tsMovement.PageIndex] := True;
      tabs[tsMagic.PageIndex] := True;
    end;

    for i := 0 to tabcount-1 do
      PControl.Pages[i].TabVisible := tabs[i];

    // Page
    page := Config.ReadInteger('ItemEdit', 'Page', 0);
    if tabs[page] then PControl.ActivePageIndex := page
    else begin
      i := 0;
      while (i < tabcount) and not tabs[i] do Inc(i);
      if i < tabcount then PControl.ActivePageIndex := i
    end;

    cmAbility.ItemIndex := cmAbility.Items.IndexOfObject(Special);
    eAbilityLv.Value := SpecLevel;

    // Flags
    cbTool.Checked := Test(Flags, IT_TOOL);
    cbResource.Checked := Test(Flags, IT_RESOURCE);
    cbCantgive.Checked := Test(Flags, IT_CANTGIVE);
    cbAdvanced.Checked := Test(Flags, IT_ADVANCED);
    cbMagic.Checked := Test(Flags, IT_MAGIC);
    cbAnyOf.Checked := Test(Flags, IT_ORMATERIALS);

    if Test(Flags, IT_GOOD) then cmAlignment.ItemIndex := alGood
    else if Test(Flags, IT_EVIL) then cmAlignment.ItemIndex := alEvil
    else cmAlignment.ItemIndex := alNeutral;

    // Production
    with Produce do begin
      if Skill <> nil then begin
        cmSkill.ItemIndex := cmSkill.Items.IndexOfObject(Skill.Data);
        eSkillLv.Value := Skill.Level;
      end
      else begin
        cmSkill.ItemIndex := 0;
        eSkillLv.Value := 0;
      end;
      // Rate
      eProdRate.Value := Rate;
      eProdManMonths.Value := ManMonths;
      // Tool
      cmProdTool.ItemIndex := cmProdTool.Items.IndexOfObject(Tool);
      eProdToolBonus.Value := ToolBonus;
      // Materials
      FillIDataGrid(gMaterials, Materials);
      // Byproducts
      FillIDataGrid(gByproducts, Byproducts);
      // Struct
      cmRequiredStruct.ItemIndex := cmRequiredStruct.Items.IndexOfObject(RequiredStruct);
    end;

    // Magic Production
    with MagProduce do begin
      if Skill <> nil then begin
        cmMProdSpell.ItemIndex := cmMProdSpell.Items.IndexOfObject(Skill.Data);
        eMProdSpell.Value := Skill.Level;
      end
      else begin
        cmMProdSpell.ItemIndex := 0;
        eMProdSpell.Value := 0;
      end;
      // Materials
      FillIDataGrid(gMagMaterials, Materials);
    end;

    // Movement
    eWalk.Value := Moves[mtWalk] - Weight;
    cbWalk.Checked := (Moves[mtWalk] >= Weight) and (Moves[mtWalk] > 0);
    eRide.Value := Moves[mtRide] - Weight;
    cbRide.Checked := (Moves[mtRide] >= Weight) and (Moves[mtRide] > 0);
    eFly.Value := Moves[mtFly] - Weight;
    cbFly.Checked := (Moves[mtFly] >= Weight) and (Moves[mtFly] > 0);
    eSwim.Value := Moves[mtSwim] - Weight;
    cbSwim.Checked := (Moves[mtSwim] >= Weight) and (Moves[mtSwim] > 0);

    // Man
    cbLeader.Checked := Man.Leader;
    eDefLevel.Value := Man.DefLevel;
    eMagDefLevel.Value := Man.MagDefLevel;
    FillSDataGrid(gManSkills, Man.SpecSkills);

    // Monster
    with Monster do begin
      eMonAtk.Value := Attack;
      eMonTactics.Value := Tactics;
      eMonStealth.Value := Stealth;
      eMonObservation.Value := Observation;
      eMonNumAtk.Value := NumAttacks;
      eMonHits.Value := Hits;
      eMonRegen.Value := Regen;
      eMonMelee.Value := Defence[0];
      eMonEnergy.Value := Defence[1];
      eMonSpirit.Value := Defence[2];
      eMonWeather.Value := Defence[3];
      eMonRiding.Value := Defence[4];
      eMonRanged.Value := Defence[5];
    end;

    // Horse
    cmHorseSkill.ItemIndex := cmHorseSkill.Items.IndexOfObject(Mount.RideSkill);
    eHorseMin.Value := Mount.MinBonus;
    eHorseMax.Value := Mount.MaxBonus;
    eHampBonus.Value := Mount.MaxHamperedBonus;

    // Wagon
    cmWagonHitch.ItemIndex := cmWagonHitch.Items.IndexOfObject(Wagon.Hitched);
    eHitchWalk.Value := Wagon.HitchWalk - Weight;

    // Weapon
    cmWpnClass.ItemIndex := Weapon.WpnClass;
    cmWpnAttack.ItemIndex := Weapon.AttackType;
    cbRanged.Checked := Test(Weapon.Flags, WPN_RANGED);
    cbLong.Checked := Test(Weapon.Flags, WPN_LONG);
    cbShort.Checked := Test(Weapon.Flags, WPN_SHORT);
    cbNoFoot.Checked := Test(Weapon.Flags, WPN_NOFOOT);
    cbNoMount.Checked := Test(Weapon.Flags, WPN_NOMOUNT);
    cbNoAttDefence.Checked := Test(Weapon.Flags, WPN_NOATTACKERSKILL);
    cbAttRidingBonus.Checked := Test(Weapon.Flags, WPN_RIDINGBONUS);
    cbDefRidingBonus.Checked := Test(Weapon.Flags, WPN_RIDINGBONUSDEFENSE +
      WPN_RIDINGBONUS);
    cbNumPlusSkill.Checked := Test(Weapon.Flags, WPN_NUMATTSKILL);
    cbNumPlusHalf.Checked := Test(Weapon.Flags, WPN_NUMATTHALFSKILL);
    eAttBonus.Value := Weapon.AttackBonus;
    eDefBonus.Value := Weapon.DefenceBonus;
    eMountBonus.Value := Weapon.MountBonus;
    cmWpnSkill1.ItemIndex := cmWpnSkill1.Items.IndexOfObject(Weapon.Skill1);
    cmWpnSkill2.ItemIndex := cmWpnSkill2.Items.IndexOfObject(Weapon.Skill2);
    if Test(Weapon.Flags, WPN_NUMATTSKILL + WPN_NUMATTHALFSKILL) then begin
      eNumAttacks.Value := 0;
      eNumRounds.Value := 1;
      eNumAttacks.Enabled := False;
      eNumRounds.Enabled := False;
    end
    else begin
      if Weapon.NumAttacks >= 0 then begin
        eNumAttacks.Value := Weapon.NumAttacks;
        eNumRounds.Value := 1;
      end
      else begin
        eNumAttacks.Value := 1;
        eNumRounds.Value := -Weapon.NumAttacks;
      end;
      eNumAttacks.Enabled := True;
      eNumRounds.Enabled := True;
    end;

    // Armor
    with Armor do begin
      cbUseInAss.Checked := Test(Flags, ARM_USEINASS);
      eArmSlashing.Value := Defence[0];
      eArmPiercing.Value := Defence[1];
      eArmCrushing.Value := Defence[2];
      eArmCleaving.Value := Defence[3];
      eArmAPiercing.Value := Defence[4];
      eArmEnergy.Value := Defence[5];
      eArmSpirit.Value := Defence[6];
      eArmWeather.Value := Defence[7];
    end;

    // Magic
    cbMageOnly.Checked := Magic.MageOnly;

    // Upkeep
    eUpkeepSilver.Value := Upkeep.Silver;

    // Food
    eFoodValue.Value := Food.Value;
    eFoodOrder.Value := Food.Order;
  end;
  ItemModified := False;
  Filling := False;
end;

function TItemEditForm.Selected: TItemData;
begin
  if (ItemGrid.Row < 0) or (ItemGrid.Row >= ItemGrid.RowCount) then
    Result := nil
  else Result := TItemData(ItemGrid.ImgRows[ItemGrid.Row].Data);
end;

procedure TItemEditForm.DataChange(Sender: TObject);
var AItemData: TItemData;
begin
  if Filling then Exit;
  AItemData := TItemData(ItemGrid.ImgRows[ItemGrid.Row].Data);
  with AItemData do begin
    Weight := WeightEdit.Value;
    // Flags
    SetFlag(Flags, IT_MAN, cmType.ItemIndex = 1);
    SetFlag(Flags, IT_MONSTER, cmType.ItemIndex = 2);
    SetFlag(Flags, IT_MAGIC, cbMagic.Checked);
    SetFlag(Flags, IT_WEAPON, cmType.ItemIndex = 3);
    SetFlag(Flags, IT_ARMOR, cmType.ItemIndex = 4);
    SetFlag(Flags, IT_MOUNT, cmType.ItemIndex = 5);
    SetFlag(Flags, IT_WAGON, cmType.ItemIndex = 6);
    SetFlag(Flags, IT_SILVER, cmType.ItemIndex = 7);
    SetFlag(Flags, IT_FOOD, cmType.ItemIndex = 8);
    SetFlag(Flags, IT_TRADE, cmType.ItemIndex = 9); 
    SetFlag(Flags, IT_SHIP, cmType.ItemIndex = 10);
    SetFlag(Flags, IT_TOOL, cbTool.Checked);
    SetFlag(Flags, IT_RESOURCE, cbResource.Checked);
    SetFlag(Flags, IT_CANTGIVE, cbCantgive.Checked);
    SetFlag(Flags, IT_ADVANCED, cbAdvanced.Checked);
    SetFlag(Flags, IT_ORMATERIALS, cbAnyOf.Checked);

    SetFlag(Flags, IT_GOOD + IT_EVIL, False);
    if cmAlignment.ItemIndex = alGood then SetFlag(Flags, IT_GOOD);
    if cmAlignment.ItemIndex = alEvil then SetFlag(Flags, IT_EVIL);

    // Ability
    if cmAbility.ItemIndex >= 0 then begin
      Special := TSpecData(cmAbility.Items.Objects[cmAbility.ItemIndex]);
      SpecLevel := eAbilityLv.Value;
    end
    else SpecLevel := 0;

    // Production
    with Produce do begin
      Rate := eProdRate.Value;
      ManMonths := eProdManMonths.Value;
      FreeAndNil(Skill);
      if cmSkill.ItemIndex > 0 then begin
        Skill := TSkill.Create;
        Skill.Data := TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex]);
        Skill.Level := eSkillLv.Value;
      end;
      Tool := TItemData(cmProdTool.Items.Objects[cmProdTool.ItemIndex]);
      ToolBonus := eProdToolBonus.Value;
      RequiredStruct := TStructData(cmRequiredStruct.Items.Objects[cmRequiredStruct.ItemIndex]);
    end;

    // Mag Production
    with MagProduce do begin
      FreeAndNil(Skill);
      if cmMProdSpell.ItemIndex > 0 then begin
        Skill := TSkill.Create;
        Skill.Data := TSkillData(cmMProdSpell.Items.Objects[cmMProdSpell.ItemIndex]);
        Skill.Level := eMProdSpell.Value;
      end;
    end;

    // Movement
    if (cbWalk.Checked or cbRide.Checked or cbFly.Checked or cbswim.Checked)
      and (Weight = 0) then Weight := 1;
    if cbWalk.Checked then Moves[mtWalk] := eWalk.Value + Weight
    else Moves[mtWalk] := 0;
    if cbRide.Checked then Moves[mtRide] := eRide.Value + Weight
    else Moves[mtRide] := 0;
    if cbFly.Checked then Moves[mtFly] := eFly.Value + Weight
    else Moves[mtFly] := 0;
    if cbSwim.Checked then Moves[mtSwim] := eSwim.Value + Weight
    else Moves[mtSwim] := 0;

    // Man
    with Man do begin
      Leader := cbLeader.Checked;
      DefLevel := eDefLevel.Value;
      MagDefLevel := eMagDefLevel.Value;
    end;

    // Monster
    with Monster do begin
      Attack := eMonAtk.Value;
      Tactics := eMonTactics.Value;
      Stealth := eMonStealth.Value;
      Observation := eMonObservation.Value;
      NumAttacks := eMonNumAtk.Value;
      Hits := eMonHits.Value;
      Regen := eMonRegen.Value;
      Defence[0] := eMonMelee.Value;
      Defence[1] := eMonEnergy.Value;
      Defence[2] := eMonSpirit.Value;
      Defence[3] := eMonWeather.Value;
      Defence[4] := eMonRiding.Value;
      Defence[5] := eMonRanged.Value;
    end;

    // Weapon
    with Weapon do begin
      SetFlag(Flags, WPN_RANGED, cbRanged.Checked);
      SetFlag(Flags, WPN_LONG, cbLong.Checked);
      SetFlag(Flags, WPN_SHORT, cbShort.Checked);
      SetFlag(Flags, WPN_NOFOOT, cbNoFoot.Checked);
      SetFlag(Flags, WPN_NOMOUNT, cbNoMount.Checked);
      SetFlag(Flags, WPN_NOATTACKERSKILL, cbNoAttDefence.Checked);
      SetFlag(Flags, WPN_RIDINGBONUS, cbAttRidingBonus.Checked);
      SetFlag(Flags, WPN_RIDINGBONUSDEFENSE, (cbDefRidingBonus.Checked
        and not cbAttRidingBonus.Checked));
      SetFlag(Flags, WPN_NUMATTSKILL, cbNumPlusSkill.Checked);
      SetFlag(Flags, WPN_NUMATTHALFSKILL, cbNumPlusHalf.Checked);

      AttackBonus := eAttBonus.Value;
      DefenceBonus := eDefBonus.Value;
      MountBonus := eMountBonus.Value;
      if eNumAttacks.Value < eNumRounds.Value then NumAttacks := -eNumRounds.Value
      else NumAttacks := eNumAttacks.Value;
      WpnClass := cmWpnClass.ItemIndex;
      AttackType := cmWpnAttack.ItemIndex;
      Skill1 := TSkillData(cmWpnSkill1.Items.Objects[cmWpnSkill1.ItemIndex]);
      Skill2 := TSkillData(cmWpnSkill2.Items.Objects[cmWpnSkill2.ItemIndex]);
      SetFlag(Flags, WPN_NEEDSKILL, (Skill1 <> nil) or (Skill2 <> nil));
    end;

    // Armor
    with Armor do begin
      SetFlag(Flags, ARM_USEINASS, cbUseInAss.Checked);
      Defence[0] := eArmSlashing.Value;
      Defence[1] := eArmPiercing.Value;
      Defence[2] := eArmCrushing.Value;
      Defence[3] := eArmCleaving.Value;
      Defence[4] := eArmAPiercing.Value;
      Defence[5] := eArmEnergy.Value;
      Defence[6] := eArmSpirit.Value;
      Defence[7] := eArmWeather.Value;
    end;

    // Horse
    Mount.RideSkill := TSkillData(cmHorseSkill.Items.Objects[cmHorseSkill.ItemIndex]);
    Mount.MinBonus := eHorseMin.Value;
    Mount.MaxBonus := eHorseMax.Value;

    // Wagon
    Wagon.Hitched := TItemData(cmWagonHitch.Items.Objects[cmWagonHitch.ItemIndex]);
    if eHitchWalk.Value > 0 then
      Wagon.HitchWalk := eHitchWalk.Value + Weight
    else Wagon.HitchWalk := 0;

    // Magic
    Magic.MageOnly := cbMageOnly.Checked;

    // Upkeep
    Upkeep.Silver := Max(0, eUpkeepSilver.Value);

    // Food
    Food.Value := Max(0, eFoodValue.Value);
    Food.Order := Max(0, eFoodOrder.Value);

    ItemGrid.ImgRows[ItemGrid.Row].SortKey[1] := IntToStr(ItemIconIndex(AItemData));
    ItemGrid.ImgRows[ItemGrid.Row].ImageIndex := ItemIconIndex(AItemData);
    ItemGrid.ImgCells[4, ItemGrid.Row] := IntToStr(Weight);
    ItemGrid.Fixup;
  end;
  FillBoxes;
  ItemModified := True;
  Modified := True;
  SelectItem(AItemData);
end;

procedure TItemEditForm.PControlChange(Sender: TObject);
begin
  Config.WriteInteger('ItemEdit', 'Page', PControl.ActivePageIndex);
end;

procedure TItemEditForm.cmTypeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.IconList.Draw(Canvas, Rect.Left+1, Rect.Top, TypeIcons[Index]);
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TItemEditForm.cmIDataDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TItemEditForm.cmSkillDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TItemEditForm.btnFilterClick(Sender: TObject);
begin
  if TToolButton(Sender).Tag = 0 then Filter := -1
  else Filter := ItemIconIndex(Selected);
  FillItemGrid;
end;

procedure TItemEditForm.btnAddSpecSkillClick(Sender: TObject);
begin
  AddSkillFromCombo(Selected.Man.SpecSkills, cmSpecSkills);
  DataChange(Sender);
end;

procedure TItemEditForm.btnDelSpecSkillClick(Sender: TObject);
begin
  DelSkillFromGrid(Selected.Man.SpecSkills, gManSkills);
  DataChange(Sender);
end;

procedure TItemEditForm.btnProdAddClick(Sender: TObject);
begin
  AddItemFromCombo(Selected.Produce.Materials, cmMaterial);
  DataChange(Sender);
end;

procedure TItemEditForm.btnProdDelClick(Sender: TObject);
begin
  DelItemFromGrid(Selected.Produce.Materials, gMaterials);
  DataChange(Sender);
end;

procedure TItemEditForm.btnAddMRawClick(Sender: TObject);
begin
  AddItemFromCombo(Selected.MagProduce.Materials, cmMagMaterial);
  DataChange(Sender);
end;

procedure TItemEditForm.btnDelMRawClick(Sender: TObject);
begin
  DelItemFromGrid(Selected.MagProduce.Materials, gMagMaterials);
  DataChange(Sender);
end;

procedure TItemEditForm.btnAddByprodClick(Sender: TObject);
begin
  AddItemFromCombo(Selected.Produce.Byproducts, cmByproduct);
  DataChange(Sender);
end;

procedure TItemEditForm.btnDelByprodClick(Sender: TObject);
begin
  DelItemFromGrid(Selected.Produce.Byproducts, gByproducts);
  DataChange(Sender);
end;

procedure TItemEditForm.btnRequestClick(Sender: TObject);
var Data: TItemData;
begin
  Data := TItemData(ItemGrid.ImgRows[ItemGrid.Row].Data);
  FactionLeader.Orders.Add('show item "' + Data.Name(True) + '"');
  Data.Requested := True;
  btnRequest.Enabled := False;
  mDescription.Lines.Text := 'Awaiting info from server';
end;

procedure TItemEditForm.btnRequestAllClick(Sender: TObject);
var i, cnt: integer;
begin
  cnt := 0;
  for i := 0 to Game.ItemData.Count-1 do
    if not Explored(Game.ItemData[i]) and not Game.ItemData[i].Requested then begin
      FactionLeader.Orders.Add('show item "' + Game.ItemData[i].Name(True) + '"');
      Game.ItemData[i].Requested := True;
      Inc(cnt);
      if cnt > 95 then Break;
    end;
  MessageDlg('Advisor issued ' + IntToStr(cnt) + ' SHOW ITEM orders. Note ' +
    'that you shouldn''t issue more than 100 SHOW orders at one turn (including ' +
    'SHOW SKILL and SHOW OBJECT orders).', mtInformation, [mbOk], 0);
  btnRequestAll.Enabled := False;
end;

procedure TItemEditForm.cmAbilityDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ComboDrawItem(Control, Index, Rect, bmpSpecial);
end;

procedure TItemEditForm.ItemGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with TPowerGrid(Sender) do begin
    if MouseCell.Y >= FixedRows then BeginDrag(False);
  end;
end;

procedure TItemEditForm.ItemGridEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var IData1, IData2: TItemData;
begin
  if Target = Sender then with TPowerGrid(Sender) do begin
    if (MouseCell.Y >= FixedRows) and (MouseCell.Y <> Row) then begin
      IData1 := ImgRows[Row].Data;
      IData2 := ImgRows[MouseCell.Y].Data;
      Game.ItemData.Delete(Game.ItemData.IndexOf(IData1));
      Game.ItemData.Insert(Game.ItemData.IndexOf(IData2), IData1);
      FillItemGrid;
    end;
  end;
end;

procedure TItemEditForm.ItemGridDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  with TPowerGrid(Sender) do begin
    Accept := (MouseCell.Y >= FixedRows);
    if (MouseCell.Y = TopRow) and (Row <> TopRow) then
      TopRow := Max(1, TopRow - 1);
    if (MouseCell.Y = TopRow + VisibleRowCount) and (Row <> TopRow + VisibleRowCount) then
      TopRow := Min(TopRow + 1, RowCount - VisibleRowCount);
  end;
end;

procedure TItemEditForm.cmWpnClassDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.Extras.Draw(Canvas, Rect.Left+1, Rect.Top + 2,
      bmp_extWpnClasses + Index);
    Rect.Left := Rect.Left + 12;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TItemEditForm.cmWpnAttackDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.Extras.Draw(Canvas, Rect.Left+1, Rect.Top + 2,
      bmp_extAttackTypes + Index);
    Rect.Left := Rect.Left + 12;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TItemEditForm.DrawIDataIcon(Sender: TObject;
  ACanvas: TCanvas; X, Y: Integer; Data: Pointer);
begin
  DrawItemIcon(ACanvas, X, Y, Data);
end;

procedure TItemEditForm.gMaterialsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  IDataGridSetEditText(Sender, Value, Selected.Produce.Materials);
  DataChange(Sender);
end;

procedure TItemEditForm.gMagMaterialsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  IDataGridSetEditText(Sender, Value, Selected.MagProduce.Materials);
  DataChange(Sender);
end;

procedure TItemEditForm.gByproductsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  IDataGridSetEditText(Sender, Value, Selected.Produce.Byproducts);
  DataChange(Sender);
end;

procedure TItemEditForm.gManSkillsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  SDataGridSetEditText(Sender, Value, Selected.Man.SpecSkills);
  DataChange(Sender);
end;

procedure TItemEditForm.cmRequiredStructDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  StructComboDrawItem(Control, Index, Rect);
end;

procedure TItemEditForm.btnArrangeClick(Sender: TObject);
const Masks: array[0..11] of DWord = (IT_SILVER, IT_MAN, IT_WEAPON,
  IT_ARMOR, IT_TOOL, IT_MOUNT, IT_WAGON, IT_ADVANCED + IT_RESOURCE,
  IT_RESOURCE, IT_UNKNOWN, IT_TRADE, IT_MONSTER);
      UnknownPr = 9;
var i, j, pr1, pr2: integer;
    D1, D2: TItemData;
    F1, F2: DWord;
begin
  for i := 0 to Game.ItemData.Count-1 do
    for j := i+1 to Game.ItemData.Count-1 do begin
      D1 := Game.ItemData[i];
      D2 := Game.ItemData[j];
      // Priorities
      pr1 := 0;
      while pr1 < Length(Masks) do begin
        if D1.Flags <> 0 then F1 := D1.Flags else F1 := IT_UNKNOWN;
        if (F1 and Masks[pr1]) = Masks[pr1] then Break;
        Inc(pr1);
      end;
      if pr1 = Length(Masks) then pr1 := UnknownPr;
      pr2 := 0;
      while pr2 < Length(Masks) do begin
        if D2.Flags <> 0 then F2 := D2.Flags else F2 := IT_UNKNOWN;
        if (F2 and Masks[pr2]) = Masks[pr2] then Break;
        Inc(pr2);
      end;
      if pr2 = Length(Masks) then pr2 := UnknownPr;
      if pr1 > pr2 then Game.ItemData.Exchange(i, j);
      if pr1 <> pr2 then Continue;
      if Test(D1.Flags, IT_WEAPON + IT_ARMOR + IT_MOUNT) then Continue;
      // Alpha sort
      if D1.Name > D2.Name then Game.ItemData.Exchange(i, j);
    end;
  FillItemGrid;
end;


end.
