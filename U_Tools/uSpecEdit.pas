unit uSpecEdit;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, PowerGrid, DataStructs, Resources, uInterface, StdCtrls,
  ComCtrls, ToolWin, uGameSubs, uKeys, IntEdit, Math;

type
  TSpecEditForm = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Grid: TPowerGrid;
    cbNoBuilding: TCheckBox;
    GroupBox1: TGroupBox;
    cbItmExcept: TCheckBox;
    lbItems: TListBox;
    cmItems: TComboBox;
    ToolBar1: TToolBar;
    btnAddItem: TToolButton;
    btnDelItem: TToolButton;
    cbNoMonsters: TCheckBox;
    cbIllusions: TCheckBox;
    GroupBox2: TGroupBox;
    cbBuildingExcept: TCheckBox;
    lbBuildings: TListBox;
    cmBuildings: TComboBox;
    ToolBar2: TToolBar;
    btnAddBuilding: TToolButton;
    btnDelBuilding: TToolButton;
    GroupBox3: TGroupBox;
    cbEffExcept: TCheckBox;
    lbEffects: TListBox;
    cmEffects: TComboBox;
    ToolBar3: TToolBar;
    btnAddEffect: TToolButton;
    btnDelEffect: TToolButton;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    lbAttacks: TListBox;
    eAtkMin: TIntEdit;
    eAtkMax: TIntEdit;
    cmAtkEffect: TComboBox;
    cmAttack: TComboBox;
    cbAtkAlwaysReady: TCheckBox;
    cmAtkWeapon: TComboBox;
    GroupBox5: TGroupBox;
    lbDefences: TListBox;
    cmDefence: TComboBox;
    eDefLevel: TIntEdit;
    ToolBar5: TToolBar;
    btnAddDefence: TToolButton;
    btnDelDefence: TToolButton;
    GroupBox6: TGroupBox;
    lbShields: TListBox;
    cmShield: TComboBox;
    ToolBar4: TToolBar;
    btnAddShield: TToolButton;
    btnDelShield: TToolButton;
    mDescription: TMemo;
    EffGrid: TPowerGrid;
    cbEffOneShot: TCheckBox;
    Label2: TLabel;
    eEffAttack: TIntEdit;
    GroupBox7: TGroupBox;
    Label3: TLabel;
    eEffDef0: TIntEdit;
    eEffDef1: TIntEdit;
    Label5: TLabel;
    eEffDef2: TIntEdit;
    Label6: TLabel;
    eEffDef3: TIntEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label4: TLabel;
    eEffDef4: TIntEdit;
    eEffDef5: TIntEdit;
    eSpellText: TEdit;
    Label9: TLabel;
    cbAllShields: TCheckBox;
    ToolBar6: TToolBar;
    btnAddAttack: TToolButton;
    btnDelAttack: TToolButton;
    cbUseLev: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cmItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmBuildingsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbBuildingsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbEffectsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmEffectsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure EffGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure EffDataChange(Sender: TObject);
    procedure SpecDataChange(Sender: TObject);
    procedure btnDelAttackClick(Sender: TObject);
    procedure btnDelDefenceClick(Sender: TObject);
    procedure btnDelShieldClick(Sender: TObject);
    procedure btnDelItemClick(Sender: TObject);
    procedure btnDelBuildingClick(Sender: TObject);
    procedure btnDelEffectClick(Sender: TObject);
    procedure btnAddAttackClick(Sender: TObject);
    procedure btnAddDefenceClick(Sender: TObject);
    procedure btnAddShieldClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnAddBuildingClick(Sender: TObject);
    procedure btnAddEffectClick(Sender: TObject);
    procedure cmAtkWeaponDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmAttackDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    Filling: boolean;
    procedure FillSpecData(ARow: integer);
  public
    { Public declarations }
  end;

var
  SpecEditForm: TSpecEditForm;

implementation

{$R *.lfm}

procedure TSpecEditForm.FormCreate(Sender: TObject);
var i: integer;
begin
  Filling := True;

  // Fill grid
  Grid.Cells[0, 0] := 'Ability';
  for i := 0 to Game.SpecData.Count - 1 do begin
    Grid.Cells[0, i + 1] := Game.SpecData[i].Name;
    Grid.Rows[i + 1].Data := Game.SpecData[i];
  end;
  Grid.Fixup;

  // Effects grid
  EffGrid.Cells[0, 0] := 'Effect';
  for i := 0 to Game.EffectData.Count - 1 do begin
    EffGrid.Cells[0, i + 1] := Game.EffectData[i].Name;
    EffGrid.Rows[i + 1].Data := Game.EffectData[i];
  end;
  EffGrid.Fixup;

  // Fill combos
  FillItemDataCombo(cmItems, IT_MONSTER + IT_MOUNT, False, False);
  for i := 0 to Game.StructData.Count - 1 do
    cmBuildings.AddItem(Game.StructData[i].Group, Game.StructData[i]);
  cmAtkEffect.AddItem('', nil);
  for i := 0 to Game.EffectData.Count - 1 do begin
    cmEffects.AddItem(Game.EffectData[i].Name, Game.EffectData[i]);
    cmAtkEffect.AddItem(Game.EffectData[i].Name, Game.EffectData[i]);
  end;
  for i := 0 to atCount-1 do begin
    cmAttack.Items.Add(GetKey(s_atMelee, i));
    cmDefence.Items.Add(GetKey(s_atMelee, i));
    cmShield.Items.Add(GetKey(s_atMelee, i));
  end;
  for i := 0 to wcCount-1 do
    cmAtkWeapon.Items.Add(GetKey(s_wcSlashing, i));

  Filling := False;
end;

procedure TSpecEditForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
begin
  with TPowerGrid(Sender) do begin
    if ARow > 0 then begin
      if Name = 'EffGrid' then
        ResForm.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top, bmpEffect)
      else ResForm.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top, bmpSpecial);
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

procedure TSpecEditForm.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  FillSpecData(ARow);
end;

procedure TSpecEditForm.FillSpecData(ARow: integer);
var Sp: TSpecData;
    i: integer;
    s: string;
begin
  Filling := True;
  Sp := TSpecData(Grid.ImgRows[ARow].Data);

  mDescription.Text := Sp.Description;
  cbUseLev.Checked := Test(Sp.Flags, SP_USE_LEV);
  cbNoBuilding.Checked := Test(Sp.Flags, SP_NOBUILDING);
  cbNoMonsters.Checked := Test(Sp.Flags, HIT_NOMONSTER);
  cbIllusions.Checked := Test(Sp.Flags, HIT_ILLUSION);
  eSpellText.Text := Sp.SpellText;

  // Items
  lbItems.Clear;
  for i := 0 to Sp.Items.Count-1 do
    lbItems.AddItem(Sp.Items[i].Name(True), Sp.Items[i]);
  cbItmExcept.Checked := Test(Sp.Flags, HIT_ITEMEXCEPT);

  // Structs
  lbBuildings.Clear;
  for i := 0 to Sp.Structs.Count-1 do
    lbBuildings.AddItem(Sp.Structs[i].Group, Sp.Structs[i]);
  cbBuildingExcept.Checked := Test(Sp.Flags, HIT_BUILDINGEXCEPT);

  // Effects
  lbEffects.Clear;
  for i := 0 to Sp.Effects.Count-1 do
    lbEffects.AddItem(Sp.Effects[i].Name, Sp.Effects[i]);
  cbEffExcept.Checked := Test(Sp.Flags, HIT_EFFECTEXCEPT);

  // Attacks
  lbAttacks.Clear;
  for i := 0 to Length(Sp.Attacks) - 1 do begin
    s := IntToStr(Sp.Attacks[i].MinAmt) + ' - ' +
      IntToStr(Sp.Attacks[i].MaxAmt) + ' ' +
      GetKey(s_atMelee, Sp.Attacks[i].AttackType) + ', ' +
      GetKey(s_wcSlashing, Sp.Attacks[i].WeaponClass);
    if Test(Sp.Attacks[i].WeaponFlags, WPN_ALWAYSREADY) then
      s := s + ' 100%';
    if Sp.Attacks[i].Effect <> nil then
      s := s + ', ' + Sp.Attacks[i].Effect.Name;
    lbAttacks.Items.Add(s);
  end;

  // Defences
  lbDefences.Clear;
  for i := 0 to Length(Sp.Defences)-1 do
    lbDefences.Items.Add(GetKey(s_atMelee, Sp.Defences[i].AttackType) + ' ' +
      IntToStr(Sp.Defences[i].Bonus));

  // Shields
  lbShields.Clear;
  for i := 0 to atCount-1 do
    if Sp.Shields[i] then lbShields.Items.Add(GetKey(s_atMelee, i));
  cbAllShields.Checked := (Test(Sp.Flags, SP_SHIELD) and (lbShields.Items.Count = 0));

  Filling := False;
end;

procedure TSpecEditForm.SpecDataChange(Sender: TObject);
var Sp: TSpecData;
begin
  if Filling then Exit;
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);

  SetFlag(Sp.Flags, SP_USE_LEV, cbUseLev.Checked);
  SetFlag(Sp.Flags, SP_NOBUILDING, cbNoBuilding.Checked);
  SetFlag(Sp.Flags, HIT_NOMONSTER, cbNoMonsters.Checked);
  SetFlag(Sp.Flags, HIT_ILLUSION, cbIllusions.Checked);
  SetFlag(Sp.Flags, SP_SHIELD, (cbAllShields.Checked
    and (lbShields.Items.Count = 0)));
  SetFlag(Sp.Flags, HIT_ITEMEXCEPT, cbItmExcept.Checked);
  SetFlag(Sp.Flags, HIT_BUILDINGEXCEPT, cbBuildingExcept.Checked);
  SetFlag(Sp.Flags, HIT_EFFECTEXCEPT, cbEffExcept.Checked);
  Sp.SpellText := eSpellText.Text;
end;

procedure TSpecEditForm.cmItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TSpecEditForm.cmBuildingsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  StructComboDrawItem(Control, Index, Rect);
end;

procedure TSpecEditForm.lbItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataListDrawItem(Control, Index, Rect);
end;

procedure TSpecEditForm.lbBuildingsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  StructListDrawItem(Control, Index, Rect);
end;

procedure TSpecEditForm.lbEffectsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ListDrawItem(Control, Index, Rect, bmpEffect);
end;

procedure TSpecEditForm.cmEffectsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ComboDrawItem(Control, Index, Rect, bmpEffect);
end;

procedure TSpecEditForm.EffGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var Eff: TEffectData;
begin
  Filling := True;

  Eff := EffGrid.ImgRows[ARow].Data;
  cbEffOneShot.Checked := Test(Eff.Flags, EFF_ONESHOT);
  eEffAttack.Value := Eff.Attack;
  eEffDef0.Value := Eff.Defence[0];
  eEffDef1.Value := Eff.Defence[1];
  eEffDef2.Value := Eff.Defence[2];
  eEffDef3.Value := Eff.Defence[3];
  eEffDef4.Value := Eff.Defence[4];
  eEffDef5.Value := Eff.Defence[5];

  Filling := False;
end;

procedure TSpecEditForm.EffDataChange(Sender: TObject);
var Eff: TEffectData;
begin
  if Filling then Exit;
  Eff := EffGrid.ImgRows[EffGrid.Row].Data;

  SetFlag(Eff.Flags, EFF_ONESHOT, cbEffOneShot.Checked);
  Eff.Attack := eEffAttack.Value;
  Eff.Defence[0] := eEffDef0.Value;
  Eff.Defence[1] := eEffDef1.Value;
  Eff.Defence[2] := eEffDef2.Value;
  Eff.Defence[3] := eEffDef3.Value;
  Eff.Defence[4] := eEffDef4.Value;
  Eff.Defence[5] := eEffDef5.Value;
end;

procedure TSpecEditForm.btnDelAttackClick(Sender: TObject);
var Sp: TSpecData;
    i: integer;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbAttacks.ItemIndex >= 0 then begin
    for i := lbAttacks.ItemIndex to Length(Sp.Attacks) - 2 do
      Sp.Attacks[i] := Sp.Attacks[i+1];
    SetLength(Sp.Attacks, Length(Sp.Attacks) - 1);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnDelDefenceClick(Sender: TObject);
var Sp: TSpecData;
    i: integer;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbDefences.ItemIndex >= 0 then begin
    for i := lbDefences.ItemIndex to Length(Sp.Defences) - 2 do
      Sp.Defences[i] := Sp.Defences[i+1];
    SetLength(Sp.Defences, Length(Sp.Defences) - 1);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnDelShieldClick(Sender: TObject);
var Sp: TSpecData;
    at: integer;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbShields.ItemIndex >= 0 then begin
    SetFlag(Sp.Flags, SP_SHIELD, (lbShields.Items.Count > 1));
    at := KeyIndex(lbShields.Items[lbShields.ItemIndex], s_atMelee, atCount);
    if at >= 0 then Sp.Shields[at] := False;
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnDelItemClick(Sender: TObject);
var Sp: TSpecData;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbItems.ItemIndex >= 0 then begin
    Sp.Items.Delete(lbItems.ItemIndex);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnDelBuildingClick(Sender: TObject);
var Sp: TSpecData;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbBuildings.ItemIndex >= 0 then begin
    Sp.Structs.Delete(lbBuildings.ItemIndex);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnDelEffectClick(Sender: TObject);
var Sp: TSpecData;
begin
  Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
  if lbEffects.ItemIndex >= 0 then begin
    Sp.Effects.Delete(lbEffects.ItemIndex);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddAttackClick(Sender: TObject);
var i: integer;
    Sp: TSpecData;
begin
  if cmAttack.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    i := Length(Sp.Attacks) - 1;
    while (i >= 0) and (Sp.Attacks[i].AttackType <> cmAttack.ItemIndex) do Dec(i);
    if i = -1 then begin
      i := Length(Sp.Attacks);
      SetLength(Sp.Attacks, i + 1);
    end;
    with Sp.Attacks[i] do begin
      MinAmt := eAtkMin.Value;
      MaxAmt := eAtkMax.Value;
      AttackType := Max(0, cmAttack.ItemIndex);
      WeaponClass := Max(0, cmAtkWeapon.ItemIndex);
      if cmAtkEffect.ItemIndex >= 0 then
        Effect := TEffectData(cmAtkEffect.Items.Objects[cmAtkEffect.ItemIndex]);
      SetFlag(WeaponFlags, WPN_RANGED, True);
      SetFlag(WeaponFlags, WPN_ALWAYSREADY, cbAtkAlwaysReady.Checked);
    end;
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddDefenceClick(Sender: TObject);
var i: integer;
    Sp: TSpecData;
begin
  if cmDefence.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    i := Length(Sp.Defences) - 1;
    while (i >= 0) and (Sp.Defences[i].AttackType <> cmDefence.ItemIndex) do Dec(i);
    if i = -1 then begin
      i := Length(Sp.Defences);
      SetLength(Sp.Defences, i + 1);
    end;
    with Sp.Defences[i] do begin
      AttackType := cmDefence.ItemIndex;
      Bonus := eDefLevel.Value;
    end;
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddShieldClick(Sender: TObject);
var Sp: TSpecData;
begin
  if cmShield.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    Sp.Shields[cmShield.ItemIndex] := True;
    SetFlag(Sp.Flags, SP_SHIELD, True);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddItemClick(Sender: TObject);
var Sp: TSpecData;
begin
  if cmItems.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    if Sp.Items.IndexOf(cmItems.Items.Objects[cmItems.ItemIndex]) = -1 then
      Sp.Items.Add(cmItems.Items.Objects[cmItems.ItemIndex]);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddBuildingClick(Sender: TObject);
var Sp: TSpecData;
begin
  if cmBuildings.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    if Sp.Structs.IndexOf(cmBuildings.Items.Objects[cmBuildings.ItemIndex]) = -1 then
      Sp.Structs.Add(cmBuildings.Items.Objects[cmBuildings.ItemIndex]);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.btnAddEffectClick(Sender: TObject);
var Sp: TSpecData;
begin
  if cmEffects.ItemIndex >= 0 then begin
    Sp := TSpecData(Grid.ImgRows[Grid.Row].Data);
    if Sp.Effects.IndexOf(cmEffects.Items.Objects[cmEffects.ItemIndex]) = -1 then
      Sp.Effects.Add(cmEffects.Items.Objects[cmEffects.ItemIndex]);
    FillSpecData(Grid.Row);
  end;
end;

procedure TSpecEditForm.cmAtkWeaponDrawItem(Control: TWinControl;
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

procedure TSpecEditForm.cmAttackDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    if Index < atNonResistable then
      ResForm.Extras.Draw(Canvas, Rect.Left+1, Rect.Top + 2,
        bmp_extAttackTypes + Index);
    Rect.Left := Rect.Left + 12;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

end.
