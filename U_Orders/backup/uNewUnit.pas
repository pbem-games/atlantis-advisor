unit uNewUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, IntEdit, StdCtrls, DataStructs, ExtCtrls, uInterface, uGameSubs,
  Math, Resources, ComCtrls, ToolWin, Grids, PowerGrid, Buttons, MyStrings;

type
  TNewUnitForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    eName: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    cbBuy: TCheckBox;
    cmForSale: TComboBox;
    eForSale: TIntEdit;
    cbMainentance: TCheckBox;
    cbRecruit: TCheckBox;
    eMonths: TIntEdit;
    Label3: TLabel;
    cbStudy: TCheckBox;
    cmSkill: TComboBox;
    eSkillLv: TIntEdit;
    lNeeds: TLabel;
    eClaim: TIntEdit;
    cbClaim: TCheckBox;
    cbOnguard: TCheckBox;
    cbAutotax: TCheckBox;
    cbAvoid: TCheckBox;
    cbBehind: TCheckBox;
    cbHold: TCheckBox;
    cbNoaid: TCheckBox;
    cmConsume: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    cmReveal: TComboBox;
    Label10: TLabel;
    cmSpoils: TComboBox;
    Image1: TImage;
    cbNocross: TCheckBox;
    cbDelayTax: TCheckBox;
    cbDelayGuard: TCheckBox;
    Label4: TLabel;
    eCopy: TIntEdit;
    Label5: TLabel;
    Label7: TLabel;
    ToolBar1: TToolBar;
    btnTemplate: TToolButton;
    gFormer: TPowerGrid;
    Image2: TImage;
    lSilver: TLabel;
    btnRepStudy: TSpeedButton;
    Label6: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure cmSkillDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmForSaleChange(Sender: TObject);
    procedure eSkillLvChange(Sender: TObject);
    procedure PaymentChange(Sender: TObject);
    procedure cbAvoidClick(Sender: TObject);
    procedure cbOnguardClick(Sender: TObject);
    procedure cbBuyClick(Sender: TObject);
    procedure cbAutotaxClick(Sender: TObject);
    procedure cbDelayTaxClick(Sender: TObject);
    procedure cbDelayGuardClick(Sender: TObject);
    procedure btnTemplateClick(Sender: TObject);
    procedure gFormerDrawIcon(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Data: Pointer);
    procedure cbClaimClick(Sender: TObject);
    procedure SilverChange(Sender: TObject);
    procedure gFormerSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
  private
    Fee, Monthly, Total: integer;
    function FlagCheck(Index: integer): TCheckBox;
    procedure FillSkills;
    function SilverRow: integer;
    function GetTakeSilver: integer;
  public
    function GetOrders(Template: boolean): string;
  end;

var
  NewUnitForm: TNewUnitForm;

implementation

{$R *.dfm}

procedure TNewUnitForm.FormCreate(Sender: TObject);
var i, row: integer;
    Item: TItem;
begin
  gFormer.Cells[0, 0] := 'Take';
  gFormer.Cols[0].Format := cfNumber;
  gFormer.Cells[1, 0] := 'Of';
  gFormer.Cols[1].Format := cfNumber;
  gFormer.Cells[2, 0] := 'Forming unit''s';
  gFormer.Cols[2].Format := cfNumber;
  gFormer.Cols[0].AutoEdit := True;

  // Forming unit
  Caption := 'New Unit - formed by ' + CurrUnit.Name;
  gFormer.RowCount := 0;
  for i := 0 to CurrUnit.Items.Count-1 do begin
    Item := CurrUnit.Items[i];
    if not Item.Bought then begin
      row := gFormer.RowCount;
      gFormer.Cells[1, row] := IntToStr(Item.Amount);
      gFormer.Cells[2, row] := Item.Name;
      gFormer.SortKeys[2, row] := IntToStr(Game.ItemData.IndexOf(Item.Data));
      gFormer.Rows[row].Data := Item.Data;
    end;
  end;
  gFormer.Fixup;

  // Flags
  for i := 0 to UnitFlagsCount-1 do
    FlagCheck(i).Checked := CurrUnit.Flags[i];
  cmConsume.ItemIndex := CurrUnit.Consuming;
  cmReveal.ItemIndex := CurrUnit.Revealing;
  cmSpoils.ItemIndex := CurrUnit.Spoils;

  // Men for sale
  ItemListToCombo(CurrRegion.ForSale, cmForSale, IT_MAN, False, False);
  if cmForSale.Items.Count > 0 then begin
    cmForSale.ItemIndex := 0;
    cmForSaleChange(cmForSale);
  end
  else begin
    cmForSale.Enabled := False;
    eForSale.Enabled := False;
    cbBuy.Enabled := False;
  end;
  FillSkills;

  // Claim
  eClaim.MaxValue := Game.VirtTurn.Unclaimed;
  eClaim.Value := eClaim.MaxValue;
  if Game.VirtTurn.Unclaimed = 0 then begin
    eClaim.Enabled := False;
    cbClaim.Enabled := False;
  end;
end;

procedure TNewUnitForm.FillSkills;
var i, j: integer;
    Spec: TSkillList;
begin
  FillSkillCombo(cmSkill, SK_ALL, False, False, True);
  if not cbBuy.Checked or (cmForSale.ItemIndex = -1) then Exit;
  Spec := TItem(cmForSale.Items.Objects[cmForSale.ItemIndex]).Data.Man.SpecSkills;

  // Bring specialized skills to front
  for i := 0 to Spec.Count-1 do begin
    j := cmSkill.Items.Count - 1;
    while (j >= 0) and (TSkillData(cmSkill.Items.Objects[j]) <> Spec[i].Data) do
      Dec(j);
    if j >= 0 then cmSkill.Items.Move(j, i);
  end;
end;

function TNewUnitForm.GetOrders(Template: boolean): string;
var i, j, num, copies, men_amt, amt: integer;
    val: char;
    alias, order, new_order: string;
    Men: TItem;
    SData: TSkillData;
    Troop: TTroop;
    IData: TItemData;
begin
  // Find "new N" number
  num := 0;
  Troop := CurrUnit.Region.PlayerTroop;
  if Troop = nil then Exit;
  for i := 0 to Troop.Units.Count-1 do
    num := Min(num, Troop.Units[i].Num);
  num := Abs(num - 1);

  Result := '';
  if Template then copies := 1
  else copies := eCopy.Value;

  for j := 1 to copies do begin
    if Template then alias := '%N'
    else alias := IntToStr(num);
    new_order := 'form ' + alias + #13#10;
    if Config.ReadBool('MainWin', 'RedirNew', False) then
      new_order := new_order + '  @;new ' + alias + #13#10;
    order := '';

    // Name
    if (eName.Text <> 'Unit') and (eName.Text <> '') then
      new_order := new_order + '  name unit "' + eName.Text + '"' + #13#10;

    // Flags
    for i := 0 to UnitFlagsCount-1 do
      if Template or (FlagCheck(i).Checked <> CurrUnit.Flags[i]) then begin
        if FlagCheck(i).Checked then val := '1'
        else val := '0';
        new_order := new_order + '  ' + FlagOrders[i] + ' ' + val + #13#10;
      end;

    if cbDelayTax.Checked or cbDelayGuard.Checked then begin
      new_order := new_order + '  turn' + #13#10;
      if cbDelayTax.Checked then
        new_order := new_order + '    autotax 1' + #13#10;
      if cbDelayGuard.Checked then
        new_order := new_order + '    guard 1' + #13#10;
      new_order := new_order + '  endturn' + #13#10;
    end;

    // Ext flags
    if Template or (CurrUnit.Consuming <> cmConsume.ItemIndex) then
      new_order := new_order + '  ' + ExtFlags[0, cmConsume.ItemIndex] + #13#10;
    if Template or (CurrUnit.Revealing <> cmReveal.ItemIndex) then
      new_order := new_order + '  ' + ExtFlags[1, cmReveal.ItemIndex] + #13#10;
    if Template or (CurrUnit.Spoils <> cmSpoils.ItemIndex) then
      new_order := new_order + '  ' + ExtFlags[2, cmSpoils.ItemIndex] + #13#10;

    // Men
    if cbBuy.Checked and (cmForSale.ItemIndex >= 0) then begin
      new_order := new_order + '  buy ' + IntToStr(eForSale.Value) + ' ';
      Men := TItem(cmForSale.Items.Objects[cmForSale.ItemIndex]);
      men_amt := eForSale.Value;
      if Template and not Men.Data.Man.Leader then
        new_order := new_order + '"%PEASANTS"' + #13#10
      else new_order := new_order + '"' + Men.Data.Name(men_amt > 1) +
        '"' + #13#10;
    end;

    // Take items
    for i := 1 to gFormer.RowCount-1 do begin
      IData := TItemData(gFormer.Rows[i].Data);
      amt := Min(ToInt(gFormer.Cells[0, i]), ToInt(gFormer.Cells[1, i]));
      if Template and cbBuy.Checked and Test(IData.Flags, IT_SILVER) then begin
        order := order + 'give new ' + alias + ' %RECRUITFEE(' +
          IntToStr(eForSale.Value) + ') "' + IData.Name + '"; ' +
          eName.Text + #13#10;
        Dec(amt, Fee);
      end;
      if amt <= 0 then Continue;
      order := order + 'give new ' + alias + ' ' + IntToStr(amt) + ' "' +
        IData.Name(amt <> 1) + '"; ' + eName.Text + #13#10;
    end;

    // Claim
    if cbClaim.Checked then begin
      new_order := new_order + '  claim ' + IntToStr(eClaim.Value) + #13#10;
    end;

    // Study
    if cbStudy.Checked and (cmSkill.ItemIndex >= 0) then begin
      new_order := new_order + '  ';
      if btnRepStudy.Down then new_order := new_order + '@';
      SData := TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex]);
      new_order := new_order + 'study "' + SData.MakeName + '"' + #13#10;
    end;

    // Needs
    if (Monthly > 0) and Config.ReadBool('MainWin', 'MonthNeeds', False) then begin
      new_order := new_order + '@;needs ' + IntToStr(Monthly) + ' silver';
      if Config.ReadInteger('MainWin', 'NeedsPriority', 0) > 0 then
        new_order := new_order + ', priority ' +
          Config.ReadString('MainWin', 'NeedsPriority', '0');
      new_order := new_order + #13#10;
    end;

    Result := Result + new_order + 'end' + #13#10 + order;
    Inc(num);
  end;
end;

procedure TNewUnitForm.PaymentChange(Sender: TObject);
var i, men, maint, study, months, tax_months, tax: integer;
    m_leader, m_peasant: integer;
    Item: TItem;
    IData: TItemData;
begin
  m_peasant := GameConfig.ReadInteger('Settings', 'PeasantMaintenance', 10);
  m_leader := GameConfig.ReadInteger('Settings', 'LeaderMaintenance', 20);

  men := 0;
  Fee := 0;
  maint := 0;
  study := 0;

  // Buy men
  if cbBuy.Checked and (cmForSale.ItemIndex >= 0) then begin
    Item := TItem(cmForSale.Items.Objects[cmForSale.ItemIndex]);
    Inc(men, eForSale.Value);
    Inc(Fee, Item.Cost * men);
    if cbMainentance.Checked then begin
      if Item.Data.Man.Leader then Inc(maint, eForSale.Value * m_leader)
      else Inc(maint, eForSale.Value * m_peasant);
    end;
  end;

  // Take men
  for i := 1 to gFormer.RowCount-1 do begin
    IData := TItemData(gFormer.Rows[i].Data);
    if not Test(IData.Flags, IT_MAN) then Continue;
    Inc(men, ToInt(gFormer.Cells[0, i]));
    if cbMainentance.Checked then begin
      if IData.Man.Leader then Inc(maint, Min(ToInt(gFormer.Cells[0, i]),
        ToInt(gFormer.Cells[1, i])) * m_leader)
      else Inc(maint, ToInt(gFormer.Cells[0, i]) * m_peasant);
    end;
  end;

  // Study cost
  if cbStudy.Checked and (cmSkill.ItemIndex >= 0) then
    study := TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex]).Cost * men;

  // Months
  months := eMonths.Value;
  if cbAutotax.Checked then tax_months := months
  else if cbDelayTax.Checked then tax_months := months - 1
  else tax_months := 0;

  tax := GameConfig.ReadInteger('Settings', 'TaxIncome', 50) * men;

  // Summarize all expences
  Total := 0;
  if cbRecruit.Checked then Inc(Total, Fee);
  if cbStudy.Checked then Inc(Total, study * months);
  if cbMainentance.Checked then Inc(Total, maint * months);
  Total := Max(0, Total - Min(tax, study + maint) * tax_months);

  if months > 0 then Monthly := study + maint
  else if tax_months > 1 then Monthly := Max(0, study + maint - tax)
  else Monthly := 0;

  lNeeds.Caption := IntToStr(total);
  i := SilverRow;
  if i >= 0 then begin
    gFormer.Cells[0, i] := IntToStr(Min(total, ToInt(gFormer.Cells[1, i])));
    gFormer.Invalidate;
    SilverChange(Sender);
  end;
end;

procedure TNewUnitForm.btnTemplateClick(Sender: TObject);
var s: string;
begin
  if eName.Text = '' then begin
    MessageDlg('Unit name should be filled', mtWarning, [mbOk], 0);
    Exit;
  end;
  if Config.ReadString('FormTemplates', eName.Text, '') <> '' then begin
    if MessageDlg('Overwrite existing template?', mtConfirmation, [mbYes, mbNo],
      0) <> mrYes then Exit;
  end;

  s := GetOrders(True);
  s := StringReplace(s, '=', '', [rfReplaceAll]);
  s := StringReplace(s, #13#10, '<br>', [rfReplaceAll]);
  Config.WriteString('FormTemplates', eName.Text, s);
end;

function TNewUnitForm.FlagCheck(Index: integer): TCheckBox;
begin
  case Index of
    0: Result := cbOnguard;
    1: Result := cbAutotax;
    2: Result := cbAvoid;
    3: Result := cbBehind;
    4: Result := cbHold;
    5: Result := cbNoaid;
    else Result := cbNocross;
  end;
end;

procedure TNewUnitForm.cmSkillDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var Spec: TSkillList;
begin
  if cbBuy.Checked and (cmForSale.ItemIndex >= 0) then begin
    Spec := TItem(cmForSale.Items.Objects[cmForSale.ItemIndex]).Data.Man.SpecSkills;
    if Spec.Find(TSkillData(cmSkill.Items.Objects[Index]).Short) <> nil then
      cmSkill.Canvas.Font.Style := [fsBold];
  end;
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TNewUnitForm.cmItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemComboDrawItem(Control, Index, Rect);
end;

procedure TNewUnitForm.cmForSaleChange(Sender: TObject);
var amt: integer;
begin
  if cmForSale.ItemIndex >= 0 then begin
    amt := TItem(cmForSale.Items.Objects[cmForSale.ItemIndex]).Amount;
    if amt = -1 then amt := 9999;
    eForSale.MaxValue := amt;
    eForSale.Value := amt;
    FillSkills;
    PaymentChange(nil);
  end;
end;

procedure TNewUnitForm.eSkillLvChange(Sender: TObject);
var months, i: integer;
begin
  months := 0;
  for i := 1 to eSkillLv.Value do months := months + i;
  eMonths.Value := months;
  if months > 1 then btnRepStudy.Down := True;
end;

procedure TNewUnitForm.cbAvoidClick(Sender: TObject);
begin
  if cbAvoid.Checked then cbOnguard.Checked := False;
end;

procedure TNewUnitForm.cbOnguardClick(Sender: TObject);
begin
  if cbOnguard.Checked then begin
    cbDelayGuard.Checked := False;
    cbAvoid.Checked := False;
  end;
end;

procedure TNewUnitForm.cbBuyClick(Sender: TObject);
begin
  cbOnguard.Checked := False;
  cbAutotax.Checked := False;
  PaymentChange(Sender);
  FillSkills;
end;

procedure TNewUnitForm.cbAutotaxClick(Sender: TObject);
begin
  if cbAutotax.Checked then cbDelayTax.Checked := False;
  PaymentChange(Sender);
end;

procedure TNewUnitForm.cbDelayTaxClick(Sender: TObject);
begin
  if cbDelayTax.Checked then cbAutotax.Checked := False;
  PaymentChange(Sender);
end;

procedure TNewUnitForm.cbDelayGuardClick(Sender: TObject);
begin
  if cbDelayGuard.Checked then cbOnguard.Checked := False;
end;

procedure TNewUnitForm.gFormerDrawIcon(Sender: TObject; ACanvas: TCanvas;
  X, Y: Integer; Data: Pointer);
begin
  DrawItemIcon(ACanvas, X, Y, Data);
end;

procedure TNewUnitForm.cbClaimClick(Sender: TObject);
begin
  eClaim.Value := Max(0, StrToInt(lNeeds.Caption) - GetTakeSilver);
  SilverChange(Sender);
end;

procedure TNewUnitForm.SilverChange(Sender: TObject);
var sr, total: integer;
begin
  total := 0;
  if cbClaim.Checked then Inc(total, eClaim.Value);
  sr := SilverRow;
  if sr >= 0 then total := total + GetTakeSilver;
  lSilver.Caption := IntToStr(total);
end;

function TNewUnitForm.SilverRow: integer;
var i: integer;
begin
  i := gFormer.RowCount-1;
  while (i >= 1) and not Test(TItemData(gFormer.Rows[i].Data).Flags, IT_SILVER) do Dec(i);
  if i = 0 then Result := -1
  else Result := i;
end;

function TNewUnitForm.GetTakeSilver: integer;
var sr: integer;
begin
  sr := SilverRow;
  if sr <= 0 then Result := 0
  else Result := Min(ToInt(gFormer.Cells[0, sr]), ToInt(gFormer.Cells[1, sr]));
end;

procedure TNewUnitForm.gFormerSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if Test(TItemData(gFormer.ImgRows[ARow].Data).Flags, IT_MAN) then
    PaymentChange(Sender);
  SilverChange(Sender);
end;

end.
