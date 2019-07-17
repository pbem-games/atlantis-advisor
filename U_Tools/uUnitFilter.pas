unit uUnitFilter;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, StdCtrls, ComCtrls, ToolWin, IntEdit,
  ExtCtrls, uInterface, uGameSubs, PowerGrid;

type
  TUnitFilterForm = class(TForm)
    btnOk: TButton;
    Button2: TButton;
    Panel1: TPanel;
    Label2: TLabel;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lbItems: TListBox;
    cmItem: TComboBox;
    ToolBar4: TToolBar;
    btnAddItem: TToolButton;
    btnDelItem: TToolButton;
    cmSkill: TComboBox;
    eSkillLv: TIntEdit;
    ToolBar3: TToolBar;
    btnAddSkill: TToolButton;
    btnDelSkill: TToolButton;
    lbSkills: TListBox;
    cbMage: TCheckBox;
    cmFaction: TComboBox;
    ToolBar1: TToolBar;
    btnMyFaction: TToolButton;
    eName: TEdit;
    eNumber: TEdit;
    cbOr: TCheckBox;
    ToolBar2: TToolBar;
    btnClear: TToolButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cmSkillDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmItemDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure lbSkillsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnMyFactionClick(Sender: TObject);
    procedure btnAddSkillClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnDelSkillClick(Sender: TObject);
    procedure lbItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnDelItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
  private
    procedure FillFilter;
  public
    { Public declarations }
  end;

var
  UnitFilterForm: TUnitFilterForm;

  procedure ClearUnitFilter;
  function UnitFilterEmpty: boolean;

implementation

{$R *.lfm}

uses Main;

procedure TUnitFilterForm.FormCreate(Sender: TObject);
begin
  FillItemDataCombo(cmItem, IT_ALL, False, False);
  FillSkillCombo(cmSkill, SK_ALL, False, False, False);
  FillFactionsCombo(cmFaction, False,
    not Config.ReadBool('MainWin', 'GridMode', False));
  cmFaction.ItemIndex := 0;
  FillFilter;
end;

procedure TUnitFilterForm.FillFilter;
var i: integer;
    Skills: TSkillList;
    Fac: TFactionData;
begin
  with MainForm do begin
    lbItems.Clear;
    for i := 0 to Filter.Items.Count-1 do
      lbItems.AddItem(Filter.Items[i].Name, Filter.Items[i]);

    for i := 0 to lbSkills.Count-1 do
      TSkill(lbSkills.Items.Objects[i]).Free;
    lbSkills.Clear;

    Skills := TSkillList.Create;
    Skills.AssignItems(Filter.Skills);
    for i := 0 to Skills.Count-1 do
      lbSkills.AddItem(Skills[i].Data.Name + ', lv ' +
        IntToStr(Skills[i].Level), Skills[i]);

    if Filter.FactionNum = -1 then
      cmFaction.ItemIndex := 0
    else begin
      i := cmFaction.Items.Count-1;
      while i > 0 do begin
        Fac := TFactionData(cmFaction.Items.Objects[i]);
        if (Fac <> nil) and (Fac.Num = Filter.FactionNum) then Break;
        Dec(i);
      end;
      cmFaction.ItemIndex := i;
    end;

    eName.Text := Filter.UnitName;
    eNumber.Text := Filter.UnitNum;

    cbMage.Checked := Filter.Mages;

    cbOr.Checked := Filter.Any;
  end;
end;

procedure TUnitFilterForm.btnOkClick(Sender: TObject);
var i: integer;
    Fac: TFactionData;
begin
  ClearUnitFilter;
  MainForm.Filter.UnitName := eName.Text;
  MainForm.Filter.UnitNum := eNumber.Text;
  // Faction
  Fac := TFactionData(cmFaction.Items.Objects[cmFaction.ItemIndex]);
  if Fac = nil then MainForm.Filter.FactionNum := -1
  else MainForm.Filter.FactionNum := Fac.Num;
  // Mages
  MainForm.Filter.Mages := cbMage.Checked;
  // Items
  for i := 0 to lbItems.Count-1 do
    MainForm.Filter.Items.Add(lbItems.Items.Objects[i]);
  // Skills
  for i := 0 to lbSkills.Count-1 do
    MainForm.Filter.Skills.Add(lbSkills.Items.Objects[i]);
  MainForm.Filter.Any := cbOr.Checked;
end;

procedure TUnitFilterForm.FormDestroy(Sender: TObject);
var i: integer;
begin
  if ModalResult <> mrOk then begin
    for i := 0 to lbSkills.Count-1 do
      TSkill(lbSkills.Items.Objects[i]).Free;
  end;
end;

procedure ClearUnitFilter;
begin
  with MainForm do begin
    Filter.UnitName := '';
    Filter.UnitNum := '';
    Filter.Items.Clear;
    Filter.Skills.ClearAndFree;
    Filter.Skills := TSkillList.Create;
    Filter.Mages := False;
    Filter.Any := False;
    Filter.FactionNum := -1;
  end;
end;

function UnitFilterEmpty: boolean;
begin
  with MainForm.Filter do begin
    Result := (UnitName = '') and (UnitNum = '') and (Items.Count = 0)
      and (Skills.Count = 0) and (Mages = False) and (FactionNum = -1);
  end;
end;

procedure TUnitFilterForm.cmSkillDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TUnitFilterForm.cmItemDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TUnitFilterForm.lbSkillsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ListDrawItem(Control, Index, Rect,
    SkillIcon(TSkill(lbSkills.Items.Objects[Index]).Data));
end;

procedure TUnitFilterForm.btnMyFactionClick(Sender: TObject);
begin
  cmFaction.ItemIndex := cmFaction.Items.IndexOfObject(Faction.Data);
end;

procedure TUnitFilterForm.btnAddSkillClick(Sender: TObject);
var Skill: TSkill;
    i: integer;
begin
  if cmSkill.ItemIndex = -1 then Exit;
  i := lbSkills.Items.Count-1;
  while (i >= 0) and (TSkill(lbSkills.Items.Objects[i]).Data <>
    TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex])) do Dec(i);
  if i >= 0 then Exit;
  Skill := TSkill.Create;
  Skill.Data := TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex]);
  Skill.Level := eSkillLv.Value;
  lbSkills.AddItem(Skill.Data.Name + ', lv ' + IntToStr(Skill.Level), Skill);
end;

procedure TUnitFilterForm.btnDelSkillClick(Sender: TObject);
begin
  if lbSkills.ItemIndex = -1 then Exit;
  TSkill(lbSkills.Items.Objects[lbSkills.ItemIndex]).Free;
  lbSkills.Items.Delete(lbSkills.ItemIndex);
end;

procedure TUnitFilterForm.btnAddItemClick(Sender: TObject);
var IData: TItemData;
begin
  if cmItem.ItemIndex = -1 then Exit;
  if lbItems.Items.IndexOfObject(cmItem.Items.Objects[cmItem.ItemIndex]) >= 0 then
    Exit;
  IData := TItemData(cmItem.Items.Objects[cmItem.ItemIndex]);
  lbItems.AddItem(IData.Name, IData);
end;

procedure TUnitFilterForm.btnDelItemClick(Sender: TObject);
begin
  if lbItems.ItemIndex = -1 then Exit;
  lbItems.Items.Delete(lbItems.ItemIndex);
end;

procedure TUnitFilterForm.lbItemsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TListBox(Control) do begin
    Canvas.FillRect(Rect);
    DrawItemIcon(Canvas, Rect.Left+1, Rect.Top, TItemData(Items.Objects[Index]));
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TUnitFilterForm.btnClearClick(Sender: TObject);
begin
  ClearUnitFilter;
  FillFilter;
end;

end.
