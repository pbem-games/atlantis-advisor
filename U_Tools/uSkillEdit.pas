unit uSkillEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Resources, ComCtrls, ToolWin, CheckLst, DataStructs, uGameSubs,
  Buttons, IntEdit, uInterface, Grids, PowerGrid;

const
  TypeIcons: array[0..2] of integer = (bmpSkill, bmpSpell, bmpFoundation);

type
  TSkillEditForm = class(TForm)
    Button1: TButton;
    SkillTree: TTreeView;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    CodeLabel: TLabel;
    mDesc: TRichEdit;
    cbCombat: TCheckBox;
    cbCast: TCheckBox;
    cmType: TComboBox;
    cmBased: TComboBox;
    Label3: TLabel;
    ToolBar1: TToolBar;
    btnSort: TToolButton;
    Label4: TLabel;
    eCost: TIntEdit;
    ToolBar2: TToolBar;
    btnAddBased: TToolButton;
    btnDelBased: TToolButton;
    gSpecs: TPowerGrid;
    Label2: TLabel;
    cmAbility: TComboBox;
    Label5: TLabel;
    gBased: TPowerGrid;
    ToolButton1: TToolButton;
    btnRequest: TToolButton;
    btnRequestAll: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure cmTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmBasedDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure DataChange(Sender: TObject);
    procedure btnDelBasedClick(Sender: TObject);
    procedure btnAddBasedClick(Sender: TObject);
    procedure SkillTreeExpanding(Sender: TObject; Node: TTreeNode;
      var AllowExpansion: Boolean);
    procedure btnSortClick(Sender: TObject);
    procedure SkillTreeChange(Sender: TObject; Node: TTreeNode);
    procedure gSpecsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure cmAbilityDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure gBasedSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure btnRequestClick(Sender: TObject);
    procedure btnRequestAllClick(Sender: TObject);
  private
    SelNode: TTreeNode;
    Filling: boolean;
    procedure FillTree;
    procedure AddBasedSkills(Node: TTreeNode);
    function MakeNode(Parent: TTreeNode; Caption: string; Image: integer;
      Data: Pointer): TTreeNode;
    procedure FillBoxes;
    function Selected: TSkillData;
  public
    Modified: boolean;
    procedure SelectItem(ASkillData: TSkillData);
  end;

var
  SkillEditForm: TSkillEditForm;

implementation

{$R *.DFM}

procedure TSkillEditForm.FormCreate(Sender: TObject);
begin
  gBased.Cols[1].AutoEdit := True;

  FillSpecCombo(cmAbility, True);
  FillBoxes;
  FillTree;
end;

procedure TSkillEditForm.FillBoxes;
begin
  FillSkillCombo(cmBased, SK_ALL, False, False, False);
end;

procedure TSkillEditForm.FillTree;
var i: integer;
    Regular, Magic, Foundation: TTreeNode;
begin
  SkillTree.Items.Clear;
  Foundation := MakeNode(nil, 'Foundations', bmpBook, nil);
  Magic := MakeNode(nil, 'Magic skills', bmpBook, nil);
  Regular := MakeNode(nil, 'Regular skills', bmpBook, nil);

  // Fill tree
  for i := 0 to Game.SkillData.Count-1 do begin
    if Test(Game.SkillData[i].Flags, SK_FOUNDATION) then
      MakeNode(Foundation, Game.SkillData[i].MakeName,
        SkillIcon(Game.SkillData[i]), Game.SkillData[i])
    else if Test(Game.SkillData[i].Flags, SK_MAGIC) then
      MakeNode(Magic, Game.SkillData[i].MakeName,
        SkillIcon(Game.SkillData[i]), Game.SkillData[i])
    else MakeNode(Regular, Game.SkillData[i].MakeName,
        SkillIcon(Game.SkillData[i]), Game.SkillData[i]);
  end;

  // Sort tree
  SkillTree.AlphaSort;
  if Regular.Count > 0 then begin
    SkillTree.Selected := Regular.Item[0];
    SkillTreeChange(Self, Regular.Item[0]);
  end;
end;

procedure TSkillEditForm.SelectItem(ASkillData: TSkillData);
var Node: TTreeNode;
begin
  if SkillTree.Items.Count > 0 then begin
    Node := SkillTree.Items[0];
    while (Node <> nil) and ((Node.Data = nil)
      or (TSkillData(Node.Data).Short <> ASkillData.Short)) do
      Node := Node.GetNext;
    if Node <> nil then begin
      SkillTree.Selected := Node;
      SkillTreeChange(Self, Node);
    end;
  end;
end;

function TSkillEditForm.MakeNode(Parent: TTreeNode; Caption: string; Image: integer;
  Data: Pointer): TTreeNode;
begin
  Result := SkillTree.Items.AddChild(Parent, Caption);
  Result.ImageIndex := Image;
  Result.SelectedIndex := Image;
  Result.Data := Data;
end;

procedure TSkillEditForm.AddBasedSkills(Node: TTreeNode);
var i: integer;
    SData: TSkillData;
begin
  Node.DeleteChildren;
  for i := 0 to Game.SkillData.Count-1 do begin
    SData := Game.SkillData[i];
    if SData.BasedOn.Find(TSkillData(Node.Data).Short) <> nil then
      MakeNode(Node, SData.MakeName, SkillIcon(SData), SData);
  end;
end;

procedure TSkillEditForm.SkillTreeChange(Sender: TObject; Node: TTreeNode);
var AData: TSkillData;
    IData: TItemData;
    i, j, row, max_lv: integer;
begin
  gBased.HideEditor;

  Filling := True;
  AData := TSkillData(Node.Data);
  if AData <> nil then SelNode := SkillTree.Selected
  else Exit;
  with AData do begin
    CodeLabel.Caption := Short;
    btnRequest.Enabled := not Requested;

    // Main info
    if Test(Flags, SK_FOUNDATION) then cmType.ItemIndex := 2
    else if Test(Flags, SK_MAGIC) then cmType.ItemIndex := 1
    else cmType.ItemIndex := 0;
    cbCast.Checked := Test(Flags, SK_CAST);
    cbCombat.Checked := Test(Flags, SK_COMBATSPELL);
    eCost.Value := Cost;
    cmAbility.ItemIndex := cmAbility.Items.IndexOfObject(Special);
    FillSDataGrid(gBased, BasedOn);

    // Descriptions
    mDesc.Lines.Clear;
    max_lv := 5;
    while (max_lv > 1) and (Descriptions[max_lv] = '') do Dec(max_lv);
    for i := 1 to max_lv do begin
      mDesc.SelAttributes.Style := [fsBold];
      mDesc.Lines.Add(AData.MakeName + ', level ' + IntToStr(i));
      mDesc.SelAttributes.Style := [];
      mDesc.SelAttributes.Name := 'Times';
      mDesc.SelAttributes.Style := [fsItalic];

      gSpecs.RowCount := 0;
      if not Test(Flags, SK_MAGIC) then begin
        row := 0;
        for j := 0 to Game.ItemData.Count-1 do begin
          IData := Game.ItemData[j];
          if not Test(IData.Flags, IT_MAN) then Continue;
          if IData.Man.SpecSkills.Find(Short) = nil then Continue;
          gSpecs.Cells[0, row] := IData.Name;
          gSpecs.Rows[row].Data := IData;
          Inc(row);
        end;
      end;
      gSpecs.Fixup;

      mDesc.SelAttributes.Name := mDesc.Font.Name;
      mDesc.SelAttributes.Style := [];
      if Descriptions[i] <> '' then mDesc.Lines.Add(Descriptions[i])
      else mDesc.Lines.Add('(no description)');
      mDesc.Lines.Add('');
    end;
  end;
  Filling := False;
end;

procedure TSkillEditForm.DataChange(Sender: TObject);
var AData: TSkillData;
begin
  if Filling then Exit;
  AData := TSkillData(SelNode.Data);
  with AData do begin
    case cmType.ItemIndex of
      1: SetFlag(Flags, SK_MAGIC, True);
      2: SetFlag(Flags, SK_FOUNDATION + SK_MAGIC, True);
      else SetFlag(Flags, SK_FOUNDATION + SK_MAGIC, False);
    end;
    SetFlag(Flags, SK_CAST, cbCast.Checked);
    SetFlag(Flags, SK_COMBATSPELL, cbCombat.Checked);
    SelNode.ImageIndex := SkillIcon(AData);
    SelNode.SelectedIndex := SkillIcon(AData);
    Cost := eCost.Value;
    if cmAbility.ItemIndex >= 0 then
      Special := TSpecData(cmAbility.Items.Objects[cmAbility.ItemIndex])
    else Special := nil;
  end;
  FillBoxes;
  SkillTreeChange(Sender, SelNode);
  Modified := True;
end;


procedure TSkillEditForm.cmTypeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.IconList.Draw(Canvas, Rect.Left+1, Rect.Top, TypeIcons[Index]);
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TSkillEditForm.cmBasedDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TSkillEditForm.btnDelBasedClick(Sender: TObject);
begin
  DelSkillFromGrid(Selected.BasedOn, gBased);
  DataChange(Sender);
end;

procedure TSkillEditForm.btnAddBasedClick(Sender: TObject);
begin
  AddSkillFromCombo(Selected.BasedOn, cmBased);
  DataChange(Sender);
end;

procedure TSkillEditForm.SkillTreeExpanding(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
var Child: TTreeNode;
begin
  Child := Node.getFirstChild;
  while Child <> nil do begin
    AddBasedSkills(Child);
    Child := Node.GetNextChild(Child);
  end;
end;

procedure TSkillEditForm.btnSortClick(Sender: TObject);
begin
  FillTree;
end;

procedure TSkillEditForm.gSpecsDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with Sender as TPowerGrid do begin
    DrawItemIcon(Canvas, TxtRect.Left+1, TxtRect.Top,
      TItemData(Rows[ARow].Data));
    TxtRect.Left := TxtRect.Left + 18;
  end;
end;

procedure TSkillEditForm.cmAbilityDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ComboDrawItem(Control, Index, Rect, bmpSpecial);
end;

procedure TSkillEditForm.gBasedSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  SDataGridSetEditText(Sender, Value, Selected.BasedOn);
end;

function TSkillEditForm.Selected: TSkillData;
begin
  Result := TSkillData(SelNode.Data);
end;

procedure TSkillEditForm.btnRequestClick(Sender: TObject);
var Data: TSkillData;
    i: integer;
begin
  if SelNode = nil then Exit;
  Data := TSkillData(SelNode.Data);
  for i := 1 to 5 do
    FactionLeader.Orders.Add('show skill "' + Data.Name + '" ' +
      IntToStr(i));
  Data.Requested := True;
  btnRequest.Enabled := False;
  mDesc.Lines.Text := 'Awaiting info from server';
end;

procedure TSkillEditForm.btnRequestAllClick(Sender: TObject);
var i, j, cnt: integer;
begin
  cnt := 0;
  for i := 0 to Game.SkillData.Count-1 do
    if (Game.SkillData[i].Descriptions[0] = '') and not Game.SkillData[i].Requested then begin
      for j := 1 to 5 do
        FactionLeader.Orders.Add('show skill "' + Game.SkillData[i].Name + '" ' +
          IntToStr(j));
      Game.SkillData[i].Requested := True;
      Inc(cnt, 5);
      if cnt > 90 then Break;
    end;
  MessageDlg('Advisor issued ' + IntToStr(cnt) + ' SHOW SKILL orders. Note ' +
    'that you shouldn''t issue more than 100 SHOW orders at one turn (including ' +
    'SHOW OBJECT and SHOW ITEM orders).', mtInformation, [mbOk], 0);
  btnRequestAll.Enabled := False;
end;

end.
