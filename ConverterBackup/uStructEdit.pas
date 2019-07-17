unit uStructEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Resources, ComCtrls, ToolWin, Grids, PowerGrid,
  DataStructs, uGameSubs, IntEdit, uInterface, Math, uKeys, MyStrings;

const
  StructGridCols = 4;
  StructGridHeaders: array[0..StructGridCols-1] of string = ('#',
    'Type', 'Group', 'Size');
  StructGridFormats: array[0..StructGridCols-1] of TColFormat = (cfNumber,
    cfNumber, cfString, cfNumber);

type
  TStructEditForm = class(TForm)
    CloseBtn: TButton;
    Grid: TPowerGrid;
    ToolBar1: TToolBar;
    btnNoFilter: TToolButton;
    btnFilter: TToolButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    GroupLabel: TLabel;
    GroupBox2: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    GroupBox3: TGroupBox;
    Label2: TLabel;
    Label3: TLabel;
    cmMaterial1: TComboBox;
    cmMaterial2: TComboBox;
    Label7: TLabel;
    cmSkill: TComboBox;
    GroupBox4: TGroupBox;
    cmProduction: TComboBox;
    SizeEdit: TIntEdit;
    ProtectionEdit: TIntEdit;
    CapacityEdit: TIntEdit;
    SailorEdit: TIntEdit;
    SkillLvEdit: TIntEdit;
    cmType: TComboBox;
    mDescription: TRichEdit;
    lTool: TLabel;
    cmTool: TComboBox;
    eToolBonus: TIntEdit;
    ToolButton1: TToolButton;
    btnArrange: TToolButton;
    Label6: TLabel;
    gDefence: TPowerGrid;
    ToolButton2: TToolButton;
    btnRequest: TToolButton;
    btnRequestAll: TToolButton;
    procedure FormCreate(Sender: TObject);
    procedure ToolButtonClick(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DataChange(Sender: TObject);
    procedure cmIDataDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmSkillDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmTypeDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btnRequestClick(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure GridEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure btnArrangeClick(Sender: TObject);
    procedure gDefenceDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure gDefenceSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure btnRequestAllClick(Sender: TObject);
  private
    Filling: boolean;
    Filter: integer;
    procedure FillGrid;
  public
    Modified: boolean;
  end;

var
  StructEditForm: TStructEditForm;

implementation

{$R *.DFM}

procedure TStructEditForm.FormCreate(Sender: TObject);
var i: integer;
    tarm: boolean;
begin
  Filling := True;
  Filter := -1;

  // Tarmellion-specific controls
  tarm := (GameConfig.ReadInteger('Settings', 'Mod', modStandard) = modTarmellion);
  lTool.Visible := tarm;
  cmTool.Visible := tarm;
  eToolBonus.Visible := tarm;
  gDefence.Visible := tarm;

  // Setup grids
  gDefence.Cols[1].AutoEdit := True;
  for i := 0 to StructGridCols-1 do begin
    Grid.Cells[i, 0] := StructGridHeaders[i];
    Grid.Cols[i].Format := StructGridFormats[i];
  end;

  // Fill item combos
  FillResourceCombo(cmMaterial1, True);
  FillResourceCombo(cmMaterial2, True);
  FillItemDataCombo(cmProduction, IT_RESOURCE + IT_FOOD, False, True);
  cmProduction.AddItem('entertainment', SilverData);
  FillItemDataCombo(cmTool, IT_TOOL, False, True, False);

  // Fill skills
  FillSkillCombo(cmSkill, SK_ALL, False, True, False);
  Filling := False;

  FillGrid;
end;

procedure TStructEditForm.FillGrid;
var i, row, ext: integer;
    StData: TStructData;
begin
  // Fill grid
  Grid.RowCount := 0;
  row := 1;
  for i := 0 to Game.StructData.Count-1 do begin
    StData := Game.StructData[i];
    if (Filter >= 0) and (StData.Flags <> DWord(Filter)) then
      Continue;
    Grid.Cells[0, row] := IntToStr(i);
    Grid.SortKeys[1, row] := IntToStr(StData.Flags);
    Grid.Cells[2, row] := StData.Group;
    Grid.Cells[3, row] := IntToStr(StData.Size);
    Grid.Rows[row].Data := StData;
    if Test(StData.Flags, ST_ROAD) then ext := extShaft + 1
    else ext := StructExtra(StData);
    Grid.Rows[row].ImageIndex := ext - extBuilding + bmpStructs;
    Inc(row);
  end;
  Grid.Fixup;
end;

procedure TStructEditForm.ToolButtonClick(Sender: TObject);
var StData: TStructData;
begin
  if Grid.RowCount = 1 then Exit;
  StData := TStructData(Grid.ImgRows[Grid.Row].Data);
  if TToolButton(Sender).Tag = 0 then Filter := -1
  else Filter := StData.Flags;
  FillGrid;
end;

procedure TStructEditForm.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var StData: TStructData;
    i: integer;
begin
  Filling := True;
  StData := TStructData(Grid.ImgRows[ARow].Data);
  with StData do begin
    GroupLabel.Caption := Group;
    if Requested then
      mDescription.Lines.Text := 'Awaiting info from server'
    else mDescription.Text := Description;
    btnRequest.Enabled := not Requested;
    // Numbers
    SizeEdit.Value := Size;
    CapacityEdit.Value := Capacity;
    SailorEdit.Value := Sailors;
    ProtectionEdit.Value := Protection;
    // Flags
    if Test(Flags, ST_DEFENCE) then cmType.ItemIndex := 1
    else if Test(Flags, ST_FLYING) and Test(Flags, ST_TRANSPORT) then
      cmType.ItemIndex := 3
    else if Test(Flags, ST_TRANSPORT) then cmType.ItemIndex := 2
    else if Test(Flags, ST_CLOSED) then cmType.ItemIndex := 4
    else if Test(Flags, ST_SHAFT) then cmType.ItemIndex := 5
    else if Test(Flags, ST_ROAD) then cmType.ItemIndex := 6
    else cmType.ItemIndex := 0;
    // Combos
    cmMaterial1.ItemIndex := cmMaterial1.Items.IndexOfObject(Material1);
    cmMaterial2.ItemIndex := cmMaterial2.Items.IndexOfObject(Material2);
    cmProduction.ItemIndex := cmProduction.Items.IndexOfObject(Resource);
    if BuildSkill <> nil then begin
      cmSkill.ItemIndex := cmSkill.Items.IndexOfObject(BuildSkill.Data);
      SkillLvEdit.Value := BuildSkill.Level;
    end
    else begin
      cmSkill.ItemIndex := 0;
      SkillLvEdit.Value := 0;
    end;
    cmTool.ItemIndex := Max(0, cmTool.Items.IndexOfObject(Tool));
    eToolBonus.Value := ToolBonus;

    // Defence
    gDefence.RowCount := 0;
    for i := 0 to atCount-2 do begin
      gDefence.Cells[0, i] := GetKey(s_atMelee, i);
      gDefence.Cells[1, i] := IntToStr(Defence[i]);
    end;
    gDefence.Fixup;
  end;
  Filling := False;
end;

procedure TStructEditForm.DataChange(Sender: TObject);
var AStructData: TStructData;
    i, ext: integer;
    Filter: string;
begin
  if Filling then Exit;
  AStructData := TStructData(Grid.ImgRows[Grid.Row].Data);
  with AStructData do begin
    // Numbers
    Size := SizeEdit.Value;
    Capacity := CapacityEdit.Value;
    Sailors := SailorEdit.Value;
    Protection := ProtectionEdit.Value;
    // Flags
    Flags := 0;
    SetFlag(Flags, ST_DEFENCE, cmType.ItemIndex = 1);
    SetFlag(Flags, ST_TRANSPORT, cmType.ItemIndex in [2, 3]);
    SetFlag(Flags, ST_FLYING, cmType.ItemIndex = 3);
    SetFlag(Flags, ST_CLOSED, cmType.ItemIndex = 4);
    SetFlag(Flags, ST_SHAFT, cmType.ItemIndex = 5);
    SetFlag(Flags, ST_ROAD, cmType.ItemIndex = 6);
    // Combos
    if cmMaterial1.ItemIndex > 0 then
      Material1 := TItemData(cmMaterial1.Items.Objects[cmMaterial1.ItemIndex])
    else Material1 := nil;
    if cmMaterial2.ItemIndex > 0 then
      Material2 := TItemData(cmMaterial2.Items.Objects[cmMaterial2.ItemIndex])
    else Material2 := nil;
    if cmProduction.ItemIndex > 0 then
      Resource := TItemData(cmProduction.Items.Objects[cmProduction.ItemIndex])
    else Resource := nil;
    FreeAndNil(BuildSkill);
    if cmSkill.ItemIndex > 0 then begin
      BuildSkill := TSkill.Create;
      BuildSkill.Level := SkillLvEdit.Value;
      BuildSkill.Data := TSkillData(cmSkill.Items.Objects[cmSkill.ItemIndex]);
    end;
    Tool := TItemData(cmTool.Items.Objects[cmTool.ItemIndex]);
    ToolBonus := eToolBonus.Value;

    // Defence
    for i := 0 to atCount-2 do
      Defence[i] := ToInt(gDefence.Cells[1, i]);

    // Remove filter if type changed
    Filter := Config.ReadString('StructEdit', 'Filter', '');
    if (Filter <> '') and (Filter <> IntToStr(Flags)) then begin
      btnNoFilter.Down := True;
      btnFilter.Down := False;
      btnNoFilter.Click;
      i := 1;
      while (i < Grid.ImgRowCount) and (TStructData(Grid.ImgRows[i].Data).Group <>
        Group) do Inc(i);
      if i < Grid.ImgRowCount then Grid.Row := i;
    end;

    if Test(AStructData.Flags, ST_ROAD) then ext := extShaft + 1
    else ext := StructExtra(AStructData);
    Grid.Rows[Grid.Row].ImageIndex := ext - extBuilding + bmpStructs;
    Grid.ImgCells[3, Grid.Row] := IntToStr(Size);
    Grid.Fixup;
  end;
  Modified := True;
end;

procedure TStructEditForm.cmIDataDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TStructEditForm.cmSkillDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TStructEditForm.cmTypeDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with TComboBox(Control) do begin
    Canvas.FillRect(Rect);
    ResForm.IconList.Draw(Canvas, Rect.Left+1, Rect.Top, bmpStructs + Index);
    Rect.Left := Rect.Left + 18;
    Canvas.TextRect(Rect, Rect.Left+1, Rect.Top+1, Items[Index]);
  end;
end;

procedure TStructEditForm.btnRequestClick(Sender: TObject);
var Data: TStructData;
begin
  Data := TStructData(Grid.ImgRows[Grid.Row].Data);
  FactionLeader.Orders.Add('show object "' + Data.Group + '"');
  Data.Requested := True;
  btnRequest.Enabled := False;
  mDescription.Lines.Text := 'Awaiting info from server';
end;

procedure TStructEditForm.btnRequestAllClick(Sender: TObject);
var i, cnt: integer;
begin
  cnt := 0;
  for i := 0 to Game.StructData.Count-1 do
    if (Game.StructData[i].Description = '') and not Game.StructData[i].Requested then begin
      FactionLeader.Orders.Add('show object "' + Game.StructData[i].Group + '"');
      Game.StructData[i].Requested := True;
      Inc(cnt);
      if cnt > 95 then Break;
    end;
  MessageDlg('Advisor issued ' + IntToStr(cnt) + ' SHOW OBJECT orders. Note ' +
    'that you shouldn''t issue more than 100 SHOW orders at one turn (including ' +
    'SHOW SKILL and SHOW ITEM orders).', mtInformation, [mbOk], 0);
  btnRequestAll.Enabled := False;
end;

procedure TStructEditForm.GridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  with TPowerGrid(Sender) do begin
    if MouseCell.Y >= FixedRows then BeginDrag(False);
  end;
end;

procedure TStructEditForm.GridDragOver(Sender, Source: TObject; X,
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

procedure TStructEditForm.GridEndDrag(Sender, Target: TObject; X,
  Y: Integer);
var StData1, StData2: TStructData;
begin
  if Target = Sender then with TPowerGrid(Sender) do begin
    if (MouseCell.Y >= FixedRows) and (MouseCell.Y <> Row) then begin
      StData1 := ImgRows[Row].Data;
      StData2 := ImgRows[MouseCell.Y].Data;
      Game.StructData.Delete(Game.StructData.IndexOf(StData1));
      Game.StructData.Insert(Game.StructData.IndexOf(StData2), StData1);
      FillGrid;
    end;
  end;
end;

procedure TStructEditForm.btnArrangeClick(Sender: TObject);
const Masks: array[0..6] of DWord = (ST_SHAFT, ST_FLYING, ST_TRANSPORT,
  ST_DEFENCE, ST_UNKNOWN, ST_ROAD, ST_CLOSED);
      UnknownPr = 6;
var i, j, pr1, pr2: integer;
    D1, D2: TStructData;
    F1, F2: DWord;
begin
  for i := 0 to Game.StructData.Count-1 do
    for j := i+1 to Game.StructData.Count-1 do begin
      D1 := Game.StructData[i];
      D2 := Game.StructData[j];
      // Priorities
      pr1 := 0;
      while pr1 < Length(Masks) do begin
        if D1.Flags <> 0 then F1 := D1.Flags else F1 := ST_UNKNOWN;
        if (F1 and Masks[pr1]) = Masks[pr1] then Break;
        Inc(pr1);
      end;
      if pr1 = Length(Masks) then pr1 := UnknownPr;
      pr2 := 0;
      while pr2 < Length(Masks) do begin
        if D2.Flags <> 0 then F2 := D2.Flags else F2 := ST_UNKNOWN;
        if (F2 and Masks[pr2]) = Masks[pr2] then Break;
        Inc(pr2);
      end;
      if pr2 = Length(Masks) then pr2 := UnknownPr;
      if pr1 > pr2 then Game.StructData.Exchange(i, j);
      if pr1 <> pr2 then Continue;
      // Alpha sort
      if D1.Group > D2.Group then Game.StructData.Exchange(i, j);
    end;
  FillGrid;
end;

procedure TStructEditForm.gDefenceDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with Sender as TPowerGrid do begin
    if ACol <> 0 then Exit;
    ResForm.Extras.Draw(Canvas, TxtRect.Left + 1, TxtRect.Top + 4,
      bmp_extAttackTypes + ARow);
    Inc(TxtRect.Left, 11);
  end;
end;

procedure TStructEditForm.gDefenceSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  DataChange(Sender);
end;


end.
