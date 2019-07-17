unit uBattle;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, StdCtrls, ExtCtrls, Resources, Math, ComCtrls,
  uGameSubs, IniFiles, MyStrings, Painter, Grids, PowerGrid, uInterface,
  CylinderMap, uKeys, ToolWin, Buttons, IntEdit, uAvatars, uArmy, uSoldiers,
  ImgList, uBattleDraw, Menus, ActnList;

type
  TBattleFormMode = (bmNormal, bmPlay);

  TBattleForm = class(TForm)
    tsSpoils: TTabSheet;
    gSpoils: TPowerGrid;
    Panel4: TPanel;
    ToolBar: TToolBar;
    btnSimulation: TToolButton;
    ToolButton8: TToolButton;
    btnPlay: TToolButton;
    pRound: TPanel;
    lRound: TLabel;
    trRounds: TTrackBar;
    PlayPopup: TPopupMenu;
    Slow1: TMenuItem;
    Fast1: TMenuItem;
    Normal1: TMenuItem;
    btnStartSim: TToolButton;
    SimPopup: TPopupMenu;
    N10battles1: TMenuItem;
    N50battles1: TMenuItem;
    N100battles1: TMenuItem;
    procedure PControlChange(Sender: TObject);
    procedure trRoundsChange(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  published
    cmBattles: TComboBox;
    Button1: TButton;
    PControl: TPageControl;
    tsReport: TTabSheet;
    tsSimUnit: TTabSheet;
    Memo: TMemo;
    Painter: TPainter;
    ScrollBar: TScrollBar;
    pUnitControls: TPanel;
    eName: TEdit;
    bvName: TBevel;
    cbBehind: TCheckBox;
    ItemGrid: TPowerGrid;
    cmItems: TComboBox;
    ToolBar2: TToolBar;
    btnAddItem: TToolButton;
    btnDelItem: TToolButton;
    SkillGrid: TPowerGrid;
    cmSkills: TComboBox;
    ToolBar1: TToolBar;
    btnAddSkill: TToolButton;
    btnDelSkill: TToolButton;
    Panel1: TPanel;
    imgAtkLeader: TImage;
    Panel2: TPanel;
    imgDefLeader: TImage;
    lAtkTactics: TLabel;
    lDefTactics: TLabel;
    lAtkLeaderName: TLabel;
    lDefLeaderName: TLabel;
    pUnitButtons: TPanel;
    btnAddAttacker: TBitBtn;
    btnAddDefender: TBitBtn;
    btnDelUnit: TButton;
    lAtkWin: TLabel;
    lDefWin: TLabel;
    tsUnit: TTabSheet;
    tsSimRegion: TTabSheet;
    Label2: TLabel;
    lFaction: TLabel;
    imgFaction: TImage;
    lNum: TLabel;
    lCast: TLabel;
    Label7: TLabel;
    gUItems: TPowerGrid;
    gUSkills: TPowerGrid;
    lUName: TLabel;
    cmTerrain: TComboBox;
    Label1: TLabel;
    lMounts: TLabel;
    Label3: TLabel;
    cmStructs: TComboBox;
    barStructControl: TToolBar;
    tbAddStruct: TToolButton;
    tbDelStruct: TToolButton;
    gStructs: TPowerGrid;
    Label4: TLabel;
    cmInside: TComboBox;
    ToolBar5: TToolBar;
    ToolBar6: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    tbCombatSpell: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolBar4: TToolBar;
    btnSoldiers: TToolButton;
    procedure cmBattlesChange(Sender: TObject);
    procedure PainterMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SkillGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure PainterMouseOut(Sender: TObject);
    procedure PainterMouseOver(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure ScrollBarChange(Sender: TObject);
    procedure eNameExit(Sender: TObject);
    procedure cmItemsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmSkillsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure PainterMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DataChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: Char);
    procedure btnAddUnitClick(Sender: TObject);
    procedure btnAddItemClick(Sender: TObject);
    procedure btnDelItemClick(Sender: TObject);
    procedure btnAddSkillClick(Sender: TObject);
    procedure btnDelSkillClick(Sender: TObject);
    procedure btnDelUnitClick(Sender: TObject);
    procedure btnStartSimClick(Sender: TObject);
    procedure PainterKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnSimulationClick(Sender: TObject);
    procedure cmStructsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmTerrainDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmTerrainChange(Sender: TObject);
    procedure tbAddStructClick(Sender: TObject);
    procedure gStructsDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure tbDelStructClick(Sender: TObject);
    procedure cmInsideDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cmSpellDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure ItemFilterClick(Sender: TObject);
    procedure SkillGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure tbCombatSpellClick(Sender: TObject);
    procedure ItemGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure SkillGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure ItemGridDrawIcon(Sender: TObject; ACanvas: TCanvas; X,
      Y: Integer; Data: Pointer);
    procedure btnSoldiersClick(Sender: TObject);
    procedure SetupPlayControls(Value: boolean);
  private
    Mode: TBattleFormMode;
    Timer: TTimer;
    Filling: boolean;
    FillingUnit: TBattleUnit;
    procedure SetupBattle(ABattle: TBattle);
    procedure SetupPainter;
    procedure SetupTerrain;
    procedure SetupPictures;
    procedure FillStructs;
    procedure RepaintBattle;
    procedure DrawTactics;
    procedure FillUnitInfo(BUnit: TBattleUnit; ForceRefresh: boolean);
    procedure FillItems(Filter: DWord; AddHeal: boolean);
    procedure SetupControls;
    procedure PlayTimer(Sender: TObject);
    procedure RunSimulation(amt: integer);
    procedure RunOneBattle;
    procedure StopPlay;
  public
    NumFront: array[sideAttack..sideDefence] of integer;
    constructor Create(AOwner: TComponent; ABattle: TBattle); reintroduce;
  end;

var
  BattleForm: TBattleForm;

implementation

{$R *.lfm}

{ TBattleForm }

constructor TBattleForm.Create(AOwner: TComponent; ABattle: TBattle);
var i: integer;

  procedure AddSkill(SData: TSkillData);
  begin
    if (SData <> nil) and (cmSkills.Items.IndexOf(SData.Name) < 0) then
      cmSkills.AddItem(SData.Name, SData);
  end;

begin
  inherited Create(AOwner);
  SetupDrawArea(Painter);

  gStructs.Cells[0, 0] := 'N';
  gStructs.Cells[1, 0] := 'Object';
  gStructs.Cells[2, 0] := 'Defence';

  ItemGrid.Cols[0].AutoEdit := True;
  SkillGrid.Cols[1].AutoEdit := True;
  gSpoils.Cols[1].Format := cfNumber;

  // Fill battle select
  if ABattle = nil then begin
    Caption := 'Battles in ' + MakeRegionName(CurrRegion.Coords, True);
    for i := 0 to CurrRegion.Battles.Count-1 do
      cmBattles.AddItem(CurrRegion.Battles[i].Name, CurrRegion.Battles[i]);
  end
  else cmBattles.AddItem(ABattle.Name, ABattle);
  cmBattles.ItemIndex := 0;
  cmBattles.Enabled := (cmBattles.Items.Count > 1);

  // Fill unit pickers
  AddSkill(Game.SkillData.Find(Keys[s_Combat]));
  AddSkill(Game.SkillData.Find(Keys[s_Tactics]));
  for i := 0 to Game.ItemData.Count-1 do begin
    // Add weapon and mount's skills
    if Test(Game.ItemData[i].Flags, IT_WEAPON) then begin
      AddSkill(Game.ItemData[i].Weapon.Skill1);
      AddSkill(Game.ItemData[i].Weapon.Skill2);
    end
    else if Test(Game.ItemData[i].Flags, IT_MOUNT) then
      AddSkill(Game.ItemData[i].Mount.RideSkill);
  end;
  AddSkill(Game.SkillData.Find(Keys[s_Healing]));
  AddSkill(Game.SkillData.Find(Keys[s_MagicalHealing]));
  for i := 0 to Game.SkillData.Count-1 do
    if Test(Game.SkillData[i].Flags, SK_COMBATSPELL) then
      cmSkills.AddItem(Game.SkillData[i].Name, Game.SkillData[i]);

  // Add items
  FillItems(IT_MAN + IT_MONSTER + IT_WEAPON + IT_ARMOR + IT_MOUNT +
    IT_MAGIC, True);

  // Region
  for i := 0 to Game.TerrainData.Count-1 do
    cmTerrain.AddItem(Game.TerrainData[i].Name, Game.TerrainData[i]);
  for i := 0 to Game.StructData.Count-1 do
    if Game.StructData[i].Protection > 0 then
      cmStructs.AddItem(Game.StructData[i].Group, Game.StructData[i]);

  if (ABattle <> nil) and (ABattle.Name = 'Simulation') then begin
    btnSimulation.Down := True;
    btnSimulation.Enabled := False;
    PControl.ActivePage := tsSimUnit;
  end
  else begin
    PControl.ActivePageIndex := Config.ReadInteger('BattleViewer', 'Page', 0);
    barStructControl.Enabled := False;
    cmTerrain.Enabled := False;
  end;
  SetupControls;
  FillUnitInfo(nil, True);
end;

procedure TBattleForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Mode = bmPlay then StopPlay;
end;

procedure TBattleForm.FillItems(Filter: DWord; AddHeal: boolean);
var IData: TItemData;
begin
  FillItemDataCombo(cmItems, Filter, False, False);
  if AddHeal then begin
    IData := Game.ItemData.Find(Keys[s_Herbs]);
    if IData <> nil then cmItems.AddItem(IData.Name(True), IData);
    IData := Game.ItemData.Find(Keys[s_HealingPotions]);
    if IData <> nil then cmItems.AddItem(IData.Name(True), IData);
  end;
end;

procedure TBattleForm.FillStructs;
var i: integer;
    St: TStruct;
begin
  cmInside.Items.Clear;
  cmInside.Items.Add('');
  gStructs.RowCount := 0;
  for i := 0 to Battle.Region.Structs.Count-1 do begin
    St := Battle.Region.Structs[i];
    cmInside.AddItem(St.Data.Group + ' [' + IntToStr(i+1) + ']', St.Data);
    gStructs.Cells[0, i + 1] := IntToStr(i + 1);
    gStructs.Cells[1, i + 1] := St.Data.Group;
    gStructs.Cells[2, i + 1] := IntToStr(St.Data.Protection);
    gStructs.Rows[i + 1].Data := St.Data;
  end;
  gStructs.Fixup;
end;

procedure TBattleForm.SetupControls;
begin
  tsReport.TabVisible := not btnSimulation.Down;
  tsUnit.TabVisible := not btnSimulation.Down;
  tsSpoils.TabVisible := not btnSimulation.Down;
  tsSimUnit.TabVisible := btnSimulation.Down;
  tsSimRegion.TabVisible := btnSimulation.Down;
  btnStartSim.Enabled := btnSimulation.Down;
  trRounds.Enabled := not btnSimulation.Down;
  lAtkWin.Caption := '';
  lDefWin.Caption := '';
  if not btnSimulation.Down then btnPlay.Hint := 'Play battle'
  else btnPlay.Hint := 'Play simulated battle';
end;

procedure TBattleForm.btnSimulationClick(Sender: TObject);
begin
  SetupControls;
end;

procedure TBattleForm.FormActivate(Sender: TObject);
begin
  cmBattlesChange(Self);
end;

procedure TBattleForm.cmBattlesChange(Sender: TObject);
begin
  SetupBattle(TBattle(cmBattles.Items.Objects[cmBattles.ItemIndex]));
  RepaintBattle;
end;

procedure TBattleForm.SetupBattle(ABattle: TBattle);
var i, j: integer;
    Tactitian: TBattleUnit;
    side: boolean;
    Units: TBattleUnitList;

  function NeedExchange(A, B: TBattleUnit): boolean;
  var amtA, amtB: integer;
  begin
    Result := False;
    if A.Behind and not B.Behind then Result := True
    else if A.Behind <> B.Behind then Exit
    else if not A.Mage and B.Mage then Result := True
    else if A.Mage <> B.Mage then Exit
    else if B = Tactitian then Result := True
    else if A = Tactitian then Exit
    else if not A.Faction.Player and B.Faction.Player then Result := True
    else if A.Faction.Player <> B.Faction.Player then Exit
    else if A.Faction.Num > B.Faction.Num then Result := True
    else if A.Faction.Num <> B.Faction.Num then Exit
    else begin
      amtA := A.Items.Amount(IT_MAN) + A.Items.Amount(IT_MONSTER);
      amtB := B.Items.Amount(IT_MAN) + B.Items.Amount(IT_MONSTER);
      if amtA < amtB then Result := True;
    end;
  end;

begin
  Battle := ABattle;
  Memo.Lines.Assign(Battle.Report);

  FillIDataGrid(gSpoils, Battle.Spoils, True);
  if not btnSimulation.Down then begin
    if Battle.Loses[sideAttack] > 0 then
      lAtkWin.Caption := 'loses ' + IntToStr(Battle.Loses[sideAttack]);
    if Battle.Loses[sideDefence] > 0 then
      lDefWin.Caption := 'loses ' + IntToStr(Battle.Loses[sideDefence]);
  end;
  trRounds.Max := Max(0, High(Battle.Rounds));
  trRounds.Position := 0;
  trRoundsChange(trRounds);

  // Setup units
  SelUnit := nil;
  for side := sideAttack to sideDefence do begin
    Units := Battle.Units[side];

    // Sort Units by front/behind and importance
    TacticsLevel(Units, i); // find army leader
    if i >= 0 then Tactitian := Units[i]
    else Tactitian := nil;

    for i := 0 to Units.Count-1 do
      for j := i+1 to Units.Count-1 do
        if NeedExchange(Units[i], Units[j]) then
          Units.Exchange(i, j);
  end;
  SetupPainter;
  SetupTerrain;
  SetupPictures;
  FillStructs;
  cmTerrain.ItemIndex := Game.TerrainData.IndexOf(Battle.Region.Terrain);
  cmTerrainChange(nil);
end;

procedure TBattleForm.SetupPictures;
var i: integer;
    side: boolean;
    BUnit: TBattleUnit;
begin
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      BUnit := Battle.Units[side][i];
      BUnit.Viewer.Picture := UnitMultiImg(BUnit, Battle.Region);
    end;
end;

procedure TBattleForm.SetupPainter;
var front, behind: integer;
begin
  front := Max(NumFront[sideAttack], NumFront[sideDefence]);
  behind := Max(Battle.Units[sideAttack].Count - NumFront[sideAttack],
    Battle.Units[sideDefence].Count - NumFront[sideDefence]);

  // Set painter height
  ScrollBar.Max := Max(Painter.Height, unit_margin * 2 +
    Max(front, behind) * unit_dy + 20);
  ScrollBar.PageSize := Painter.Height;
  ScrollBar.LargeChange := Painter.Height;
end;

procedure TBattleForm.SetupTerrain;
var i: integer;
    Wea: TWeatherData;
    Tr: TTerrainData;
    s: string;
begin
  Wea := GetWeather(Battle.Region.Coords);
  if Wea <> nil then i := Min(weatherCount, Game.WeatherData.IndexOf(Wea))
  else i := 0;
  s := '';
  TerIndex := 0;
  Tr := Battle.Region.Terrain;
  if Tr <> nil then s := BattleIni.ReadString('Terrain', Tr.Name, '');
  if s <> '' then begin
    Tr := Game.TerrainData.Find(s);
    if Tr <> nil then TerIndex := Tr.BmpIndexes[i];
  end;
end;

procedure TBattleForm.RepaintBattle;
begin
  RedrawBattleRect;
  DrawTactics;
end;

procedure TBattleForm.DrawTactics;

  procedure DrawTactic(Side: boolean; Image: TImage; lName, lSkill: TLabel);
  var lv, idx: integer;
      BUnit: TBattleUnit;
      s: string;
  begin
    lv := TacticsLevel(Battle.Units[Side], idx);
    if idx = -1 then begin
      lName.Caption := '';
      lSkill.Caption := '';
    end
    else begin
      BUnit := Battle.Units[side][idx];
      lName.Caption := BUnit.Name;
      lSkill.Caption := 'tactics ' + IntToStr(lv);
      s := BUnit.Viewer.Picture;
      if Pos(' & ', s) > 0 then s := Copy(s, 1, Pos(' & ', s)-1);
      DrawCombinedAvatar(BUnit.URef, BUnit.Faction, s, Image.Picture.Bitmap,
        (BUnit.Side = sideDefence));
      Image.Invalidate;
    end;
  end;

begin
  DrawTactic(sideAttack, imgAtkLeader, lAtkLeaderName, lAtkTactics);
  DrawTactic(sideDefence, imgDefLeader, lDefLeaderName, lDefTactics);
end;

procedure TBattleForm.FillUnitInfo(BUnit: TBattleUnit; ForceRefresh: boolean);
begin
  Filling := True;
  ItemGrid.HideEditor;
  SkillGrid.HideEditor;
  pUnitControls.Enabled := (BUnit <> nil);
  btnDelUnit.Enabled := (SelUnit <> nil) and (BUnit <> nil);
  if BUnit <> FillingUnit then begin
    tbCombatSpell.Down := False;
    tbCombatSpell.Enabled := False;
  end;
  if BUnit = nil then begin
    eName.Text := '';
    lUName.Caption := '';
    lNum.Caption := '';
    cbBehind.Checked := False;
    lCast.Caption := '';
    lFaction.Caption := '';
    imgFaction.Visible := False;
    ItemGrid.RowCount := 0;
    ItemGrid.Fixup;
    gUItems.RowCount := 0;
    gUItems.Fixup;
    SkillGrid.RowCount := 0;
    SkillGrid.Fixup;
    gUSkills.RowCount := 0;
    gUSkills.Fixup;
    cmInside.ItemIndex := -1;
  end
  else begin
    if (FillingUnit <> BUnit) or ForceRefresh then begin
      eName.Text := BUnit.Name;
      lUName.Caption := BUnit.Name;
      lNum.Caption := IntToStr(BUnit.Num);
      cbBehind.Checked := BUnit.Behind;
      lCast.Caption := BUnit.SpellText;
      if BUnit.Faction <> nil then lFaction.Caption := BUnit.Faction.Name
      else lFaction.Caption := '';
      imgFaction.Visible := TRUE;
      DrawCExtra(extFlag, BUnit.Faction, imgFaction.Canvas, 0, 0);
      cmInside.ItemIndex := Battle.Region.Structs.IndexOf(BUnit.Struct) + 1;
      // Items
      FillIDataGrid(ItemGrid, BUnit.Items);
      FillIDataGrid(gUItems, BUnit.Items);
      // Skills
      FillSDataGrid(SkillGrid, BUnit.Skills);
      FillSDataGrid(gUSkills, BUnit.Skills);
    end;
  end;
  FillingUnit := BUnit;
  Filling := False;
end;

procedure TBattleForm.DataChange(Sender: TObject);
var Units: TBattleUnitList;
    side: boolean;
begin
  if Filling then Exit;
  if SelUnit <> nil then begin
    SelUnit.Viewer.Picture := UnitMultiImg(SelUnit, Battle.Region);
    // Load with form parameters
    SelUnit.Name := eName.Text;
    if SelUnit.Behind <> cbBehind.Checked then begin
      SelUnit.Behind := cbBehind.Checked;
      side := SelUnit.Side;
      Units := Battle.Units[side];
      Units.Delete(Units.IndexOf(SelUnit));
      if not SelUnit.Behind then begin
        Units.Insert(NumFront[side], SelUnit);
        Inc(NumFront[side]);
      end
      else begin
        Units.Insert(Units.Count, SelUnit);
        Dec(NumFront[side]);
      end;
      SetupPainter;
    end;
    if cmInside.ItemIndex < 1 then SelUnit.Struct := nil
    else SelUnit.Struct := Battle.Region.Structs[cmInside.ItemIndex-1];
    // Repaint
    RepaintBattle;
  end;
end;

procedure TBattleForm.SkillGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with Sender as TPowerGrid do begin
    if ACol = 0 then begin
      if (FillingUnit <> nil) and
        (FillingUnit.CombatSpell = TSkillData(ImgRows[ARow].Data)) then
        ResForm.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top, bmpCombatSpell)
      else ResForm.IconList.Draw(Canvas, TxtRect.Left+1, TxtRect.Top,
        SkillIcon(TSkillData(ImgRows[ARow].Data)));
      TxtRect.Left := TxtRect.Left + 18;
    end;
  end;
end;

procedure TBattleForm.SkillGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var SData: TSkillData;
begin
  SData := TSkillData(SkillGrid.ImgRows[ARow].Data);
  tbCombatSpell.Enabled := Test(SData.Flags, SK_COMBATSPELL);
  tbCombatSpell.Down := (FillingUnit <> nil) and (SData = FillingUnit.CombatSpell);
end;

procedure TBattleForm.tbCombatSpellClick(Sender: TObject);
begin
  if SelUnit = nil then Exit;
  if tbCombatSpell.Down then
    SelUnit.CombatSpell := TSkillData(SkillGrid.ImgRows[SkillGrid.Row].Data)
  else SelUnit.CombatSpell := nil;
  FillUnitInfo(SelUnit, True);
end;

procedure TBattleForm.PainterMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var BUnit: TBattleUnit;
begin
  BUnit := UnitInPos(X, Y);
  if BUnit <> nil then FillUnitInfo(BUnit, False)
  else FillUnitInfo(SelUnit, False);
end;

procedure TBattleForm.PainterMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Mode <> bmNormal then Exit;
  eNameExit(eName);
  if Button = mbRight then Exit;
  SelUnit := UnitInPos(X, Y);
  FillUnitInfo(SelUnit, True);
  RepaintBattle;
end;

procedure TBattleForm.PainterMouseOut(Sender: TObject);
begin
  DrawNames := False;
  RepaintBattle;
end;

procedure TBattleForm.PainterMouseOver(Sender: TObject);
begin
  DrawNames := True;
  RepaintBattle;
end;

procedure TBattleForm.ScrollBarChange(Sender: TObject);
begin
  ShiftImage(ScrollBar.Position);
end;

procedure TBattleForm.eNameExit(Sender: TObject);
begin
  if eName.Modified then DataChange(Sender);
end;

procedure TBattleForm.eNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then DataChange(Sender);
end;

procedure TBattleForm.cmItemsDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TBattleForm.cmSkillsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TBattleForm.btnAddUnitClick(Sender: TObject);
begin
  if SelUnit <> nil then FillUnitInfo(nil, False);
  SelUnit := TBattleUnit.Create;
  SelUnit.Side := (TBitBtn(Sender).Tag = 1);
  SelUnit.Behind := True;
  SelUnit.Faction := Faction;
  Battle.Units[SelUnit.Side].Add(SelUnit);
  FillUnitInfo(SelUnit, False);
  DataChange(Sender);
end;

procedure TBattleForm.btnDelUnitClick(Sender: TObject);
var Units: TBattleUnitList;
begin
  if SelUnit <> nil then begin
    Units := Battle.Units[SelUnit.Side];
    Units.Delete(Units.IndexOf(SelUnit));
    if not SelUnit.Behind then Dec(NumFront[SelUnit.Side]);
    SelUnit := nil;
    FillUnitInfo(nil, False);
    RepaintBattle;
  end;
end;

procedure TBattleForm.btnAddItemClick(Sender: TObject);
var IData: TItemData;
    Skill: TSkill;
begin
  if (SelUnit <> nil) and (cmItems.ItemIndex >= 0) then begin
    IData := TItemData(cmItems.Items.Objects[cmItems.ItemIndex]);
    SelUnit.Items.Seek(IData.Short);
    // Add skills for monsters
    if Test(IData.Flags, IT_MONSTER) then begin
      if IData.Monster.Attack > 0 then begin
        Skill := SelUnit.Skills.Seek(Keys[s_Combat]);
        Skill.Level := IData.Monster.Attack;
      end;
      if IData.Monster.Tactics > 0 then begin
        Skill := SelUnit.Skills.Seek(Keys[s_Tactics]);
        Skill.Level := IData.Monster.Attack;
      end;
    end;
    FillUnitInfo(SelUnit, True);
    DataChange(Sender);
  end;
end;

procedure TBattleForm.btnDelItemClick(Sender: TObject);
begin
  if (SelUnit <> nil) and (ItemGrid.RowCount > 0) then begin
    SelUnit.Items[ItemGrid.Row].Free;
    SelUnit.Items.Delete(ItemGrid.Row);
    FillUnitInfo(SelUnit, True);
    DataChange(Sender);
  end;
end;

procedure TBattleForm.btnAddSkillClick(Sender: TObject);
var SData: TSkillData;
begin
  if (SelUnit <> nil) and (cmSkills.ItemIndex >= 0) then begin
    SData := TSkillData(cmSkills.Items.Objects[cmSkills.ItemIndex]);
    SelUnit.Skills.Seek(SData.Short);
    FillUnitInfo(SelUnit, True);
    if SData.Short = Keys[s_Tactics] then
      DrawTactics;
    DataChange(Sender);
  end;
end;

procedure TBattleForm.btnDelSkillClick(Sender: TObject);
begin
  if (SelUnit <> nil) and (SkillGrid.RowCount > 0) then begin
    if SelUnit.Skills[SkillGrid.Row].Data.Short = Keys[s_Tactics] then
      DrawTactics;
    SelUnit.Skills[SkillGrid.Row].Free;
    SelUnit.Skills.Delete(SkillGrid.Row);
    FillUnitInfo(SelUnit, True);
    DataChange(Sender);
  end;
end;

procedure TBattleForm.PainterKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_DELETE then btnDelUnitClick(Sender);
end;

procedure TBattleForm.RunSimulation(amt: integer);
var Sim: TSimulation;
    i, atk_win, def_win, res, divider: integer;
    side: boolean;
    U: TBattleUnit;
begin
  Screen.Cursor := crHourGlass;

  // Clear unit losses
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      U := Battle.Units[side][i];
      U.Loses := 0;
      U.Damaged := False;
    end;

  atk_win := 0;
  def_win := 0;

  Randomize;
  Sim := TSimulation.Create(Battle, False);

  // Run simulations
  for i := 1 to amt do begin
    res := Sim.Run;
    case res of
      0: Inc(atk_win);
      1: Inc(def_win);
    end;
    Sim.Restart;
  end;

  Sim.Free;

  // Convert losses to percents
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      U := Battle.Units[side][i];
      divider := U.Items.Amount(IT_MAN + IT_MONSTER) * amt;
      if divider > 0 then U.Loses := (U.Loses * 100) div divider
      else U.Loses := 0;
      U.Damaged := (U.Loses > 0);
    end;

  // Redraw all
  lAtkWin.Caption := IntToStr(atk_win * 100 div amt) + '% victory';
  lDefWin.Caption := IntToStr(def_win * 100 div amt) + '% victory';
  RepaintBattle;

  Screen.Cursor := crDefault;
end;

procedure TBattleForm.RunOneBattle;
var Sim: TSimulation;
    i: integer;
    side: boolean;
    U: TBattleUnit;
begin
  SetLength(Battle.Rounds, 0);
  SelRound := -1;

  // Clear unit losses
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      U := Battle.Units[side][i];
      U.Loses := 0;
      U.Damaged := False;
    end;

  Randomize;
  Sim := TSimulation.Create(Battle, True);
  Sim.Run;
  Sim.Free;

  // Convert losses to damage
  for side := sideAttack to sideDefence do
    for i := 0 to Battle.Units[side].Count-1 do begin
      U := Battle.Units[side][i];
      U.Damaged := (U.Loses > 0);
      U.Loses := 0;
    end;

  // Redraw all
  lAtkWin.Caption := '';
  lDefWin.Caption := '';
  RepaintBattle;
  lRound.Caption := '';
  trRounds.Max := Max(0, High(Battle.Rounds));
  trRounds.Position := 0;
  trRoundsChange(trRounds);
end;

procedure TBattleForm.btnStartSimClick(Sender: TObject);
begin
  if Sender is TMenuItem then RunSimulation(TMenuItem(Sender).Tag)
  else RunSimulation(100);
end;

procedure TBattleForm.cmStructsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  StructComboDrawItem(Control, Index, Rect);
end;

procedure TBattleForm.cmTerrainDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var B: TColor;
begin
  with cmTerrain.Canvas do begin
    FillRect(Rect);
    Pen.Color := clBlack;
    B := Brush.Color;
    Brush.Color := Config.ReadInteger('TerrainColors',
      cmTerrain.Items[Index], clWhite);
    Rectangle(Rect.Left + 2, Rect.Top + 2, Rect.Left + 16, Rect.Bottom - 2);
    Brush.Color := B;
    TextOut(Rect.Left + 20, Rect.Top + 2, cmTerrain.Items[Index]);
  end;
end;

procedure TBattleForm.cmTerrainChange(Sender: TObject);
var s: string;
begin
  Battle.Region.Terrain := TTerrainData(cmTerrain.Items.Objects[cmTerrain.ItemIndex]);
  s := '';
  if Test(Battle.Region.Terrain.Flags, TER_RIDINGMOUNTS) then s := s + 'riding';
  if Test(Battle.Region.Terrain.Flags, TER_FLYINGMOUNTS) then begin
    if s <> '' then s := s + ' and ';
    s := s + 'flying';
  end;
  if s = '' then s := 'no';
  lMounts.Caption := s + ' mounts used';
  SetupTerrain;
  SetupPictures;
  RepaintBattle;
end;

procedure TBattleForm.tbAddStructClick(Sender: TObject);
var St: TStruct;
begin
  if cmStructs.ItemIndex >= 0 then begin
    St := TStruct.Create;
    St.Data := TStructData(cmStructs.Items.Objects[cmStructs.ItemIndex]);
    Battle.Region.Structs.Add(St);
    FillStructs;
  end;
end;

procedure TBattleForm.tbDelStructClick(Sender: TObject);
var i: integer;
    side: boolean;
    Struct: TStruct;
begin
  if (gStructs.RowCount > 1) and (gStructs.Row > 0) then begin
    Struct := Battle.Region.Structs[gStructs.Row-1];
    // Clear struct from units inside
    for side := sideAttack to sideDefence do
      for i := 0 to Battle.Units[side].Count-1 do
        if Battle.Units[side][i].Struct = Struct then
          Battle.Units[side][i].Struct := nil;
    // Delete struct
    Battle.Region.Structs.Delete(gStructs.Row-1);
    Struct.Free;
    FillStructs;
  end;
end;

procedure TBattleForm.gStructsDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with TPowerGrid(Sender) do
    if (ARow > 0) and (ACol = 1) then begin
      DrawStructIcon(Canvas, TxtRect.Left, TxtRect.Top,
        TStructData(Rows[ARow].Data), False);
      Inc(TxtRect.Left, 18);
    end;
end;

procedure TBattleForm.cmInsideDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  StructComboDrawItem(Control, Index, Rect);
end;

procedure TBattleForm.cmSpellDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  SkillComboDrawItem(Control, Index, Rect);
end;

procedure TBattleForm.ItemFilterClick(Sender: TObject);
var Filter: DWord;
begin
  Filter := IT_MAN + IT_MONSTER + IT_WEAPON + IT_ARMOR + IT_MOUNT + IT_MAGIC;
  if TToolButton(Sender).Down then begin
    case TToolButton(Sender).Tag of
      0: Filter := IT_MAN;
      1: Filter := IT_MONSTER;
      2: Filter := IT_MOUNT;
      3: Filter := IT_WEAPON;
      4: Filter := IT_ARMOR;
      5: Filter := IT_MAGIC;
    end;
  end;
  FillItems(Filter, not TToolButton(Sender).Down or (Filter = IT_MAGIC));
end;

procedure TBattleForm.ItemGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if SelUnit <> nil then begin
    IDataGridSetEditText(Sender, Value, SelUnit.Items);
    DataChange(Sender);
  end;
end;

procedure TBattleForm.SkillGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
begin
  if SelUnit <> nil then begin
    SDataGridSetEditText(Sender, Value, SelUnit.Skills);
    DataChange(Sender);
  end;
end;

procedure TBattleForm.ItemGridDrawIcon(Sender: TObject; ACanvas: TCanvas;
  X, Y: Integer; Data: Pointer);
begin
  DrawItemIcon(ACanvas, X, Y, Data);
end;

procedure TBattleForm.btnSoldiersClick(Sender: TObject);
begin
  if SelUnit = nil then Exit;
  with TSoldiersForm.Create(Self) do begin
    Setup(SelUnit, Battle.Region);
    ShowModal;
    Free;
  end;
end;

procedure TBattleForm.PControlChange(Sender: TObject);
begin
  if (Battle <> nil) and (Battle.Name <> 'Simulation') then
    Config.WriteInteger('BattleViewer', 'Page', pControl.ActivePageIndex);
end;

procedure TBattleForm.trRoundsChange(Sender: TObject);
var Rnd: TRound;
begin
  if Length(Battle.Rounds) = 0 then SelRound := -1
  else SelRound := trRounds.Position;
  HalfPhase := -1;
  if SelRound >= 0 then begin
    Rnd := Battle.Rounds[SelRound];
    if Rnd.Num < 0 then lRound.Caption := 'conclusion'
    else if Rnd.Num = 0 then lRound.Caption := 'free round'
    else lRound.Caption := 'round ' + IntToStr(Rnd.Num);
  end
  else lRound.Caption := '';
end;

procedure TBattleForm.btnPlayClick(Sender: TObject);
begin
  if Mode = bmNormal then begin
    if btnSimulation.Down then RunOneBattle;
    Mode := bmPlay;
    SetupPlayControls(True);
    Timer := TTimer.Create(Self);
    if Sender is TMenuItem then Timer.Interval := TMenuItem(Sender).Tag
    else Timer.Interval := 100;
    Timer.OnTimer := PlayTimer;
  end
  else StopPlay;
end;

procedure TBattleForm.PlayTimer(Sender: TObject);
var allhalves: integer;
begin
  if SelRound = -1 then begin
    StopPlay;
    Exit;
  end;
  allhalves := Length(Battle.Rounds[SelRound].Actions) * subphases - 1;
  if (SelRound < High(Battle.Rounds)) or (HalfPhase < allhalves) then begin
    if HalfPhase < allhalves then begin
      Inc(HalfPhase);
      DrawBattleActions;
    end
    else begin
      trRounds.Position := trRounds.Position + 1;
      trRoundsChange(trRounds);
      RepaintBattle;
    end;
  end
  else StopPlay;
end;

procedure TBattleForm.SetupPlayControls(Value: boolean);
var i: integer;
begin
  if Value then btnPlay.ImageIndex := 42
  else btnPlay.ImageIndex := 67;
  PControl.Enabled := not Value;
  cmBattles.Enabled := not Value;
  btnSimulation.Enabled := not Value and (Battle.Name <> 'Simulation');
  btnStartSim.Enabled := not Value;
  trRounds.Enabled := not Value and not btnSimulation.Down;
  for i := 0 to PlayPopup.Items.Count-1 do
    PlayPopup.Items[i].Enabled := not Value;
end;

procedure TBattleForm.StopPlay;
begin
  FreeAndNil(Timer);
  trRounds.Position := 0;
  trRoundsChange(trRounds);
  RepaintBattle;
  Mode := bmNormal;
  SetupPlayControls(False);
end;

end.
