unit uUnitArmies;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, StdCtrls, Grids, PowerGrid, ComCtrls,
  ToolWin, ColorBtn, Math;

type
  TUArmyForm = class(TForm)
    Grid: TPowerGrid;
    Button1: TButton;
    ToolBar1: TToolBar;
    btnAddForce: TToolButton;
    btnDelForce: TToolButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    eName: TEdit;
    Label2: TLabel;
    btnColor: TColorBtn;
    ColorDialog: TColorDialog;
    gUnits: TPowerGrid;
    procedure FormCreate(Sender: TObject);
    procedure btnAddForceClick(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DataChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: Char);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure gUnitsDblClick(Sender: TObject);
    procedure btnDelForceClick(Sender: TObject);
  private
    Filling: boolean;
    procedure FillGrid;
  public
    { Public declarations }
  end;

var
  UArmyForm: TUArmyForm;

implementation

uses
  Main;

{$R *.lfm}

procedure TUArmyForm.FormCreate(Sender: TObject);
begin
  gUnits.Cells[0, 0] := 'Unit';
  gUnits.Cells[1, 0] := 'Soldiers';
  gUnits.Cols[1].Format := cfNumber;
  FillGrid;
end;

procedure TUArmyForm.FillGrid;
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to Game.UArmies.Count-1 do begin
    Grid.Cells[0, i] := Game.UArmies[i].Name;
    Grid.Rows[i].Data := Game.UArmies[i];
  end;
  Grid.Fixup;
end;

procedure TUArmyForm.btnAddForceClick(Sender: TObject);
var UArmy: TUArmy;
begin
  UArmy := TUArmy.Create('New Army');
  Game.UArmies.Add(UArmy);
  FillGrid;
end;

procedure TUArmyForm.btnDelForceClick(Sender: TObject);
var i: integer;
    A: TUArmy;
begin
  if Grid.Row >= 0 then begin
    A := TUArmy(Grid.ImgRows[Grid.Row].Data);
    for i := 0 to Faction.Units.Count-1 do
      if Faction.Units[i].UArmy = A then Faction.Units[i].UArmy := nil;
    Game.UArmies.Delete(Game.UArmies.IndexOf(A));
    FillGrid;
  end;
end;

procedure TUArmyForm.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var F: TUArmy;
    row, i: integer;
    U: TUnit;
begin
  Filling := True;
  F := TUArmy(Grid.ImgRows[ARow].Data);
  eName.Text := F.Name;
  btnColor.Color := F.Color;

  // Units
  gUnits.RowCount := 1;
  row := 1;
  for i := 0 to F.UnitIds.Count-1 do begin
    U := VFaction.Units.FindById(F.UnitIds[i]);
    if U = nil then Continue;
    gUnits.Cells[0, row] := U.Name + ' (' + U.NumStr + ')';
    gUnits.Cells[1, row] := IntToStr(U.Items.Amount(IT_MAN + IT_MONSTER));
    gUnits.Rows[row].Data := U;
    Inc(row);
  end;
  gUnits.Fixup;

  Filling := False;
end;

procedure TUArmyForm.DataChange(Sender: TObject);
var F: TUArmy;
begin
  if Filling or (Grid.RowCount = 0) then Exit;
  F := TUArmy(Grid.ImgRows[Grid.Row].Data);
  F.Name := eName.Text;
  F.Color := btnColor.Color;
  Grid.ImgCells[0, Grid.Row] := F.Name;
  Grid.Invalidate;
end;

procedure TUArmyForm.eNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then DataChange(Sender);
end;

procedure TUArmyForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var Bmp: TBitmap;
    Cl: TColor;
begin
  with Grid do begin
    Cl := TUArmy(ImgRows[ARow].Data).Color;
    if Cl = -1 then Cl := FactionColor(Faction);
    Bmp := GetMaskedBmp(ResForm.IconList, bmpForce div 2, Cl);
    Canvas.Draw(TxtRect.Left, TxtRect.Top, Bmp);
    Bmp.Free;
    Inc(TxtRect.Left, 18);
  end;
end;

procedure TUArmyForm.gUnitsDblClick(Sender: TObject);
begin
  if gUnits.Row > 0 then begin
    MainForm.HexMapGoto(TUnit(gUnits.ImgRows[gUnits.Row].Data).Region.Coords);
    Close;
  end;
end;

end.
