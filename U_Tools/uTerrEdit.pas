unit uTerrEdit;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataStructs, Resources, StdCtrls, Spin,
  ColorBtn, ComCtrls, ToolWin, uGameSubs, uInterface;

type
  TTerrEditForm = class(TForm)
    Grid: TStringGrid;
    ColorDialog: TColorDialog;
    Button1: TButton;
    gbProps: TGroupBox;
    Label1: TLabel;
    eMoveCost: TSpinEdit;
    cbWater: TCheckBox;
    cbRiding: TCheckBox;
    cbFlying: TCheckBox;
    Label3: TLabel;
    AdvGrid: TStringGrid;
    cmAdv: TComboBox;
    ToolBar1: TToolBar;
    btnAddAdv: TToolButton;
    btnDelAdv: TToolButton;
    Label2: TLabel;
    ColorBtn: TColorBtn;
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cmAdvDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure AdvGridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure DataChange(Sender: TObject);
    procedure btnAddAdvClick(Sender: TObject);
    procedure btnDelAdvClick(Sender: TObject);
    procedure ColorBtnClick(Sender: TObject);
  private
    Filling: boolean;
    procedure FillData(ARow: integer);
  public
    Modified: boolean;
  end;

var
  TerrEditForm: TTerrEditForm;

implementation

{$R *.lfm}

procedure TTerrEditForm.FormCreate(Sender: TObject);
var i: integer;
begin
  // FIXME: broken
  //Grid.Cells[0, 0] := 'unknown';
  //for i := 0 to Game.TerrainData.Count-1 do begin
  //  Grid.Cells[0, i+1] := Game.TerrainData[i].Name;
  //  Grid.Rows[i+1].Data := Game.TerrainData[i];
  //end;
  //Grid.Fixup;
  //// Resources
  //cmAdv.Items.Clear;
  //for i := 0 to Game.ItemData.Count-1 do
  //  if Test(Game.ItemData[i].Flags, IT_RESOURCE)
  //    and Test(Game.ItemData[i].Flags, IT_ADVANCED) then
  //    cmAdv.AddItem(Game.ItemData[i].Name(True), Game.ItemData[i]);
end;

procedure TTerrEditForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var BColor: TColor;
begin
  // FIXME: broken
  //with Grid.Canvas do begin
  //  Pen.Color := clBlack;
  //  BColor := Brush.Color;
  //  Brush.Color := Config.ReadInteger('TerrainColors',
  //    Grid.ImgCells[0, ARow], clWhite);
  //  Rectangle(TxtRect.Left + 2, TxtRect.Top + 2, TxtRect.Left + 16, TxtRect.Bottom - 2);
  //  Brush.Color := BColor;
  //  TxtRect.Left := TxtRect.Left + 20;
  //end;
end;

procedure TTerrEditForm.GridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  FillData(ARow);
end;

procedure TTerrEditForm.FillData(ARow: integer);
var Ter: TTerrainData;
    i: integer;
begin
  // FIXME: broken
  //Filling := True;
  //
  //ColorBtn.Color := Config.ReadInteger('TerrainColors',
  //  Grid.ImgCells[0, ARow], clWhite);
  //
  //Ter := TTerrainData(Grid.ImgRows[ARow].Data);
  //if Ter <> nil then begin
  //  eMoveCost.Value := Ter.MoveCost;
  //  cbWater.Checked := Test(Ter.Flags, TER_WATER);
  //  cbRiding.Checked := Test(Ter.Flags, TER_RIDINGMOUNTS);
  //  cbFlying.Checked := Test(Ter.Flags, TER_FLYINGMOUNTS);
  //
  //  AdvGrid.RowCount := 0;
  //  for i := 0 to Ter.AdvResources.Count-1 do begin
  //    AdvGrid.Cells[0, i] := Ter.AdvResources[i].Name(True);
  //    AdvGrid.Rows[i].Data := Ter.AdvResources[i];
  //  end;
  //  AdvGrid.Fixup;
  //  gbProps.Enabled := True;
  //end
  //else begin
  //  eMoveCost.Value := 0;
  //  cbWater.Checked := False;
  //  cbRiding.Checked := False;
  //  cbFlying.Checked := False;
  //  AdvGrid.RowCount := 0;
  //  AdvGrid.Fixup;
  //  gbProps.Enabled := False;
  //end;
  //
  //Filling := False;
end;

procedure TTerrEditForm.DataChange(Sender: TObject);
var Ter: TTerrainData;
begin
  // FIXME: broken
  //if Filling then Exit;
  //Ter := TTerrainData(Grid.ImgRows[Grid.Row].Data);
  //Ter.MoveCost := eMoveCost.Value;
  //SetFlag(Ter.Flags, TER_WATER, cbWater.Checked);
  //SetFlag(Ter.Flags, TER_RIDINGMOUNTS, cbRiding.Checked);
  //SetFlag(Ter.Flags, TER_FLYINGMOUNTS, cbFlying.Checked);
  //Modified := True;
end;

procedure TTerrEditForm.btnAddAdvClick(Sender: TObject);
var Ter: TTerrainData;
begin
  // FIXME: broken
  //if cmAdv.ItemIndex = -1 then Exit;
  //Ter := TTerrainData(Grid.ImgRows[Grid.Row].Data);
  //if Ter.AdvResources.IndexOf(cmAdv.Items.Objects[cmAdv.ItemIndex]) = -1 then
  //  Ter.AdvResources.Add(cmAdv.Items.Objects[cmAdv.ItemIndex]);
  //FillData(Grid.Row);
end;

procedure TTerrEditForm.btnDelAdvClick(Sender: TObject);
var Ter: TTerrainData;
begin
  // FIXME: broken
  //if AdvGrid.Row = -1 then Exit;
  //Ter := TTerrainData(Grid.ImgRows[Grid.Row].Data);
  //Ter.AdvResources.Delete(AdvGrid.Row);
  //FillData(Grid.Row);
end;

procedure TTerrEditForm.ColorBtnClick(Sender: TObject);
var Ter: TTerrainData;
begin
  // FIXME: broken
  //Ter := TTerrainData(Grid.ImgRows[Grid.Row].Data);
  //Config.WriteInteger('TerrainColors', Grid.ImgCells[0, Grid.Row], ColorBtn.Color);
  //if Ter <> nil then Ter.Color := ColorBtn.Color;
  //Grid.Repaint;
end;

procedure TTerrEditForm.cmAdvDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
begin
  ItemDataComboDrawItem(Control, Index, Rect);
end;

procedure TTerrEditForm.AdvGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  // FIXME: broken
  //with TStringGrid(Sender) do begin
  //  DrawItemIcon(Canvas, TxtRect.Left+1, TxtRect.Top, TItemData(ImgRows[ARow].Data));
  //  TxtRect.Left := TxtRect.Left + 18;
  //end;
end;


end.
