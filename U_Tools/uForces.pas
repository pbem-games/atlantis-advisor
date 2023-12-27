unit uForces;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, StdCtrls, Grids, ComCtrls,
  ToolWin, ColorBtn, Math;

type
  TForceForm = class(TForm)
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
    procedure FormCreate(Sender: TObject);
    procedure btnAddForceClick(Sender: TObject);
    procedure GridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure DataChange(Sender: TObject);
    procedure eNameKeyPress(Sender: TObject; var Key: Char);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
  private
    Filling: boolean;
    procedure FillGrid;
  public
    { Public declarations }
  end;

var
  ForceForm: TForceForm;

implementation

{$R *.dfm}

procedure TForceForm.FormCreate(Sender: TObject);
begin
  FillGrid;
end;

procedure TForceForm.FillGrid;
var i: integer;
begin
  Grid.RowCount := 0;
  for i := 0 to Game.Forces.Count-1 do begin
    Grid.Cells[0, i] := Game.Forces[i].Name;
    Grid.Rows[i].Data := Game.Forces[i];
  end;
  Grid.Fixup;
end;

procedure TForceForm.btnAddForceClick(Sender: TObject);
var Force: TForce;
begin
  Force := TForce.Create('New Army');
  Game.Forces.Add(Force);
  FillGrid;
end;

procedure TForceForm.GridSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
var F: TForce;
begin
  Filling := True;
  F := TForce(Grid.ImgRows[ARow].Data);
  eName.Text := F.Name;
  if F.Color = 0 then btnColor.Color := -1
  else btnColor.Color := F.Color;
  Filling := False;
end;

procedure TForceForm.DataChange(Sender: TObject);
var F: TForce;
begin
  if Filling or (Grid.RowCount = 0) then Exit;
  F := TForce(Grid.ImgRows[Grid.Row].Data);
  F.Name := eName.Text;
  F.Color := Max(0, btnColor.Color);
  Grid.Cells[0, Grid.Row] := F.Name;
  Grid.Invalidate;
end;

procedure TForceForm.eNameKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then DataChange(Sender);
end;

procedure TForceForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var Bmp: TBitmap;
    Cl: TColor;
begin
  with Grid do begin
    Cl := TForce(Rows[ARow].Data).Color;
    if Cl = 0 then Cl := FactionColor(Faction);
    Bmp := GetMaskedBmp(ResForm.IconList, bmpForce div 2, Cl);
    Canvas.Draw(TxtRect.Left, TxtRect.Top, Bmp);
    Bmp.Free;
    Inc(TxtRect.Left, 18);
  end;
end;

end.
