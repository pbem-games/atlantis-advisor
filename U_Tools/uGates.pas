unit uGates;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Grids, uGameSubs;

type
  TGatesForm = class(TForm)
    Grid: TPowerGrid;
    procedure FormCreate(Sender: TObject);
    procedure GridDblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  GatesForm: TGatesForm;

implementation

{$R *.dfm}

uses Main;

procedure TGatesForm.FormCreate(Sender: TObject);
var z, x, y, row: integer;
    R: TARegion;
begin
  Grid.Cells[0, 0] := 'Num';
  Grid.Cols[0].Format := cfNumber;
  Grid.Cells[1, 0] := 'Region';

  row := 1;
  for z := 0 to Map.Levels.Count-1 do
    for y := Map.Levels[z].Bounds.Top to Map.Levels[z].Bounds.Bottom do
      for x := Map.Levels[z].Bounds.Left to Map.Levels[z].Bounds.Right do begin
        R := Map.Region(x, y, z);
        if (R = nil) or (R.Gate = 0) then Continue;
        if R.Gate > 0 then
          Grid.Cells[0, row] := IntToStr(R.Gate);
        Grid.Cells[1, row] := MakeRegionName(R.Coords, True);
        Grid.Rows[row].Data := R;
        Inc(row);
      end;
  Grid.Fixup;
end;

procedure TGatesForm.GridDblClick(Sender: TObject);
var R: TARegion;
begin
  with Sender as TPowerGrid do
    R := TARegion(ImgRows[Row].Data);
  if R <> nil then begin
    MainForm.HexMapGoto(R.Coords);
    Close;
  end;
end;

end.
