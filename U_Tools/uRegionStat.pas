unit uRegionStat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, Math, uGameSubs, Grids,
  AtlaDate, StdCtrls, MyStrings;

type
  TRegStatForm = class(TForm)
    Grid: TPowerGrid;
    Button1: TButton;
    cbNumbers: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
    procedure cbNumbersClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RegStatForm: TRegStatForm;

implementation

uses Types;

{$R *.dfm}

procedure TRegStatForm.FormCreate(Sender: TObject);
var i, row, j, k, wid: integer;
    R, RealR: TARegion;
begin
  cbNumbers.Checked := Config.ReadBool('RegionStats', 'Numbers', True);
  if CurrRegion <> nil then begin
    RealR := Map.Region(CurrRegion.Coords, Turn.Num);
    Caption := Caption + ' - ' + MakeRegionName(RealR.Coords, True);

    // Setup grid
    Grid.RowHeights[0] := 17;
    Grid.ColCount := Game.Turns.Count;
    wid := Grid.Cols[0].Width;
    for i := 1 to Grid.ColCount-1 do begin
      Grid.Cols[i].Width := (Grid.Width - wid - GetSystemMetrics(SM_CXVSCROLL)
         - 4) div (Grid.ColCount - i);
      Grid.Cells[i, 0] := TurnToShortDate(Game.Turns[i].Num);
      Inc(wid, Grid.Cols[i].Width);
    end;

    // Fill grid
    Grid.Cells[0, 1] := 'Population';
    Grid.Cells[0, 2] := 'Wages';
    Grid.Cells[0, 3] := 'Tax Rate';
    Grid.Cells[0, 4] := 'Entertainment';
    row := 5;
    if RealR.Wanted.Count > 0 then begin
      Grid.Cells[0, row] := 'Wanted';
      Grid.Rows[row].FontStyle := [fsBold];
      Inc(row);
      for i := 0 to RealR.Wanted.Count-1 do begin
        Grid.Cells[0, row] := RealR.Wanted[i].Data.Name(True);
        Inc(row);
      end;
    end;
    if RealR.ForSale.Count > 0 then begin
      Grid.Cells[0, row] := 'For Sale';
      Grid.Rows[row].FontStyle := [fsBold];
      Inc(row);
      for i := 0 to RealR.ForSale.Count-1 do begin
        Grid.Cells[0, row + 6] := RealR.ForSale[i].Data.Name(True);
        Inc(row);
      end;
    end;
    for i := 1 to Grid.ColCount-1 do begin
      R := Map.Region(RealR.Coords, Game.Turns[i].Num);
      if R = nil then Continue;
      if R.Peasants <> nil then Grid.Cells[i, 1] := IntToStr(R.Peasants.Amount)
      else Grid.Cells[i, 1] := '0';
      Grid.Cells[i, 2] := IntToStr(R.Wages);
      Grid.Cells[i, 3] := IntToStr(R.TaxRate);
      Grid.Cells[i, 4] := IntToStr(R.Entertainment);
      row := 6;
      for j := 0 to R.Wanted.Count-1 do begin
        k := RealR.Wanted.Count-1;
        while (k >= 0) and (RealR.Wanted[k].Data <> R.Wanted[j].Data) do
          Dec(k);
        if k >= 0 then Grid.Cells[i, row + k] := IntToStr(R.Wanted[j].Amount);
      end;
      row := row + RealR.Wanted.Count;
      if RealR.Wanted.Count > 0 then Inc(row);
      for j := 0 to R.ForSale.Count-1 do begin
        k := RealR.ForSale.Count-1;
        while (k >= 0) and (RealR.ForSale[k].Data <> R.ForSale[j].Data) do
          Dec(k);
        if k >= 0 then Grid.Cells[i, row + k] := IntToStr(R.ForSale[j].Amount);
      end;
    end;

    Grid.Fixup;
  end
  else Caption := Caption + ' - Unexplored region';
end;

procedure TRegStatForm.GridDrawCell(Sender: TObject; ACol, ARow: Integer;
  var TxtRect: TRect; State: TGridDrawState);
var i, maxval, sleft, stop, minval, val, hei: integer;
    s: string;
begin
  with Grid do begin
    if (ACol > 0) and (ARow > 0) and (ImgCells[ACol, ARow] <> '') then begin
      if cbNumbers.Checked and (Canvas.TextWidth(s) + 6 < TxtRect.Right -
        TxtRect.Left) then begin
        s := ImgCells[ACol, ARow];
        Canvas.Font.Color := clHighlight;
        Canvas.Font.Style := [fsBold];
        sleft := TxtRect.Left + ((TxtRect.Right - TxtRect.Left) div 2 -
          Canvas.TextWidth(s) div 2);
        stop := TxtRect.Top + ((TxtRect.Bottom - TxtRect.Top) div 2 -
          Canvas.TextHeight(s) div 2);
        Canvas.TextOut(sleft, stop, s);
      end;
      Canvas.Pen.Color := clHighlight;
      Canvas.Pen.Mode := pmMergePenNot;
      // Get edge values
      maxval := 0;
      minval := 99999999;
      for i := 1 to ColCount-1 do begin
        maxval := Max(maxval, ToInt(Cells[i, ARow]));
        minval := Min(minval, ToInt(Cells[i, ARow]));
      end;
      // Count percent
      minval := Max(0, maxval - Round(((maxval - minval) * 6 / 5)));
      if maxval - minval = 0 then
        hei := Min(maxval, (DefaultRowHeight - 7) div 5)
      else begin
        val := ToInt(Cells[ACol, ARow]);
        hei := Max(1, Round((DefaultRowHeight - 7) * ((val - minval) /
          (maxval - minval))));
      end;
      for i := TxtRect.Bottom - hei - 3 to TxtRect.Bottom - 3 do begin
        Canvas.MoveTo(TxtRect.Left + 1, i);
        Canvas.LineTo(TxtRect.Right, i);
      end;
      Canvas.Pen.Mode := pmCopy;
      TxtRect.Left := TxtRect.Right;
    end;
  end;
end;

procedure TRegStatForm.cbNumbersClick(Sender: TObject);
begin
  Grid.Repaint;
  Config.WriteBool('RegionStats', 'Numbers', cbNumbers.Checked);
end;

end.
