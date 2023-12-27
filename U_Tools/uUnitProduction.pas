unit uUnitProduction;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DataStructs, Resources, uGameSubs, StdCtrls, Grids,
  uKeys, Math;

type
  TUnitProductionForm = class(TForm)
    Grid: TPowerGrid;
    procedure FormCreate(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; ACol, ARow: Integer;
      var TxtRect: TRect; State: TGridDrawState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UnitProductionForm: TUnitProductionForm;

implementation

{$R *.dfm}

procedure TUnitProductionForm.FormCreate(Sender: TObject);
var i, row, amt, lv, maxout, turnout: integer;
    RealR: TARegion;
    Skill: TSkill;
begin
  if CurrUnit <> nil then begin
    Caption := Caption + ' - ' + CurrUnit.Name;

    Grid.Cells[0, 0] := 'Item';
    Grid.Cells[1, 0] := 'Size';
    Grid.Cols[1].Format := cfNumber;
    Grid.Cells[2, 0] := 'Max';
    Grid.Cols[2].Format := cfNumber;
    Grid.Cells[3, 0] := 'This turn';
    Grid.Cols[3].Format := cfNumber;

    // Items
    RealR := Map.Region(CurrRegion.Coords, Turn.Num);
    row := 1;
    for i := 0 to Game.ItemData.Count-1 do begin
      if ProduceOut(CurrUnit, RealR, Game.ItemData[i], maxout, turnout, -1,
        True) = prdOk then begin
        Grid.Cells[0, row] := IntToStr(i);
        Grid.Cells[2, row] := IntToStr(maxout);
        if turnout >= 0 then Grid.Cells[3, row] := IntToStr(turnout);
        Grid.Rows[row].Data := Game.ItemData[i];
        Inc(row);
      end;
    end;

    // Entertainment
    Skill := CurrUnit.Skills.Find(Keys[s_EntertainSkill]);
    if Skill <> nil then begin
      turnout := EntertainOut(CurrUnit);
      Grid.Cells[0, row] := IntToStr(Game.ItemData.IndexOf(SilverData));
      Grid.Cells[2, row] := IntToStr(turnout);
      Grid.Cells[3, row] := IntToStr(Min(RealR.Entertainment, turnout));
      Grid.Rows[row].Data := SilverData;
    end;

    // Buildings
    for i := 0 to Game.StructData.Count-1 do begin
      lv := BuildSkillLv(CurrUnit, Game.StructData[i]);
      if lv > 0 then begin
        amt := BuildMaterials(CurrUnit, Game.StructData[i]);
        maxout := CurrUnit.Items.Amount(IT_MAN) * lv;
        turnout := Min(maxout, amt);
        Grid.Cells[0, row] := IntToStr(i + Game.ItemData.Count);
        Grid.Cells[1, row] := IntToStr(Game.StructData[i].Size);
        Grid.Cells[2, row] := IntToStr(maxout);
        Grid.Cells[3, row] := IntToStr(turnout);
        Grid.Rows[row].Data := Game.StructData[i];
        Inc(row);
      end;
    end;

    Grid.Fixup;
  end
  else Caption := Caption + ' - No unit';
end;

procedure TUnitProductionForm.GridDrawCell(Sender: TObject; ACol,
  ARow: Integer; var TxtRect: TRect; State: TGridDrawState);
begin
  with Sender as TPowerGrid do
    if (ACol = 0) and (ARow > 0) then begin
      if TObject(ImgRows[ARow].Data) is TItemData then begin
        Canvas.TextRect(TxtRect, TxtRect.Left + 18, TxtRect.Top + 2,
          TItemData(ImgRows[ARow].Data).Name(True));
        DrawItemIcon(Canvas, TxtRect.Left, TxtRect.Top,
          TItemData(ImgRows[ARow].Data));
      end
      else if TObject(ImgRows[ARow].Data) is TStructData then begin
        Canvas.TextRect(TxtRect, TxtRect.Left + 18, TxtRect.Top + 2,
          TStructData(ImgRows[ARow].Data).Group);
        DrawStructIcon(Canvas, TxtRect.Left, TxtRect.Top,
          TStructData(ImgRows[ARow].Data), False);
      end;
      TxtRect.Left := TxtRect.Right;
    end;
end;

end.
