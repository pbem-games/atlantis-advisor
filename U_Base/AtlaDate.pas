unit AtlaDate;   { Date/Turn convertors }

interface

uses
  SysUtils, uKeys, Math;


 function TurnToShortDate(turn: integer): string;
 function TurnToDate(turn: integer): string;
 function ShortDateToTurn(date: string): integer;
 function MonthYearToTurn(month: string; year: integer): integer;

implementation

uses
  DataStructs;

function TurnToDate(turn: integer): string;
var year, month: integer;
begin
  year := (turn-1) div 12 + 1;
  month := ((turn-1) mod 12)+1;
  Result := GetKey(s_January, + month - 1) + ', ' + Keys[s_Year] + ' ' + IntToStr(year);
end;

function TurnToShortDate(turn: integer): string;
var year,month: integer;
begin
  year := Max(1, (turn-1) div 12 + 1);
  month := Max(1, ((turn-1) mod 12)+1);
  Result := GetKey(s_Jan, + month - 1) + ' ' + IntToStr(year);
end;

function ShortDateToTurn(date: string): integer;
var i,year,space: integer;
    month: string;
begin
  space := Pos(' ',date);
  month := Copy(date,0,space-1);
  i:=1;
  while (i < 12) and (GetKey(s_Jan, i - 1) <> month) do Inc(i);
  if GetKey(s_Jan, i - 1) <> month then Result := -1
  else
    try
      year := StrToInt(Copy(date,space+1,Length(date)-space));
      Result := (year-1)*12 + (i-1) + 1;
    except
      on EConvertError do Result := -1;
    end;
end;

function MonthYearToTurn(month: string; year: integer): integer;
var i: integer;
begin
  Result := (year-1)*12;
  for i:=1 to 12 do
    if month = GetKey(s_January, i - 1) then Result := Result + i;
end;

end.
