unit uMapExport;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TMapExportForm = class(TForm)
    cbAdvances: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    cbBuildings: TCheckBox;
    cbTransports: TCheckBox;
    Label1: TLabel;
    cbPlayerUnits: TCheckBox;
    cbOtherUnits: TCheckBox;
    cbDetails: TCheckBox;
    rbMap: TRadioButton;
    rbRegion: TRadioButton;
    cbGates: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MapExportForm: TMapExportForm;

implementation

{$R *.lfm}

end.
