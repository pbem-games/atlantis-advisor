unit uNewGame;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Resources;

type
  TNewGameForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    GameNameEdit: TEdit;
    Label2: TLabel;
    cmRuleset: TComboBox;
    cbLocal: TCheckBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewGameForm: TNewGameForm;

implementation

{$R *.lfm}

procedure TNewGameForm.FormCreate(Sender: TObject);
var sr: TSearchRec;
begin
  // Look for rulesets
  if FindFirst(BaseDir + RuleFolder + '*.dat', faAnyFile, sr) = 0 then begin
    cmRuleset.Items.Add(Copy(sr.Name, 1, Pos('.', sr.Name)-1));
    while FindNext(sr) = 0 do
      cmRuleset.Items.Add(Copy(sr.Name, 1, Pos('.', sr.Name)-1));
  end;
  FindClose(sr);
  if cmRuleset.Items.Count = 0 then
    MessageDlg('Rulesets not found', mtError, [mbOk], 0)
  else cmRuleset.ItemIndex := 0;
  cbLocal.Enabled := (Config.ReadString('Engine', 'EngineFile', '') <> '');
end;

end.
