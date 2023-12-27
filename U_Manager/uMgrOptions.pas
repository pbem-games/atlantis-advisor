unit uMgrOptions;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType, LMessages, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin, Resources, EditBtn;

type
  TManagerOptionsForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Label18: TLabel;
    eEngineFile: TFileNameEdit;
    GroupBox2: TGroupBox;
    Label2: TLabel;
    KeepEdit: TSpinEdit;
    Label3: TLabel;
    MoveRepCheck: TCheckBox;
    cbLastOrder: TCheckBox;
    PromptWrongCheck: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  ManagerOptionsForm: TManagerOptionsForm;

implementation

{$R *.lfm}

procedure TManagerOptionsForm.FormCreate(Sender: TObject);
begin
  KeepEdit.Value := Config.ReadInteger('Game Manager', 'KeepReps', 10);
  MoveRepCheck.Checked := Config.ReadBool('Game Manager', 'MoveReps', TRUE);
  PromptWrongCheck.Checked := Config.ReadBool('Game Manager', 'PromptRemWrong', TRUE);
  cbLastOrder.Checked := Config.ReadBool('Game Manager', 'LoadLastOrder', TRUE);
  eEngineFile.Text := Config.ReadString('Engine', 'EngineFile', '');
end;

procedure TManagerOptionsForm.Button1Click(Sender: TObject);
begin
  Config.WriteInteger('Game Manager', 'KeepReps', KeepEdit.Value);
  Config.WriteBool('Game Manager', 'MoveReps', MoveRepCheck.Checked);
  Config.WriteBool('Game Manager', 'PromptRemWrong', PromptWrongCheck.Checked);
  Config.WriteBool('Game Manager', 'LoadLastOrder', cbLastOrder.Checked);
  Config.WriteString('Engine', 'EngineFile', eEngineFile.Text);
end;

end.
