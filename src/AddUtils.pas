unit AddUtils;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  ComCtrls, StdCtrls;

type
  TAddTaskF = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ComboBox1: TComboBox;
    edt2: TEdit;
    Button1: TButton;
    Button2: TButton;
    edt1: TEdit;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    InfoLabel1: TLabel;
    edt3: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edt2KeyPress(Sender: TObject; var Key: Char);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AddTaskF: TAddTaskF;
  IsEdit: Boolean;
implementation
uses
  FOnTime;
{$R *.dfm}

procedure TAddTaskF.Button1Click(Sender: TObject);
var
  d: TDateTime;
  i: integer;
begin
  if trystrtoint(Edt1.Text, i) or TryStrToDateTime(Edt1.Text, d) then begin
    if IsEdit then frmOnTimer.ListView1.DeleteSelected;
    frmOnTimer.AddTaskItem(true, edt1.Text, ComboBox1.Text, edt2.Text, edt3.Text);
    Close;
  end
  else
    messagebox(handle, '时间格式有误!', '提示', MB_ICONERROR);
end;

procedure TAddTaskF.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TAddTaskF.FormShow(Sender: TObject);
begin
  if not IsEdit then edt1.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
end;

procedure TAddTaskF.edt2KeyPress(Sender: TObject; var Key: Char);
begin
  if word(Key) = VK_RETURN then Button1.Click;
end;

procedure TAddTaskF.CheckBox1Click(Sender: TObject);
begin
  if CheckBox1.Checked then begin
    edt1.Text := FormatDateTime('hh:mm:ss', now);
    edt1.MaxLength := 8;
    CheckBox2.Enabled := false;
  end
  else begin
    CheckBox2.Enabled := true;
    if CheckBox2.Checked then begin
      edt1.Text := '输入倒计时间(秒)';
      edt1.MaxLength := 0;
    end
    else begin
      edt1.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
      edt1.MaxLength := 19;
    end;
  end;
end;

procedure TAddTaskF.FormCreate(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(MText) do ComboBox1.Items.Add(MText[i]);
  ComboBox1.ItemIndex := 0;
end;

end.

