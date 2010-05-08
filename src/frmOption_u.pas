unit frmOption_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  StdCtrls, Controls, Spin;

type
  TfrmOption = class(TForm)
    grpSBox: TGroupBox;
    InfoLabel2: TLabel;
    InfoLabel3: TLabel;
    InfoLabel4: TLabel;
    edtServer: TEdit;
    edtUser: TEdit;
    edtPass: TEdit;
    sePort: TSpinEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure edtPassKeyPress(Sender: TObject; var Key: Char);
  private
    isPassChanged: Boolean;
  public
    { Public declarations }
  end;

var
  frmOption         : TfrmOption;

implementation
uses
  TaskMgr_u, Proc_u;
{$R *.dfm}

procedure TfrmOption.btnOkClick(Sender: TObject);
begin
  g_SMTPOption.Server := edtServer.Text;
  g_SMTPOption.Port := sePort.Value;
  g_SMTPOption.UserName := edtUser.Text;
  if isPassChanged then
    g_SMTPOption.Password := edtPass.Text;
  g_TaskMgr.UpdateOption;
end;

procedure TfrmOption.FormCreate(Sender: TObject);
begin
  edtServer.Text := g_SMTPOption.Server;
  sePort.Value := g_SMTPOption.Port;
  edtUser.Text := g_SMTPOption.UserName;
  isPassChanged := False;
  if g_SMTPOption.Password <> '' then edtPass.Text := '******';
end;

procedure TfrmOption.FormPaint(Sender: TObject);
begin
  DrawRoundForm(Handle, Width, Height, $00CD746D);
end;

procedure TfrmOption.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  //发送系统消息，通知窗口标题栏被按下，之后就可以拖了
  SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
end;

procedure TfrmOption.edtPassKeyPress(Sender: TObject; var Key: Char);
begin
  if not isPassChanged then begin
    edtPass.Clear;
    isPassChanged := True;
  end;
end;

end.
