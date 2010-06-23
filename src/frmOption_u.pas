unit frmOption_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Forms,
  StdCtrls, Controls, Spin, ComCtrls;

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
    hk1: THotKey;
    lbl1: TLabel;
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
  g_Option.SmtpServer := edtServer.Text;
  g_Option.SmtpPort := sePort.Value;
  g_Option.SmtpUser := edtUser.Text;
  if isPassChanged then
    g_Option.SmtpPass := edtPass.Text;
  g_Option.ShortCut := hk1.HotKey;
  g_TaskMgr.UpdateOption;
end;

procedure TfrmOption.FormCreate(Sender: TObject);
begin
  edtServer.Text := g_Option.SmtpServer;
  sePort.Value := g_Option.SmtpPort;
  edtUser.Text := g_Option.SmtpUser;
  hk1.HotKey := g_Option.ShortCut;
  isPassChanged := False;
  if g_Option.SmtpPass <> '' then edtPass.Text := '******';
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

