unit frmCountdown_u;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Proc_u, FuncLib;

type
  TShutdownType = (
    stShutdown,
    stReboot,
    stLogout,
    stLock,
    stSuspend);

const
  SHUTDOWN_TYPE_STR : array[TShutdownType] of string = (
    '关闭系统',
    '重启系统',
    '注销登陆',
    '锁定系统',
    '系统待机'
    );

type
  TfrmCountdown = class(TForm)
    tmr1: TTimer;
    pb1: TProgressBar;
    procedure tmr1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pb1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FMsg: string;
    FShutdownType: TShutdownType;

    FCountdownTime: Integer;            //关机倒计时 秒
    FCountdownPause: Boolean;           //暂停计时
  public
    class procedure Countdown(st: TShutdownType; Time: Integer; Msg: string);
  end;

var
  frmCountdown      : TfrmCountdown;

function DoShutdown(st: TShutdownType): LongBool;
implementation

function SetSuspendState(Hibernate, ForceCritical, DisableWakeEvent: Boolean): Boolean;
  stdcall; external 'PowrProf.dll'
{$R *.dfm}

function DoShutdown(st: TShutdownType): LongBool;
begin
  if st = stLock then
    LockWorkStation
  else
  begin
    SetPrivilege('SeShutdownPrivilege');
    case st of
      stShutdown:
        ExitWindowsEX(EWX_SHUTDOWN or EWX_FORCE, 0); {强制关机}

      stReboot:
        ExitWindowsEX(EWX_REBOOT or EWX_FORCE, 0); {重启}

      stLogout:
        ExitWindowsEX(EWX_LOGOFF or EWX_FORCE, 0); {注销}

      stSuspend:
        SetSuspendState(False, False, False); {待机}
    end;
  end;
end;

{ TfrmCountdown }

class procedure TfrmCountdown.Countdown(st: TShutdownType; Time: Integer;
  Msg: string);
begin
  if Time > 0 then
  begin
    with TfrmCountdown.Create(Application) do
    begin
      FShutdownType := st;
      FCountdownTime := Time;
      FMsg := Msg;

      pb1.Max := Time;

      tmr1.Enabled := True;
      Show;
    end;
  end
  else
    DoShutdown(st);
end;

procedure TfrmCountdown.tmr1Timer(Sender: TObject);
begin
  if FCountdownPause then
    Exit;

  if FCountdownTime > 0 then
  begin
    pb1.StepIt;

    Caption := MSecondToTimeStr(FCountdownTime * 1000) + '后'
      + SHUTDOWN_TYPE_STR[FShutdownType] + '!';

    Dec(FCountdownTime);
  end
  else
  begin
    tmr1.Enabled := False;
    Close;
    DoShutdown(FShutdownType);
  end;
end;

procedure TfrmCountdown.FormCreate(Sender: TObject);
begin
  pb1.Step := 1;
  pb1.DoubleBuffered := True;

  Top := 0;
  Left := 0;
  Width := GetSystemMetrics(SM_CXSCREEN);
  Height := GetSystemMetrics(SM_CYSCREEN) div 5;
end;

procedure TfrmCountdown.pb1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  s                 : string;
begin
  FCountdownPause := True;
  s := '是否取消' + SHUTDOWN_TYPE_STR[FShutdownType] + '？';
  if MessageBox(Handle,
    PChar(s),
    '询问',
    MB_YESNO or MB_ICONQUESTION) = ID_YES then
  begin
    Close;
    Exit;
  end;
  FCountdownPause := False;
end;

procedure TfrmCountdown.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Free;
end;

end.

