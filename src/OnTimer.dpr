program OnTimer;

uses
  Windows,
  Forms,
  SysUtils,
  frmOnTimer_u in 'frmOnTimer_u.pas' {frmOnTimer},
  frmAddTask_u in 'frmAddTask_u.pas' {frmAddTask},
  frmOption_u in 'frmOption_u.pas' {frmOption},
  frmAbout_u in 'frmAbout_u.pas' {frmAbout},
  TaskMgr_u in 'TaskMgr_u.pas',
  TooltipUtil in 'TooltipUtil.pas',
  DB_u in 'DB_u.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'HOU任务计划 v1.3b';
  if FindCmdLineSwitch('h', ['/', '-'], True) then
    Application.ShowMainForm := False
  else if FindCmdLineSwitch('h-all', ['/', '-'], True) then
  begin
    g_IsHideTray := True;
    Application.ShowMainForm := False;
  end;
  Application.CreateForm(TfrmOnTimer, frmOnTimer);
  Application.Run;
end.

