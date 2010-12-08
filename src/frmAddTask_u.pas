unit frmAddTask_u;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  ComCtrls, StdCtrls, Spin, TaskMgr_u, TooltipUtil;

type
  TfrmAddTask = class(TForm)
    grpTask: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    cbbType: TComboBox;
    seExecNum: TSpinEdit;
    btnOk: TButton;
    btnCancel: TButton;
    edtTime: TEdit;
    chkEveryDay: TCheckBox;
    chkLoop: TCheckBox;
    InfoLabel1: TLabel;
    lbl1: TLabel;
    lblParam: TLabel;
    edtParam: TEdit;
    chkWeek: TCheckBox;
    chkMon: TCheckBox;
    chkTue: TCheckBox;
    chkWed: TCheckBox;
    chkThu: TCheckBox;
    chkFri: TCheckBox;
    chkSat: TCheckBox;
    chkSun: TCheckBox;
    chkTmpExecNum: TCheckBox;
    chkMonthly: TCheckBox;
    lbl2: TLabel;
    cbbClass: TComboBox;
    mmoContent: TMemo;
    chkActive: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure chkEveryDayClick(Sender: TObject);
    procedure cbbTypeChange(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure chkWeekClick(Sender: TObject);
    procedure mmoContentEnter(Sender: TObject);
    procedure edtParamEnter(Sender: TObject);
    procedure mmoContentExit(Sender: TObject);
  private
    FTask: TTask;
    FToolTip: TToolTip;
    FContentTip: string;
    FParamTip: string;
  public
    constructor Create(Task: TTask);
  end;

var
  frmAddTask        : TfrmAddTask;

implementation
uses
  Proc_u;
{$R *.dfm}

constructor TfrmAddTask.Create(Task: TTask);
var
  i                 : Integer;
  weekSet           : TWeekSet;

  SelItem           : TListItem;
  SelNode           : TTreeNode;
begin
  inherited Create(Application);
  FTask := Task;

  FToolTip := TToolTip.Create(Self);
  FToolTip.Interval := 5000;

  SelItem := g_TaskMgr.Lv.Selected;
  SelNode := g_TaskMgr.Classes.Tv.Selected;

  //类型
  cbbType.Clear;
  for i := 0 to Integer(High(TTaskType)) do
    cbbType.Items.Add(TASK_TYPE_STR[TTaskType(i)]);
  cbbType.ItemIndex := 0;

  //分类
  cbbClass.Clear;
  cbbClass.AddItem('未分类', TObject(0));
  with g_TaskMgr.Classes.ClassNode[tcByClass] do
    for i := 0 to Count - 1 do
      cbbClass.AddItem(Item[i].Text, Item[i].Data);
  cbbClass.ItemIndex := 0;

  { 任务数据 }
  if Assigned(Task) then                //Edit
  begin
    chkActive.Checked := Task.Active;
    cbbType.ItemIndex := Integer(Task.TaskType);
    cbbClass.ItemIndex := cbbClass.Items.IndexOfObject(TObject(Task.CId));
    seExecNum.Value := Task.ExecNum;
    chkTmpExecNum.Checked := Task.TmpExecNum;

    case Task.TimeType of
      tmLoop, tmTime:
        begin
          if Task.TimeType = tmLoop then
          begin
            chkLoop.Checked := True;
            edtTime.Text := IntToStr(Task.TimeRec.TimeOrLoop);
          end
          else
          begin
            chkEveryDay.Checked := True;
            edtTime.Text := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongTimeFormat,
              TimeStampToDateTime(PTimeStamp(@Task.TimeRec)^));
          end;
          { 星期数据要晚时间数据设置！！ }
          if Task.IsWeek then
          begin
            chkWeek.Checked := True;
            weekSet := PWeekSet(@Task.TimeRec.DateOrWeek)^;
            chkMon.Checked := wdMon in weekSet;
            chkTue.Checked := wdTue in weekSet;
            chkWed.Checked := wdWed in weekSet;
            chkThu.Checked := wdThu in weekSet;
            chkFri.Checked := wdFri in weekSet;
            chkSat.Checked := wdSat in weekSet;
            chkSun.Checked := wdSun in weekSet;
          end;
        end;

      tmDateTime:
        begin
          edtTime.Text := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongDateFormat,
            TimeStampToDateTime(PTimeStamp(@Task.TimeRec)^));
        end;

      tmMonthly:
        begin
          chkMonthly.Checked := True;
          edtTime.Text := IntToStr(Task.TimeRec.DateOrWeek) + ' '
            + FormatDateTime(DATETIME_FORMAT_SETTINGS.LongTimeFormat,
            TimeStampToDateTime(PTimeStamp(@Task.TimeRec)^));
        end;
    end;

    edtParam.Text := Task.Param;
    mmoContent.Text := Task.Content;
  end
  else                                  //Add
  begin
    edtTime.Text := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongDateFormat, now);

    //Default
    if SelItem <> nil then
    begin
      cbbType.ItemIndex := Integer(TTask(SelItem.Data).TaskType);
      cbbClass.ItemIndex := cbbClass.Items.IndexOfObject(TObject(TTask(SelItem.Data).CId));
    end;

    if (SelNode <> nil) then
      if (SelNode.Parent = g_TaskMgr.Classes.ClassNode[tcByType]) then //按类型
        cbbType.ItemIndex := Integer(SelNode.Data)
      else if (SelNode.Parent = g_TaskMgr.Classes.ClassNode[tcByClass]) then //按分类
        cbbClass.ItemIndex := cbbClass.Items.IndexOfObject(SelNode.Data);
  end;

  cbbTypeChange(cbbType);
end;

procedure TfrmAddTask.btnOkClick(Sender: TObject);
var
  S                 : string;
  n                 : Integer;
  bErr, isAdd       : Boolean;
  timeType          : TTimeType;
  dateTime          : TDateTime;
  timeRec           : TTimeRec;
  weekSet           : TWeekSet;

  Item              : TListItem;
begin
  bErr := False;

  timeType := tmDateTime;
  if chkLoop.Checked then
  begin                                 { 倒计时 }
    if not TryStrToInt(edtTime.Text, Integer(timeRec.TimeOrLoop)) then
      bErr := true;
    //    if loopTime > MAX_LOOP_VALUE then begin
    //      FToolTip.Popup(edtTime.Handle, ttWarningIcon,
    //        '提示', '计时不能大于' + IntToStr(MAX_LOOP_VALUE) + '秒!');
    //      Exit;
    //    end;
    timeType := tmLoop;
  end
  else if TryStrToDateTime(edtTime.Text, dateTime, DATETIME_FORMAT_SETTINGS) then
  begin                                 { 时间、日期 }
    if chkEveryDay.Checked then
    begin
      timeType := tmTime;
      timeRec.TimeOrLoop := DateTimeToTimeStamp(dateTime).Time;
    end
    else
    begin
      timeType := tmDateTime;
      PTimeStamp(@timeRec)^ := DateTimeToTimeStamp(dateTime);
    end;
  end
  else if chkMonthly.Checked then
  begin                                 { 每月 24 22:29:00}
    S := edtTime.Text;
    bErr := not (
      (S[1] in ['0'..'3']) and (S[2] in ['0'..'9']) and (S[3] = ' ')
      and not TryStrToInt(S, n) and (n > 0) and (n <= 31)
      and TryStrToDateTime(Copy(S, 4, 8), dateTime, DATETIME_FORMAT_SETTINGS)
      );

    if not bErr then
    begin
      timeType := tmMonthly;
      timeRec.DateOrWeek := n;
      timeRec.TimeOrLoop := DateTimeToTimeStamp(dateTime).Time;
    end;
  end
  else
    bErr := True;

  if not (timeType in [tmDateTime, tmMonthly]) then
  begin
    weekSet := [wdNone];
    if chkWeek.Checked then
    begin
      if chkMon.Checked then
        weekSet := weekSet + [wdMon];
      if chkTue.Checked then
        weekSet := weekSet + [wdTue];
      if chkWed.Checked then
        weekSet := weekSet + [wdWed];
      if chkThu.Checked then
        weekSet := weekSet + [wdThu];
      if chkFri.Checked then
        weekSet := weekSet + [wdFri];
      if chkSat.Checked then
        weekSet := weekSet + [wdSat];
      if chkSun.Checked then
        weekSet := weekSet + [wdSun];
    end;
    timeRec.DateOrWeek := 0;            //清空
    PWeekSet(@timeRec.DateOrWeek)^ := weekSet;
  end;

  if bErr then
  begin
    FToolTip.Popup(edtTime.Handle, ttWarningIcon, '提示', '时间格式有误!');
    Exit;
  end;

  isAdd := not Assigned(FTask);
  if isAdd then
  begin
    FTask := g_TaskMgr.NewTask(True);
    Item := g_TaskMgr.Lv.Items.Insert(0);
    g_TaskMgr.UpdateTaskCount;
  end
  else
    Item := FTask.ItemUI;

  FTask.Active := chkActive.Checked;
  FTask.CId := Integer(cbbClass.Items.Objects[cbbClass.ItemIndex]);
  FTask.TaskType := TTaskType(cbbType.ItemIndex);
  FTask.ExecNum := seExecNum.Value;
  FTask.TimeType := timeType;
  FTask.TimeRec := timeRec;
  FTask.Param := edtParam.Text;
  FTask.Content := mmoContent.Text;
  FTask.TmpExecNum := chkTmpExecNum.Checked;
  FTask.Update;
  FTask.ResetLoop;
  FTask.AssignUI(Item);

  Close;
end;

procedure TfrmAddTask.chkEveryDayClick(Sender: TObject);
begin
  if Self.Showing then
    FToolTip.EndPopup;

  { 初始状态 }
  chkLoop.Enabled := not (chkEveryDay.Checked or chkMonthly.Checked);
  chkMonthly.Enabled := not (chkEveryDay.Checked or chkLoop.Checked);
  chkEveryDay.Enabled := not (chkLoop.Checked or chkMonthly.Checked);
  chkWeek.Enabled := chkEveryDay.Checked or chkLoop.Checked;
  if not chkWeek.Enabled then
    chkWeek.Checked := False;

  if chkLoop.Checked then
  begin                                 { 循环 }
    edtTime.Text := '60';
    edtTime.MaxLength := 9;             //999 999 999 < MAX_DWORD
    SetWindowLong(edtTime.Handle, GWL_STYLE,
      GetWindowLong(edtTime.Handle, GWL_STYLE) or ES_NUMBER);

    if Self.Showing then
      FToolTip.Popup(edtTime.Handle, ttInformationIcon, '提示', '输入倒计时间(秒)');
  end
  else
  begin
    SetWindowLong(edtTime.Handle, GWL_STYLE,
      GetWindowLong(edtTime.Handle, GWL_STYLE) and not ES_NUMBER);

    if chkEveryDay.Checked then
    begin                               { 每日 }
      edtTime.Text := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongTimeFormat, now);
      edtTime.MaxLength := 8;
    end
    else if chkMonthly.Checked then
    begin                               { 每月 }
      edtTime.Text := FormatDateTime('dd hh:mm:ss', now);
      edtTime.MaxLength := 11;
    end
    else
    begin                               { 日期 }
      edtTime.Text := FormatDateTime(DATETIME_FORMAT_SETTINGS.LongDateFormat, now);
      edtTime.MaxLength := 19;
    end;
  end;
end;

procedure TfrmAddTask.chkWeekClick(Sender: TObject);
begin
  { 星期 }
  chkMon.Enabled := chkWeek.Checked;
  chkTue.Enabled := chkWeek.Checked;
  chkWed.Enabled := chkWeek.Checked;
  chkThu.Enabled := chkWeek.Checked;
  chkFri.Enabled := chkWeek.Checked;
  chkSat.Enabled := chkWeek.Checked;
  chkSun.Enabled := chkWeek.Checked;

  if chkWeek.Checked then
  begin
    if not chkLoop.Enabled and not chkEveryDay.Enabled then
    begin                               { 处于日期模式 }
      chkLoop.Enabled := True;
      chkEveryDay.Enabled := True;
    end;
  end;
end;

procedure TfrmAddTask.cbbTypeChange(Sender: TObject);
var
  s                 : string;
  tt                : TTaskType;
begin
  tt := TTaskType(TComboBox(Sender).ItemIndex);

  if tt in [ttSendEmail] then
    lblParam.Caption := '参数:'
  else
    lblParam.Caption := '备注:';

  if tt in [ttExec, ttParamExec, ttCmdExec] then
    FParamTip := HIDE_PARAM_HEAD + ' 开头隐藏执行此任务!'
  else
    FParamTip := '';

  if tt in [ttExec, ttParamExec, ttDownExec, ttKillProcess, ttWakeUp] then
    FContentTip := '每行执行一次！'
  else if tt in [ttShutdownSys, ttRebootSys, ttLogoutSys, ttLockSys,
    ttSuspendSys] then
    FContentTip := '开头为数字，执行时提示并等待n秒'
  else
    FContentTip := '';

  s := '例如:'#13#10;
  case tt of
    ttExec:
      s := s
        + 'http://www.yryz.net'#13#10
        + 'd:\mp3\alarm.mp3';

    ttParamExec:
      s := s
        + 'ping 127.0.0.1'#13#10
        + 'shutdown -s';

    ttDownExec:
      s := s
        + 'http://im.qq.com/qq.exe';

    ttKillProcess:
      s := s
        + 'qq.exe'#13#10
        + 'cmd.exe';

    ttWakeUp:
      s := s
        + '00-e0-4d-df-7e-8a'#13#10
        + '00-e0-4d-df-88-1c';

    ttCmdExec:
      s := s
        + 'del c:\*.log'#13#10
        + 'mkdir c:\s'#13#10
        + 'systeminfo > c:\s\s.txt';

    ttMsgTip:
      s := s
        + '该睡觉了~~！';

    ttSendEmail:
      s := s
        + '文件'#13#10
        + 'c:\s\s.txt'#13#10
        + '文字'#13#10
        + 'SMTP Test!';

    ttSendKey:
      s := s
        + '^%z'#13#10
        + '模拟 Ctrl+Alt+Z';

    ttShutdownSys,
      ttRebootSys,
      ttLogoutSys,
      ttLockSys,
      ttSuspendSys:
      s := s
        + '30秒倒计时...';
  end;

  mmoContent.Hint := s;
  if mmoContent.Enabled then
    mmoContent.Color := clWindow
  else
    mmoContent.Color := clBtnFace;
end;

procedure TfrmAddTask.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ReleaseCapture;
  //发送系统消息，通知窗口标题栏被按下，之后就可以拖了
  SendMessage(Handle, WM_SYSCOMMAND, $F012, 0);
end;

procedure TfrmAddTask.FormPaint(Sender: TObject);
begin
  DrawRoundForm(Handle, Width, Height, $00CD746D);
end;

procedure TfrmAddTask.mmoContentEnter(Sender: TObject);
begin
  if Self.Showing and (FContentTip <> '') then
    FToolTip.Popup(TWinControl(Sender).Handle, ttInformationIcon, '提示', FContentTip);
end;

procedure TfrmAddTask.edtParamEnter(Sender: TObject);
begin
  if Self.Showing and (FParamTip <> '') then
    FToolTip.Popup(TWinControl(Sender).Handle, ttInformationIcon, '提示', FParamTip);
end;

procedure TfrmAddTask.mmoContentExit(Sender: TObject);
begin
  FToolTip.EndPopup;
end;

end.

