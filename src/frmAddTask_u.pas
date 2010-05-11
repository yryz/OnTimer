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
    edtContent: TEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtParam: TEdit;
    chkActive: TCheckBox;
    chkWeek: TCheckBox;
    chkMon: TCheckBox;
    chkTue: TCheckBox;
    chkWed: TCheckBox;
    chkThu: TCheckBox;
    chkFri: TCheckBox;
    chkSat: TCheckBox;
    chkSun: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure chkEveryDayClick(Sender: TObject);
    procedure cbbTypeChange(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure chkWeekClick(Sender: TObject);
  private
    FTask: TTask;
    FToolTip: TToolTip;
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
  i                 : TTaskType;
  weekSet           : TWeekSet;
begin
  inherited Create(Application);
  FTask := Task;

  cbbType.Clear;
  for i := TTaskType(0) to High(TTaskType) do
    cbbType.Items.Add(TASK_TYPE_STR[i]);
  cbbType.ItemIndex := 0;

  FToolTip := TToolTip.Create(Self);
  FToolTip.Interval := 3000;
  { 任务数据 }
  if Assigned(Task) then
  begin
    chkActive.Checked := Task.Checked;
    cbbType.ItemIndex := Integer(Task.TaskType);
    seExecNum.Value := Task.ExecNum;

    case Task.TimeType of
      ttLoop, ttTime: begin
          if Task.TimeType = ttLoop then begin
            seExecNum.Enabled := True;
            chkLoop.Checked := True;
            edtTime.Text := IntToStr(Task.TimeRec.TimeOrLoop);
          end else begin
            seExecNum.Enabled := True;
            chkEveryDay.Checked := True;
            edtTime.Text := FormatDateTime('hh:mm:ss',
              TimeStampToDateTime(PTimeStamp(@Task.TimeRec)^));
          end;
          { 星期数据要晚时间数据设置！！ }
          if Task.IsWeek then begin
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

      ttDateTime: begin
          seExecNum.Enabled := False;
          edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss',
            TimeStampToDateTime(PTimeStamp(@Task.TimeRec)^));
        end;
    end;

    edtParam.Text := Task.Param;
    edtContent.Text := Task.Content;
  end else
  begin
    seExecNum.Enabled := False;
    edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
  end;

  cbbTypeChange(cbbType);
end;


procedure TfrmAddTask.btnOkClick(Sender: TObject);
var
  bErr, isAdd       : Boolean;
  taskType          : TTaskType;
  timeType          : TTimeType;
  dateTime          : TDateTime;
  timeRec           : TTimeRec;
  weekSet           : TWeekSet;
begin
  bErr := False;

  timeType := ttDateTime;
  if chkLoop.Checked then begin         { 倒计时 }
    if not TryStrToInt(edtTime.Text, Integer(timeRec.TimeOrLoop)) then
      bErr := true;
    //    if loopTime > MAX_LOOP_VALUE then begin
    //      FToolTip.Popup(edtTime.Handle, ttWarningIcon,
    //        '提示', '计时不能大于' + IntToStr(MAX_LOOP_VALUE) + '秒!');
    //      Exit;
    //    end;
    timeType := ttLoop;
  end else
    if not TryStrToDateTime(edtTime.Text, dateTime) then
      bErr := true
    else begin                          { 时间、日期 }
      if chkEveryDay.Checked then begin
        timeType := ttTime;
        timeRec.TimeOrLoop := DateTimeToTimeStamp(dateTime).Time;
      end else
      begin
        timeType := ttDateTime;
        PTimeStamp(@timeRec)^ := DateTimeToTimeStamp(dateTime);
      end;
    end;

  { 星期设置 }
  if timeType <> ttDateTime then begin
    weekSet := [wdNone];
    if chkWeek.Checked then begin
      if chkMon.Checked then weekSet := weekSet + [wdMon];
      if chkTue.Checked then weekSet := weekSet + [wdTue];
      if chkWed.Checked then weekSet := weekSet + [wdWed];
      if chkThu.Checked then weekSet := weekSet + [wdThu];
      if chkFri.Checked then weekSet := weekSet + [wdFri];
      if chkSat.Checked then weekSet := weekSet + [wdSat];
      if chkSun.Checked then weekSet := weekSet + [wdSun];
    end;
    timeRec.DateOrWeek := 0;            //清空
    PWeekSet(@timeRec.DateOrWeek)^ := weekSet;
  end;

  if bErr then begin
    FToolTip.Popup(edtTime.Handle, ttWarningIcon, '提示', '时间格式有误!');
    Exit;
  end;

  isAdd := not Assigned(FTask);
  if isAdd then FTask := g_TaskMgr.Make(chkActive.Checked)
  else FTask.Checked := chkActive.Checked;
  FTask.TaskType := TTaskType(cbbType.ItemIndex);
  FTask.ExecNum := seExecNum.Value;
  FTask.SetTime(timeType, timeRec);
  FTask.Param := edtParam.Text;
  FTask.Content := edtContent.Text;
  g_TaskMgr.Update(isAdd, FTask);

  Close;
end;

procedure TfrmAddTask.chkEveryDayClick(Sender: TObject);
begin
  if Self.Showing then FToolTip.EndPopup;
  { 初始状态 }
  chkLoop.Enabled := not chkEveryDay.Checked;
  chkEveryDay.Enabled := not chkLoop.Checked;
  chkWeek.Enabled := chkEveryDay.Checked or chkLoop.Checked;
  if not chkWeek.Enabled then chkWeek.Checked := False;

  { 循环 }
  if chkLoop.Checked then begin
    edtTime.Text := '60';
    edtTime.MaxLength := 9;             //999 999 999 < MAX_DWORD
    chkEveryDay.Checked := False;
    seExecNum.Enabled := True;
    SetWindowLong(edtTime.Handle, GWL_STYLE,
      GetWindowLong(edtTime.Handle, GWL_STYLE) or ES_NUMBER);

    if Self.Showing then
      FToolTip.Popup(edtTime.Handle, ttInformationIcon, '提示', '输入倒计时间(秒)');
  end else
  begin
    SetWindowLong(edtTime.Handle, GWL_STYLE,
      GetWindowLong(edtTime.Handle, GWL_STYLE) and not ES_NUMBER);

    if chkEveryDay.Checked then begin   { 每日 }
      edtTime.Text := FormatDateTime('hh:mm:ss', now);
      edtTime.MaxLength := 8;
      chkLoop.Checked := False;
      seExecNum.Enabled := True;
    end
    else begin                          { 日期 }
      seExecNum.Enabled := False;
      edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
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

  if chkWeek.Checked then begin
    if not chkLoop.Enabled and not chkEveryDay.Enabled then
    begin                               { 处于日期模式 }
      chkLoop.Enabled := True;
      chkEveryDay.Enabled := True;
    end;
  end;
end;

procedure TfrmAddTask.cbbTypeChange(Sender: TObject);
begin
  edtParam.Enabled := TTaskType(TComboBox(Sender).ItemIndex) in [ttSendEmail];
  edtContent.Enabled := not (TTaskType(TComboBox(Sender).ItemIndex) in
    [ttShutdownPC, ttRebootPC, ttLogoutPC, ttLockPC]);
  if edtParam.Enabled then
    edtParam.Color := clWindow
  else
    edtParam.Color := clBtnFace;

  if edtContent.Enabled then
    edtContent.Color := clWindow
  else
    edtContent.Color := clBtnFace;
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

end.

