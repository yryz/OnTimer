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
    btnOk: TButton;
    btnCancel: TButton;
    edtTime: TEdit;
    chkEveryDay: TCheckBox;
    chkLoop: TCheckBox;
    InfoLabel1: TLabel;
    edtContent: TEdit;
    seExecNum: TSpinEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtParam: TEdit;
    chkActive: TCheckBox;
    procedure btnOkClick(Sender: TObject);
    procedure chkEveryDayClick(Sender: TObject);
    procedure cbbTypeChange(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
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
  if Assigned(FTask) then
  begin
    chkActive.Checked := FTask.Checked;
    cbbType.ItemIndex := Integer(FTask.TaskType);
    seExecNum.Value := FTask.ExecNum;

    case FTask.TimeType of
      ttLoop: begin
          seExecNum.Enabled := True;
          chkLoop.Checked := True;
          edtTime.Text := IntToStr(FloatToInt23(FTask.DateTime));
        end;

      ttTime: begin
          seExecNum.Enabled := True;
          chkEveryDay.Checked := True;
          edtTime.Text := FormatDateTime('hh:mm:ss', FTask.DateTime);
        end;

      ttDateTime: begin
          seExecNum.Enabled := False;
          edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', FTask.DateTime);
        end;
    end;

    edtParam.Text := FTask.Param;
    edtContent.Text := FTask.Content;
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
  loopTime          : Integer;
  dateTime          : TDateTime;
  timeType          : TTimeType;
begin
  bErr := False;
  loopTime := 0;
  timeType := ttDateTime;
  if chkLoop.Checked then begin         { 倒计时 }
    if not TryStrToInt(edtTime.Text, loopTime) then
      bErr := true;
    //    if loopTime > MAX_LOOP_VALUE then begin
    //      FToolTip.Popup(edtTime.Handle, ttWarningIcon,
    //        '提示', '计时不能大于' + IntToStr(MAX_LOOP_VALUE) + '秒!');
    //      Exit;
    //    end;
    dateTime := loopTime;
    timeType := ttLoop;
  end else
    if not TryStrToDateTime(edtTime.Text, dateTime) then
      bErr := true
    else begin                          { 时间、日期 }
      if chkEveryDay.Checked then
        timeType := ttTime
      else
        timeType := ttDateTime;
    end;

  if bErr then begin
    FToolTip.Popup(edtTime.Handle, ttWarningIcon, '提示', '时间格式有误!');
    Exit;
  end;

  taskType := TTaskType(cbbType.ItemIndex);

  isAdd := not Assigned(FTask);

  if isAdd then FTask := g_TaskMgr.Make(chkActive.Checked)
  else FTask.Checked := chkActive.Checked;

  FTask.TaskType := taskType;
  FTask.ExecNum := seExecNum.Value;
  FTask.SetTime(timeType, dateTime);
  FTask.Param := edtParam.Text;
  FTask.Content := edtContent.Text;
  g_TaskMgr.Update(isAdd, FTask);

  Close;
end;

procedure TfrmAddTask.chkEveryDayClick(Sender: TObject);
begin
  SetWindowLong(edtTime.Handle, GWL_STYLE,
    GetWindowLong(edtTime.Handle, GWL_STYLE) and not ES_NUMBER);
  FToolTip.EndPopup;
  chkLoop.Enabled := not chkEveryDay.Checked;
  if chkEveryDay.Checked then begin
    edtTime.Text := FormatDateTime('hh:mm:ss', now);
    edtTime.MaxLength := 8;
    chkLoop.Checked := False;
    seExecNum.Enabled := True;
    Exit;
  end else
  begin
    chkEveryDay.Enabled := not chkLoop.Checked;
    if chkLoop.Checked then begin
      edtTime.Text := '60';
      edtTime.MaxLength := 6;           //999 999 <MAX_LOOP_VALUE
      chkEveryDay.Checked := False;
      seExecNum.Enabled := True;
      SetWindowLong(edtTime.Handle, GWL_STYLE,
        GetWindowLong(edtTime.Handle, GWL_STYLE) or ES_NUMBER);

      FToolTip.Popup(edtTime.Handle, ttInformationIcon, '提示', '输入倒计时间(秒)');
      Exit;
    end
  end;

  seExecNum.Enabled := False;
  edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
  edtTime.MaxLength := 19;
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

