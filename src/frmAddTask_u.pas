unit frmAddTask_u;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms, Graphics,
  ComCtrls, StdCtrls, Spin, TaskMgr_u, TooltipUtil;

type
  TfrmAddTask = class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    cbbType: TComboBox;
    Button1: TButton;
    Button2: TButton;
    edtTime: TEdit;
    chkEveryDay: TCheckBox;
    chkLoop: TCheckBox;
    InfoLabel1: TLabel;
    edtContent: TEdit;
    seExecNum: TSpinEdit;
    lbl1: TLabel;
    lbl2: TLabel;
    edtParam: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure edtParamKeyPress(Sender: TObject; var Key: Char);
    procedure chkEveryDayClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbbTypeChange(Sender: TObject);
  private
    FTask: TTask;
    FToolTip: TToolTip;
  public
    property Task: TTask write FTask;
  end;

var
  frmAddTask        : TfrmAddTask;
  IsEdit            : Boolean;
implementation
uses
  frmOnTime_u;
{$R *.dfm}

procedure TfrmAddTask.Button1Click(Sender: TObject);
var
  bErr              : Boolean;
  Task              : TTask;
  taskType          : TTaskType;
  loopTime          : Integer;
  dateTime          : TDateTime;
  timeStamp         : TTimeStamp;
begin
  bErr := False;
  loopTime := 0;
  if chkLoop.Checked then begin
    if not TryStrToInt(edtTime.Text, loopTime) then
      bErr := true;
  end else
    if not TryStrToDateTime(edtTime.Text, dateTime) then
      bErr := true
    else begin
      timeStamp := DateTimeToTimeStamp(dateTime);
      if chkEveryDay.Checked then
        timeStamp.Date := INVALID_DATE;
    end;

  if bErr then begin
    FToolTip.Popup(edtTime.Handle, ttWarningIcon, '提示', '时间格式有误!');
    Exit;
  end;

  taskType := TTaskType(cbbType.ItemIndex);

  if Assigned(FTask) then begin
    FTask.TaskType := taskType;
    FTask.ExecNum := seExecNum.Value;
    FTask.LoopTime := loopTime;
    FTask.TimeStamp := timeStamp;
    FTask.Param := edtParam.Text;
    FTask.Content := edtContent.Text;
  end else
    frmOnTime.AddTaskItem(True, taskType, seExecNum.Value,
      loopTime, timeStamp, edtParam.Text, edtContent.Text);

  Close;
end;

procedure TfrmAddTask.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TfrmAddTask.FormShow(Sender: TObject);
var
  timeFmt           : string;
begin
  if Assigned(FTask) then
  begin
    cbbType.ItemIndex := Integer(FTask.TaskType);
    seExecNum.Value := FTask.ExecNum;

    chkLoop.Checked := FTask.IsLoop;
    if chkLoop.Checked then
    begin
      edtTime.Text := IntToStr(FTask.LoopTime);
    end else
    begin
      chkEveryDay.Checked := FTask.IsTime;
      if chkEveryDay.Checked then
        timeFmt := 'hh:mm:ss'
      else
        timeFmt := 'yyyy-MM-dd hh:mm:ss';

      edtTime.Text := FormatDateTime(timeFmt, TimeStampToDateTime(FTask.TimeStamp))
    end;

    edtParam.Text := FTask.Param;
    edtContent.Text := FTask.Content;
  end else
    edtTime.Text := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
end;

procedure TfrmAddTask.edtParamKeyPress(Sender: TObject; var Key: Char);
begin
  if word(Key) = VK_RETURN then Button1.Click;
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
      edtTime.MaxLength := 5;
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

procedure TfrmAddTask.FormCreate(Sender: TObject);
var
  i                 : TTaskType;
begin
  cbbType.Clear;
  for i := TTaskType(0) to High(TTaskType) do
    cbbType.Items.Add(TASK_TYPE_STR[i]);
  cbbType.ItemIndex := 0;

  FToolTip := TToolTip.Create(Self);
  FToolTip.Interval := 3000;
end;

procedure TfrmAddTask.cbbTypeChange(Sender: TObject);
begin
  edtParam.Enabled := TTaskType(TComboBox(Sender).ItemIndex) in [ttSendEmail, ttWakeUp, ttMsgTip];
  edtContent.Enabled := not (TTaskType(TComboBox(Sender).ItemIndex) in [ttShutdown, ttReboot, ttLogout]);
  if edtParam.Enabled then
    edtParam.Color := clWindow
  else
    edtParam.Color := clBtnFace;

  if edtContent.Enabled then
    edtContent.Color := clWindow
  else
    edtContent.Color := clBtnFace;
end;

end.

