unit TaskMgr_u;

interface
uses
  Windows, SysUtils, Classes, Graphics, ExtCtrls, ComCtrls,
  ShellAPI, TlHelp32, UrlMon, WinSock;

type
  TTaskType = (ttExec, ttParamExec, ttDownExec, ttKillProcess, ttCmdExec,
    ttSendKey, ttSendEmail, ttWakeUp, ttMsgTip, ttShutdown, ttReboot, ttLogout);
const
  TASK_TYPE_STR     : array[TTaskType] of string[8] =
    ('普通运行', '参数运行', '下载运行', '结束进程', '执行DOS', '模拟按键',
    '发送邮件', '网络唤醒', '消息提示', '关机', '重启', '注销');
  INVALID_DATE      = 1;

type
  TTaskMgr = class;

  TTask = class(TListItem)              //TListView中添加此Item
  private
    FExecNum: DWORD;                    //可执行次数，< 1 停止
    FTaskType: TTaskType;
    FLoopTime: DWORD;                   //循环间隔
    FTimeStamp: TTimeStamp;             //IsLoop任务初始时间，否则任务开始时间
    FParam: string;
    FContent: string;
    procedure SetLoopTime(const Value: DWORD);
    procedure SetContent(const Value: string);
    procedure SetTimeStamp(const Value: TTimeStamp);
    procedure SetParam(const Value: string);
    procedure SetTaskType(const Value: TTaskType);
    procedure SetExecNum(const Value: DWORD);
  public
    constructor Create(Items: TListItems);
    destructor Destroy; override;
    procedure Execute;
    procedure ResetLoop;
    function IsLoop: Boolean;
    function IsTime: Boolean;
  published
    property ExecNum: DWORD read FExecNum write SetExecNum;
    property TaskType: TTaskType read FTaskType write SetTaskType;
    property LoopTime: DWORD read FLoopTime write SetLoopTime; //任务自动改变
    property TimeStamp: TTimeStamp read FTimeStamp write SetTimeStamp;
    property Param: string read FParam write SetParam;
    property Content: string read FContent write SetContent;
  end;

  TTaskMgr = class
  private
    FItems: TListItems;
    //设置
  public
    constructor Create(lvTask: TListView);
    destructor Destroy; override;
    function Add(taskType: TTaskType; execNum, loopTime: DWORD;
      timeStamp: TTimeStamp; param, content: string): Integer;
    function DeleteSelected: Integer;
    procedure OnTimer(dateTime: TTimeStamp);
    procedure LoadTask;
    procedure SaveTask;
  end;

  TSMTPOption = record
    Server: ShortString;
    UserName: ShortString;
    Password: ShortString;
  end;

var
  g_SMTPOption      : TSMTPOption;
  g_TaskMgr         : TTaskMgr;

procedure MsgTip(S: string);
procedure SetPrivilege(pName: PChar);
function KillTask(ExeFileName: string): Integer;
procedure DownloadExec(sUrl: PChar);
procedure SendMail(Task: TTask);
procedure WakeUpPro(MacAddr: string);
implementation
uses
  sndkey32, MSNPopUp, SendMailAPI, FuncLib;

{ TTask }

constructor TTask.Create;
begin
  inherited Create(Items);
  SubItems.Add('');                     //时间
  SubItems.Add('');                     //类型
  SubItems.Add('');                     //内容
  SubItems.Add('');                     //附加参数
  SubItems.Add('');                     //可执行次数
  Data := Self;
end;

destructor TTask.Destroy;
begin
  inherited;
end;

procedure TTask.Execute;
var
  dwThID            : DWORD;
begin
  case FTaskType of
    ttExec:
      ShellExecute(0, nil, PChar(FContent), nil, nil, SW_SHOW); //运行
    ttParamExec:
      WinExec(PChar(FContent), SW_SHOW);
    ttDownExec:
      CloseHandle(BeginThread(nil, 0, @DownloadExec, PChar(FContent), 0, dwThID));
    ttKillProcess:
      begin
        SetPrivilege('SeDebugPrivilege');
        KillTask(PChar(FContent));
      end;
    ttCmdExec:
      WinExec(PChar('cmd /c ' + FContent), SW_SHOW);
    ttSendKey:
      SendKeys(PChar(FContent), False);
    ttSendEmail:
      CloseHandle(BeginThread(nil, 0, @SendMail, Self, 0, dwThID));
    ttWakeUp:
      WakeUpPro(FContent);
    ttMsgTip:
      MsgTip(FContent);
    ttShutdown:
      begin
        SetPrivilege('SeShutdownPrivilege');
        ExitWindowsEX(EWX_SHUTDOWN or EWX_FORCE, 0); {关机}
      end;
    ttReboot:
      begin
        SetPrivilege('SeShutdownPrivilege');
        ExitWindowsEX(EWX_REBOOT or EWX_FORCE, 0); {重启}
      end;
    ttLogout:
      begin
        SetPrivilege('SeShutdownPrivilege');
        ExitWindowsEX(EWX_LOGOFF or EWX_FORCE, 0); {注销}
      end;
  end;

  ImageIndex := 1;
  Dec(FExecNum);
  SetExecNum(FExecNum);
  if IsLoop then
    if FExecNum <> 0 then ResetLoop;
end;

procedure MsgTip(S: string);
var
  FM_MSNPopUp       : TMSNPopUp;
begin
  FM_MSNPopUp := TMSNPopUp.Create(nil);
  try
    with FM_MSNPopUp do begin
      Text := S;
      Font.Name := '宋体';
      Font.Size := 10;
      Font.Color := $FF;
      HoverFont.Name := '宋体';
      HoverFont.Color := clred;
      if FileExists('OnTime.bmp') then BackgroundImage.LoadFromFile('OnTime.bmp');
      ShowPopUp;
    end;
  finally
    if assigned(FM_MSNPopUp) then begin
      FM_MSNPopUp := nil;
      FM_MSNPopUp.Free;
    end;
  end;
end;

{--------提升进程权限为DEBUG权限-------}

procedure SetPrivilege(pName: PChar);
var
  OldTokenPrivileges, TokenPrivileges: TTokenPrivileges;
  ReturnLength      : dword;
  hToken            : THandle;
  Luid              : int64;
begin
  OpenProcessToken(GetCurrentProcess, TOKEN_ADJUST_PRIVILEGES, hToken);
  LookupPrivilegeValue(nil, pName, Luid);
  TokenPrivileges.Privileges[0].Luid := Luid;
  TokenPrivileges.PrivilegeCount := 1;
  TokenPrivileges.Privileges[0].Attributes := 0;
  AdjustTokenPrivileges(hToken, False, TokenPrivileges, SizeOf(TTokenPrivileges), OldTokenPrivileges, ReturnLength);
  OldTokenPrivileges.Privileges[0].Luid := Luid;
  OldTokenPrivileges.PrivilegeCount := 1;
  OldTokenPrivileges.Privileges[0].Attributes := TokenPrivileges.Privileges[0].Attributes or SE_PRIVILEGE_ENABLED;
  AdjustTokenPrivileges(hToken, False, OldTokenPrivileges, ReturnLength, PTokenPrivileges(nil)^, ReturnLength);
end;

{-----------Kill进程--------------}

function KillTask(ExeFileName: string): integer;
const
  Proess_Terminate  = $0001;
var
  ContinueLoop      : BOOL;
  FSnapshotHandle   : THandle;
  FProcessEntry32   : TProcessEntry32;
begin
  result := 0;
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SnapProcess, 0); //获取进程列表
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  while integer(ContinueLoop) <> 0 do begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile)) = UpperCase(ExeFileName))
      or (UpperCase(FProcessEntry32.szExeFile) = UpperCase(ExeFileName))) then
      result := integer(TerminateProcess(OpenProcess(Process_Terminate, BOOL(0), FProcessEntry32.th32ProcessID), 0));
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

{ --------下载运行 -----------}

procedure DownloadExec(sUrl: PChar);
var
  PS                : ^TStrings;
  sFile             : string;
begin
  sFile := FormatDateTime('yyyyMMddhhmmss.', now) + copy(sUrl, Length(sUrl) - 2, Length(sUrl));
  UrlDownloadToFile(nil, PChar(sUrl), PChar(sFile), 0, nil);
  ShellExecute(0, nil, PChar(sFile), nil, nil, SW_SHOW);
  ExitThread(0);
end;

{ --------发送邮件 -----------}

procedure SendMail(Task: TTask);
var
  f                 : THandle;
  len               : Integer;
  sEmail, sFEmail, sContent, s: string;
begin
  try
    sEmail := Task.Param;
    sContent := Task.Content;
    if FileExists(sContent) then begin
      f := FileOpen(sContent, fmOpenRead or fmShareDenyNone);
      if Integer(f) > 0 then
        GetFileSize(f, @len);
      SetLength(sContent, len);
      FileRead(f, PChar(@sContent[1])^, len);
      FileClose(f);
    end;

    if Pos('@', g_SMTPOption.UserName) > 0 then
      sFEmail := g_SMTPOption.UserName
    else
      sFEmail := g_SMTPOption.UserName
        + '@' + GetSubStr(g_SMTPOption.Server, '@', '');

    DNASendEMail(g_SMTPOption.Server, g_SMTPOption.UserName, g_SMTPOption.Password,
      sFEmail, sEmail, FormatDateTime('yyyy-MM-dd hh:mm:ss', now) + '任务计划', sContent);
  except
  end;
  ExitThread(0);
end;

{远程唤醒函数   00-e0-4d-df-7e-8a}

procedure WakeUpPro(MacAddr: string);
var
  WSAData           : TWSAData;
  MSocket           : TSocket;
  SockAddrIn        : TSockAddrIn;
  i                 : integer;
  MagicAddr         : array[0..5] of Byte;
  MagicData         : array[0..101] of Byte;
begin
  for i := 0 to 5 do MagicAddr[i] := StrToInt('$' + copy(MacAddr, i * 3 + 1, 2));
  try
    WSAStartup($0101, WSAData);
    MSocket := socket(AF_INET, SOCK_DGRAM, IPPROTO_IP); //创建一个UPD数据报SOCKET.
    if MSocket = INVALID_SOCKET then exit;
    i := 1;
    setsockopt(MSocket, SOL_SOCKET, SO_BROADCAST, PChar(@i), SizeOf(i)); //设置广播
    FillChar(MagicData, SizeOf(MagicData), $FF);
    i := 6;
    while i < SizeOf(MagicData) do begin
      Move(MagicAddr, Pointer(Longint(@MagicData) + i)^, 6);
      Inc(i, 6);
    end;
    SockAddrIn.sin_family := AF_INET;
    SockAddrIn.sin_addr.S_addr := Longint(INADDR_BROADCAST);
    sendto(MSocket, MagicData, SizeOf(MagicData), 0, SockAddrIn, SizeOf(SockAddrIn));
    closesocket(MSocket);
    WSACleanup;
  except
  end;
end;

procedure TTask.ResetLoop;
begin
  FTimeStamp := DateTimeToTimeStamp(Now);
end;

function TTask.IsLoop: Boolean;
begin
  Result := FLoopTime > 0;
end;

procedure TTask.SetLoopTime(const Value: DWORD);
begin
  FLoopTime := Value;
  if Value > 0 then begin
    ResetLoop;
    Caption := IntToStr(Value);
  end;
end;

procedure TTask.SetContent(const Value: string);
begin
  FContent := Value;
  SubItems.Strings[1] := Value;
end;

procedure TTask.SetTimeStamp(const Value: TTimeStamp);
begin
  FTimeStamp := Value;
  if not IsLoop then
    if IsTime then
      Caption := FormatDateTime('hh:mm:ss', TimeStampToDateTime(Value))
    else
      Caption := FormatDateTime('yyyy-MM-dd hh:mm:ss', TimeStampToDateTime(Value));
end;

procedure TTask.SetParam(const Value: string);
begin
  FParam := Value;
  SubItems.Strings[2] := Value;
end;

procedure TTask.SetTaskType(const Value: TTaskType);
begin
  FTaskType := Value;
  SubItems.Strings[0] := TASK_TYPE_STR[Value];
end;

procedure TTask.SetExecNum(const Value: DWORD);
begin
  FExecNum := Value;
  SubItems.Strings[3] := IntToStr(Value);
end;

function TTask.IsTime: Boolean;
begin
  Result := FTimeStamp.Date = INVALID_DATE;
end;

{ TTaskMgr }

constructor TTaskMgr.Create;
begin
  FItems := lvTask.Items;
end;

destructor TTaskMgr.Destroy;
var
  I                 : Integer;
begin
  if FItems.Count > 0 then
    for i := FItems.Count - 1 downto 0 do
      with FItems[i] do begin
        TTask(Data).Free;
        Delete();
      end;
  inherited;
end;

function TTaskMgr.Add;
var
  Task              : TTask;
begin
  Task := TTask.Create(FItems);
  FItems.AddItem(Task);

  Task.Checked := True;
  Task.TaskType := taskType;
  Task.ExecNum := execNum;
  Task.LoopTime := loopTime;
  if not Task.IsLoop then
    Task.TimeStamp := timeStamp;
  Task.Param := param;
  Task.Content := content;

  Result := Task.index;
end;

function TTaskMgr.DeleteSelected;
var
  i                 : Integer;
begin
  Result := -1;
  if FItems.Count < 1 then Exit;

  for i := FItems.Count - 1 downto 0 do
    with FItems[i] do
      if Selected then begin
        Delete;
        Result := i;
      end;
end;

procedure TTaskMgr.OnTimer;
var
  i                 : Integer;
  Task              : TTask;
  RemainTime        : DWORD;
begin
  if FItems.Count < 1 then Exit;

  for i := 0 to FItems.Count - 1 do begin
    Task := FItems[I].Data;
    if not Task.Checked or
      (Integer(Task.ExecNum) < 1) then Continue; //不可执行

    if Task.IsLoop then begin           //按倒计时
      RemainTime := Task.LoopTime - (dateTime.Time - Task.TimeStamp.Time) div MSecsPerSec;

      if Integer(RemainTime) < 0 then RemainTime := 0;
      Task.Caption := IntToStr(RemainTime); //更新倒计时显示

      if RemainTime <> 0 then Continue;
    end else                            //按日期时间
    begin
      if Task.IsTime then begin         //时间
        if dateTime.Time div MSecsPerSec <>
          Task.TimeStamp.Time div MSecsPerSec then
          Continue;
      end else                          //日期时间
        if (dateTime.Time div MSecsPerSec <> Task.TimeStamp.Time div MSecsPerSec)
          or (dateTime.Date <> Task.TimeStamp.Date) then Continue;
    end;

    Task.Execute;
  end;
end;

procedure TTaskMgr.LoadTask;
begin

end;

procedure TTaskMgr.SaveTask;
begin

end;

initialization

finalization
  if Assigned(g_TaskMgr) then begin
    g_TaskMgr.SaveTask;
    g_TaskMgr.Free;
  end;

end.

