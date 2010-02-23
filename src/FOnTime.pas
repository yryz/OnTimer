unit FOnTime;

interface

uses
  Windows, Messages, SysUtils, Graphics, Classes, Forms,
  sndkey32, ShellAPI, ExtCtrls, Controls, URLMon,
  ComCtrls, ImgList, Tlhelp32, Menus, Winsock, XPMan;

const
  WM_ICON = WM_USER + 10;

type
  TfrmOnTimer = class(TForm)
    Timer1: TTimer;
    PopMenuA: TPopupMenu;
    M5: TMenuItem;
    M0: TMenuItem;
    ListView1: TListView;
    M1: TMenuItem;
    M3: TMenuItem;
    M2: TMenuItem;
    N0: TMenuItem;
    N1: TMenuItem;
    Timer2: TTimer;
    ImageList1: TImageList;
    Timer3: TTimer;
    N2: TMenuItem;
    M4: TMenuItem;
    N3: TMenuItem;
    procedure AddTaskItem(B: Boolean; S1, S2, S3, S4: string);
    procedure MenuClick(Sender: TObject);
    procedure OnTimeExec(i: integer);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
  private
    procedure SysEvent(var Message: Tmessage); message WM_SYSCOMMAND;
    procedure TrayEvent(var Message: Tmessage); message WM_ICON;
  public
  end;

resourcestring
  Conf = 'OnTime.db';

const
  MText: array[0..11] of string[8] = ('普通运行', '参数运行', '下载运行', '结束进程', '执行DOS', '模拟按键', '发送邮件', '网络唤醒', '消息提示', '关机', '重启', '注销');

var
  frmOnTimer: TfrmOnTimer;
  AppTray: TNotifyIconData;
  ThreadID: Cardinal;
  LoopTime: pstring;
implementation

uses
  uPublic, AddUtils, AboutUtils, MSNPopUp, SendMailAPI, SetUnit;
{$R *.dfm}

//自定义的 Pos 函数 单个字符查找比Pos快10倍多

function MyPos(c: Char; const Str: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(Str) do
    if c = Str[i] then begin
      result := i;
      exit
    end;
end;

procedure SetTryico;
begin
  with AppTray do begin
    cbSize := SizeOf(AppTray);
    Wnd := frmOnTimer.Handle;
    uID := 0;
    uFlags := NIF_ICON or NIF_TIP or NIF_MESSAGE;
    uCallbackMessage := WM_ICON;
    HIcon := Application.Icon.Handle;
  end;
  Shell_NotifyIcon(NIM_ADD, @AppTray);
end;

procedure MsgTip(S: string);
var
  FM_MSNPopUp: TMSNPopUp;
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
  ReturnLength: dword;
  hToken: THandle;
  Luid: int64;
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

{----------end-------------}

{-----------Kill进程--------------}

function KillTask(ExeFileName: string): integer;
const
  Proess_Terminate = $0001;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
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
{-----------end---------------}

{ --------下载运行 -----------}

procedure Download(P: Pointer);
var
  PS: ^TStrings;
  S, url: string;
begin
  PS := P;
  url := PS.Strings[2];
  //ontimer.ListView1.Items.Add.Caption:=url;
  S := FormatDateTime('yyyyMMddhhmmss.', now) + copy(url, Length(url) - 2, Length(url));
  UrlDownloadToFile(nil, PChar(url), PChar(S), 0, nil);
  ShellExecute(Application.Handle, nil, PChar(S), nil, nil, SW_SHOW);
end;
{-----------end---------------}

{ --------发送邮件 -----------}

procedure SendMail(P: Pointer);
var
  PS: ^TStrings;
  f: textfile;
  c, S: string;
begin
  PS := P;
  c := PS.Strings[2];
  if FileExists(c) then begin
    assignFile(f, c);
    reset(f);
    while not eof(f) do begin
      readln(f, S);
      c := c + #13#10 + S;
    end;
  end;
  with SetData do begin
    if Pos('@', smtpuser) = 0 then
      s := smtpuser + copy(smtpserver, Pos('smtp', smtpserver) + 5, maxint)
    else
      s := smtpuser;
    DNASendEMail(smtpserver, smtpuser, smtppass, s, PS.Strings[1], FormatDateTime('yyyy-MM-dd hh:mm:ss', now) + '任务计划', c);
  end;
end;
{ --------end-----------------}

{远程唤醒函数   00-e0-4d-df-7e-8a}

procedure WakeUpPro(MacAddr: string);
var
  WSAData: TWSAData;
  MSocket: TSocket;
  SockAddrIn: TSockAddrIn;
  i: integer;
  MagicAddr: array[0..5] of Byte;
  MagicData: array[0..101] of Byte;
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
    exit;
  end;
end;
{----------end---------}

procedure TfrmOnTimer.AddTaskItem(B: Boolean; S1, S2, S3, S4: string);
begin
  with ListView1.Items.Add do begin
    Checked := B;
    if MyPos(':', S1) = 0 then begin
      New(LoopTime);
      LoopTime^ := S1;
      Data := LoopTime;
      Caption := S1;
    end
    else
      Caption := S1;
    SubItems.Append(S2);
    SubItems.Append(S3);
    SubItems.Append(S4);
  end;
end;

procedure TfrmOnTimer.SysEvent(var Message: Tmessage);
begin
  inherited;
  if Message.wParam = SC_MINIMIZE then
    ShowWindow(Application.Handle, SW_HIDE);
end;

procedure TfrmOnTimer.TrayEvent(var Message: Tmessage);
var
  Point: TPoint;
begin
  GetCursorPos(Point);
  case Message.lParam of
    WM_LBUTTONDBLCLK: begin
        Application.restore;
        Application.BringToFront;
      end;

    WM_RBUTTONDOWN: begin
        SetForegroundWindow(Handle);
        PopMenuA.Popup(Point.x, Point.y);
      end
  else
    inherited;
  end;
end;

procedure TfrmOnTimer.MenuClick(Sender: TObject);
var
  i: integer;
begin
  case (Sender as TComponent).Tag of
    0: begin
        Application.CreateForm(TFAbout, FAbout);
        FAbout.ShowModal;
      end;
    1: begin
        IsEdit := False;
        Application.CreateForm(TAddTaskF, AddTaskF);
        AddTaskF.ShowModal;
      end;
    2: begin
        if ListView1.SelCount < 1 then exit;
        IsEdit := true;
        Application.CreateForm(TAddTaskF, AddTaskF);
        with ListView1.Selected do begin
          if Data <> nil then begin
            AddTaskF.CheckBox2.Checked := true;
            AddTaskF.edt1.Text := string(Data^);
          end
          else begin
            AddTaskF.CheckBox2.Checked := False;
            if Length(Caption) = 19 then AddTaskF.CheckBox1.Checked := False
            else AddTaskF.CheckBox1.Checked := true;
            AddTaskF.edt1.Text := Caption;
          end;
          AddTaskF.ComboBox1.Text := SubItems.Strings[0];
          AddTaskF.edt2.Text := SubItems.Strings[1];
          AddTaskF.edt3.Text := SubItems.Strings[2];
        end;
        AddTaskF.ShowModal;
      end;
    3: ListView1.DeleteSelected;
    4: for i := 0 to ListView1.Items.Count - 1 do if ListView1.Items[i].Selected then OnTimeExec(i);
    5: begin
        if not Assigned(FSet) then
          Application.CreateForm(TFSet, FSet);
        FSet.edtSer.Text := SetData.smtpserver;
        FSet.edtUser.Text := SetData.smtpuser;
        FSet.ShowModal;
        FSet.Destroy;
      end;
    6: Close;
  end;
end;

procedure TfrmOnTimer.FormCreate(Sender: TObject);
var
  f: textfile;
  B, S1, S2, S3, S4: string;
begin
  SetTryico;
  try
    if FileExists(Conf) then begin
      assignFile(f, Conf);
      reset(f);
      while not eof(f) do begin
        readln(f, B);
        readln(f, S1);
        readln(f, S4);
        readln(f, S3);
        readln(f, S2);
        AddTaskItem(StrToBool(B), S1, S2, S3, S4);
      end;
      CloseFile(f);
    end;
  except
    CloseFile(f);
  end;
end;

procedure TfrmOnTimer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  f: textfile;
  i, j: integer;
begin
  Shell_NotifyIcon(NIM_DELETE, @AppTray);

  assignFile(f, Conf);
  ReWrite(f);
  i := ListView1.Items.Count - 1;
  while i > -1 do begin
    j := ListView1.Items[i].SubItems.Count - 1;
    WriteLn(f, BoolToStr(ListView1.Items[i].Checked));
    if ListView1.Items[i].Data <> nil then begin
      WriteLn(f, string(ListView1.Items[i].Data^));
      Dispose(ListView1.Items[i].Data);
    end
    else WriteLn(f, ListView1.Items[i].Caption);
    while j > -1 do begin
      WriteLn(f, ListView1.Items[i].SubItems.Strings[j]);
      Dec(j);
    end;
    Dec(i);
  end;
  CloseFile(f);
end;

procedure TfrmOnTimer.OnTimeExec(i: integer);
var
  CStr0, CStr1, CStr2: string;
begin
  CStr0 := ListView1.Items[i].SubItems.Strings[0];
  CStr1 := ListView1.Items[i].SubItems.Strings[1];
  CStr2 := ListView1.Items[i].SubItems.Strings[2];
  ListView1.Items[i].ImageIndex := 1;
  if CStr0 = MText[0] then ShellExecute(Handle, nil, PChar(CStr1), nil, nil, SW_SHOW) //运行
  else if CStr0 = MText[1] then WinExec(PChar(CStr2), SW_SHOW)
  else if CStr0 = MText[2] then BeginThread(nil, 0, @Download, @ListView1.Items[i].SubItems, 0, ThreadID)
  else if CStr0 = MText[3] then begin
    SetPrivilege('SeDebugPrivilege');
    KillTask(CStr2);
  end
  else if CStr0 = MText[4] then WinExec(PChar('cmd /c ' + CStr2), SW_SHOW)
  else if CStr0 = MText[5] then SendKeys(PChar(CStr2), False)
  else if CStr0 = MText[6] then BeginThread(nil, 0, @SendMail, @ListView1.Items[i].SubItems, 0, ThreadID)
  else if CStr0 = MText[7] then WakeUpPro(CStr2)
  else if CStr0 = MText[8] then MsgTip(CStr2)
  else if CStr0 = MText[9] then begin
    SetPrivilege('SeShutdownPrivilege');
    ExitWindowsEX(EWX_SHUTDOWN or EWX_FORCE, 0); {关机}
  end
  else if CStr0 = MText[10] then begin
    SetPrivilege('SeShutdownPrivilege');
    ExitWindowsEX(EWX_REBOOT or EWX_FORCE, 0); {重启}
  end
  else if CStr0 = MText[11] then begin
    SetPrivilege('SeShutdownPrivilege');
    ExitWindowsEX(EWX_LOGOFF or EWX_FORCE, 0); {注销}
  end;
end;


procedure TfrmOnTimer.Timer1Timer(Sender: TObject);
var
  i, N: dword;
begin
  if ListView1.Items.Count = 0 then exit;
  Caption := FormatDateTime('yyyy-MM-dd hh:mm:ss', now);
  for i := 0 to ListView1.Items.Count - 1 do begin
    if ListView1.Items[i].Checked then
      if (ListView1.Items[i].Caption = Caption) or (ListView1.Items[i].Caption = FormatDateTime('hh:mm:ss', now)) then OnTimeExec(i)
      else
        if MyPos(':', ListView1.Items[i].Caption) = 0 then begin //循环定时
          N := StrToInt(ListView1.Items[i].Caption);
          if N = 0 then begin
            ListView1.Items[i].Caption := string(ListView1.Items[i].Data^);
            OnTimeExec(i);
          end
          else begin
            Dec(N);
            ListView1.Items[i].Caption := inttostr(N);
          end;
        end;
  end;
end;

procedure TfrmOnTimer.Timer2Timer(Sender: TObject);
begin
  SetProcessWorkingSetSize(GetCurrentProcess, $FFFFFFFF, $FFFFFFFF); //整理内存
end;

procedure TfrmOnTimer.Timer3Timer(Sender: TObject);
begin
  Timer3.Destroy;
  if AnsiLowercase(ParamStr(1)) <> '/hide' then exit;
  ShowWindow(Application.Handle, SW_SHOWMINIMIZED);
  ShowWindow(Application.Handle, SW_HIDE);
end;

end.

