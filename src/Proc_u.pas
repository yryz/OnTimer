unit Proc_u;

interface

uses
  Windows, SysUtils, Winsock, Graphics, TlHelp32, ShellAPI, TaskMgr_u,
  UrlMon, FuncLib;

procedure SetPrivilege(pName: PChar);
function KillTask(ExeFileName: string): Integer;
procedure DownloadExec(sUrl: PChar);
procedure SendMail(Task: TTask);
procedure WakeUpPro(MacAddr: string);

procedure DrawRoundForm(const Handle: HWND; const Width, Height: Integer; Color: DWORD);
implementation
uses
  SendMailAPI;

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
  while integer(ContinueLoop) <> 0 do
  begin
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
  sFile             : string;
begin
  sFile := FormatDateTime('yyyyMMddhhmmss.', now) + copy(sUrl,
    Length(sUrl) - 2, Length(sUrl));
  UrlDownloadToFile(nil, PChar(sUrl), PChar(sFile), 0, nil);
  ShellExecute(0, nil, PChar(sFile), nil, nil, SW_SHOW);
  ExitThread(0);
end;

{ --------发送邮件 -----------}

procedure SendMail(Task: TTask);
var
  f                 : THandle;
  len               : Integer;
  sEmail, sFEmail, sContent: string;
begin
  try
    sEmail := Task.Param;
    sContent := Task.Content;
    if FileExists(sContent) then
    begin
      f := FileOpen(sContent, fmOpenRead or fmShareDenyNone);
      if Integer(f) > 0 then
      begin
        len := Windows.GetFileSize(f, nil);
        SetLength(sContent, len);
        FileRead(f, PChar(@sContent[1])^, len);
        FileClose(f);
      end;
    end;

    if Pos('@', g_Option.SmtpUser) > 0 then
      sFEmail := g_Option.SmtpUser
    else
      sFEmail := g_Option.SmtpUser
        + '@' + GetSubStr(g_Option.SmtpServer, '.', '');

    DNASendEMail(g_Option.SmtpServer, g_Option.SmtpPort,
      g_Option.SmtpUser, g_Option.SmtpPass, sFEmail, sEmail,
      FormatDateTime('yyyy-MM-dd hh:mm:ss', now) + ' 任务计划', sContent);
  except
    on E: Exception do
      OutDebug('SendMail except!' + E.Message);
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
  for i := 0 to 5 do
    MagicAddr[i] := StrToInt('$' + copy(MacAddr, i * 3 + 1, 2));
  try
    WSAStartup($0101, WSAData);
    MSocket := socket(AF_INET, SOCK_DGRAM, IPPROTO_IP); //创建一个UPD数据报SOCKET.
    if MSocket = INVALID_SOCKET then
      exit;
    i := 1;
    setsockopt(MSocket, SOL_SOCKET, SO_BROADCAST, PChar(@i), SizeOf(i)); //设置广播
    FillChar(MagicData, SizeOf(MagicData), $FF);
    i := 6;
    while i < SizeOf(MagicData) do
    begin
      Move(MagicAddr, Pointer(Longint(@MagicData) + i)^, 6);
      Inc(i, 6);
    end;
    SockAddrIn.sin_family := AF_INET;
    SockAddrIn.sin_addr.S_addr := Longint(INADDR_BROADCAST);
    sendto(MSocket, MagicData, SizeOf(MagicData), 0, SockAddrIn, SizeOf(SockAddrIn));
    closesocket(MSocket);
    WSACleanup;
  except
    on E: Exception do
      OutDebug('WakeUpPro ' + MacAddr + ' except!' + E.Message);
  end;
end;

procedure DrawRoundForm(const Handle: HWND; const Width, Height: Integer; Color: DWORD);
var
  //圆边
  FRegion           : THandle;
  //边框
  DC                : HDC;
  Pen               : HPen;
  OldPen            : HPen;
  OldBrush          : HBrush;
begin
  FRegion := CreateRoundRectRgn(0, 0, Width, Height, 9, 9); //界定一个椭圆区域
  SetWindowRgn(Handle, FRegion, False); //对连接后的区域进行绘制 因为是在onpait中所以redraw为FALSE

  DC := GetWindowDC(Handle);
  Pen := CreatePen(PS_SOLID, 1, Color);
  OldPen := SelectObject(DC, Pen);      //载入自定义的画笔,保存原画笔
  OldBrush := SelectObject(DC, GetStockObject(NULL_BRUSH)); //载入空画刷,保存原画刷
  RoundRect(DC, 0, 0, Width - 1, Height - 1, 10, 10); //画边框
  SelectObject(DC, OldBrush);           //载入原画刷
  SelectObject(DC, OldPen);             // 载入原画笔
  DeleteObject(Pen);
  ReleaseDC(Handle, DC);
end;
end.

