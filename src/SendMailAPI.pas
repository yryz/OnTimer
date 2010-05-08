{一个WinSock API写的邮件发送单元。调用方法:
DNASendEMail(SMTP服务器地址, 发送MAIL用户名, 发送MAIL的密码, 发送MAIL全名, 接收MAIL[可和发送MAIL相同],信笺主题, 信笺内容):Bool;{
   Send Email Unit One By Anskya
   Email:Anskya@Gmail.com
   Web:Www.Yryz.Net
}
unit SendMailAPI;

interface

uses
  windows, winsock;

function DNASendEMail(sSmtp: string; wPort: Word;
  sUser, sPass, sFromMail, sToMail, sSubject, sMailText: string): boolean;

implementation
var
  SendBody          : string;
const
  CRLF              = #13#10;
  BaseTable         : string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/=';

function StrLen(const Str: PChar): Cardinal; assembler;
asm
        MOV      EDX,EDI
        MOV      EDI,EAX
        MOV      ECX,0FFFFFFFFH
        XOR      AL,AL
        REPNE    SCASB
        MOV      EAX,0FFFFFFFEH
        SUB      EAX,ECX
        MOV      EDI,EDX
end;

function StrCopy(Dest: PChar; const Source: PChar): PChar; assembler;
asm
        PUSH     EDI
        PUSH     ESI
        MOV      ESI,EAX
        MOV      EDI,EDX
        MOV      ECX,0FFFFFFFFH
        XOR      AL,AL
        REPNE    SCASB
        NOT      ECX
        MOV      EDI,ESI
        MOV      ESI,EDX
        MOV      EDX,ECX
        MOV      EAX,EDI
        SHR      ECX,2
        REP      MOVSD
        MOV      ECX,EDX
        AND      ECX,3
        REP      MOVSB
        POP      ESI
        POP      EDI
end;

function StrPas(const Str: PChar): string;
begin
  Result := Str;
end;

function FindInTable(CSource: char): integer;
begin
  Result := Pos(string(CSource), BaseTable) - 1;
end;

function EncodeBase64(Source: string): string;
var
  Times, LenSrc, i  : integer;
  x1, x2, x3, x4    : char;
  xt                : byte;
begin
  Result := '';
  LenSrc := length(Source);
  if LenSrc mod 3 = 0 then Times := LenSrc div 3
  else Times := LenSrc div 3 + 1;
  for i := 0 to Times - 1 do begin
    if LenSrc >= (3 + i * 3) then begin
      x1 := BaseTable[(ord(Source[1 + i * 3]) shr 2) + 1];
      xt := (ord(Source[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(Source[2 + i * 3]) shr 4);
      x2 := BaseTable[xt + 1];
      xt := (ord(Source[2 + i * 3]) shl 2) and 60;
      xt := xt or (ord(Source[3 + i * 3]) shr 6);
      x3 := BaseTable[xt + 1];
      xt := (ord(Source[3 + i * 3]) and 63);
      x4 := BaseTable[xt + 1];
    end
    else if LenSrc >= (2 + i * 3) then begin
      x1 := BaseTable[(ord(Source[1 + i * 3]) shr 2) + 1];
      xt := (ord(Source[1 + i * 3]) shl 4) and 48;
      xt := xt or (ord(Source[2 + i * 3]) shr 4);
      x2 := BaseTable[xt + 1];
      xt := (ord(Source[2 + i * 3]) shl 2) and 60;
      x3 := BaseTable[xt + 1];
      x4 := '=';
    end else begin
      x1 := BaseTable[(ord(Source[1 + i * 3]) shr 2) + 1];
      xt := (ord(Source[1 + i * 3]) shl 4) and 48;
      x2 := BaseTable[xt + 1];
      x3 := '=';
      x4 := '=';
    end;
    Result := Result + x1 + x2 + x3 + x4;
  end;

end;

function LookupName(const Name: string): TInAddr;
var
  HostEnt           : PHostEnt;
  InAddr            : TInAddr;
begin
  HostEnt := gethostbyname(PChar(Name));
  FillChar(InAddr, SizeOf(InAddr), 0);
  if HostEnt <> nil then begin
    with InAddr, HostEnt^ do begin
      S_un_b.s_b1 := h_addr^[0];
      S_un_b.s_b2 := h_addr^[1];
      S_un_b.s_b3 := h_addr^[2];
      S_un_b.s_b4 := h_addr^[3];
    end;
  end;

  Result := InAddr;
end;

function StartNet(host: string; port: integer; var sock: integer): boolean;
var
  wsadata           : twsadata;
  FSocket           : integer;
  SockAddrIn        : TSockAddrIn;
  err               : integer;
begin
  err := WSAStartup($0101, wsadata);
  FSocket := socket(PF_INET, SOCK_STREAM, IPPROTO_IP);
  if FSocket = invalid_socket then begin
    Result := False;
    Exit;
  end;
  SockAddrIn.sin_addr := LookupName(host);
  SockAddrIn.sin_family := PF_INET;
  SockAddrIn.sin_port := htons(port);
  err := connect(FSocket, SockAddrIn, SizeOf(SockAddrIn));
  if err = 0 then begin
    sock := FSocket;
    Result := True;
  end
  else begin
    Result := False;
  end;
end;

procedure StopNet(FSocket: integer);
var
  err               : integer;
begin
  err := closesocket(FSocket);
  err := WSACleanup;
end;

function SendData(FSocket: integer; SendStr: string): integer;
var
  DataBuf           : array[0..4096] of char;
  err               : integer;
begin
  StrCopy(DataBuf, PChar(SendStr));
  err := send(FSocket, DataBuf, StrLen(DataBuf), MSG_DONTROUTE);
  Result := err;
end;

function GetData(FSocket: integer): string;
const
  MaxSize           = 1024;
var
  DataBuf           : array[0..MaxSize] of char;
  err               : integer;
begin
  err := recv(FSocket, DataBuf, MaxSize, 0);
  Result := StrPas(DataBuf);
end;

function DNASendEMail(sSmtp: string; wPort: Word;
  sUser, sPass, sFromMail, sToMail, sSubject, sMailText: string): boolean;
var
  FSocket, res      : integer;
  f                 : textfile;
begin
  assignFile(f, 'SendMail.log');
  rewrite(f);
  Result := False;
  if StartNet(sSmtp, wPort, FSocket) then begin
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'HELO ' + sUser + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'AUTH LOGIN' + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, EncodeBase64(sUser) + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, EncodeBase64(sPass) + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'MAIL FROM: <' + sFromMail + '>' + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'RCPT TO: <' + sToMail + '>' + CRLF);
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'DATA' + CRLF);
    writeln(f, GetData(FSocket));
    SendBody := 'From:信息 <' + sFromMail + '>' + CRLF
      + 'To: <' + sTOMail + '>' + CRLF
      + 'Subject: ' + sSubject + CRLF
      + CRLF
      + sMailText + CRLF + '.' + CRLF;
    res := SendData(FSocket, SendBody);
    writeln(f, GetData(FSocket));
    SendData(FSocket, 'QUIT' + CRLF);
    writeln(f, GetData(FSocket));
    StopNet(FSocket);
    if res <> SOCKET_ERROR then begin
      Result := True;
    end;
  end;
  closefile(f);
end;

end.

