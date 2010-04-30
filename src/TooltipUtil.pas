{
    修改者：ghs
    日期：20071218
    功能：在原版本的基础上增加。
          RegisterControl：注册需要提示的控件。
          BeginHelp：设置光标状态为帮助crHelp；
          鼠标弹起后，显示注册的提示信息，同时光标进行还原。

   原版本
   作者：thaoqi
   出处：http://www.2ccc.com/article.asp?articleid=4389
   功能：首先谢谢xsherry大大，来盒子很长一段时间了，老是下东西，没有为盒子做什么贡献。
        前段时间xsherry大大抛砖引玉的文章，给我启发很大，最近一个项目提出要求人
        机交互界面更加有好，尽量少用MessageBox，所以在他的基础上，我试图模仿XP
        登录时候的那个ToolTip提示功能，用API摸索出一个符合要求的ToolTip提示框出
        来，最后我把实现的函数封装成了一个VCL的控件，希望大家能多提意见！
}
unit TooltipUtil;

interface

uses Messages, Windows, SysUtils, Classes, Contnrs, Controls, CommCtrl,
  StdCtrls, ExtCtrls, Consts, Forms, Dialogs, AppEvnts;

type
  TTipTool_ICON = (ttNoneIcon, ttInformationIcon, ttWarningIcon, ttStopIcon);
  TTipAlignment = (taLeft, taCenter, taRight);

  PTipInfo = ^TTipInfo;

  TTipInfo = packed record
    WinControl: TWinControl;
    Handle: THandle;
    Caption: string;
    Msg: string;
    TipICON: TTipTool_ICON;
    TipAlg: TTipAlignment;
    Cursor: TCursor;
  end;

  TToolTip = class(TComponent)
  private
    FTitle: string;
    FText: string;
    FEnabled: Boolean;
    FWindowHandle: HWND;
    FTipHandle: HWND;
    FInterval: Cardinal;
    FToolInfo: TToolInfo;
    FAlignment: TTipAlignment;
    FTipIcon: TTipTool_ICON;
    FControl: TWinControl;
    //
    Flist: TList;
    ApplicationEvents: TApplicationEvents;
    FLastHandle: THandle;

    procedure SetText(AText: string);   //设置气泡提示信息
    procedure SetTitle(ATitle: string); //设置气泡提示的标题

    procedure UpdateTime;               //更新计时器状态
    procedure WndProc(var Msg: TMessage); //接收windows消息
  protected
    //拦截消息=处理左键弹起
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    //结束帮助=设置光标为控件本来状态
    procedure EndHelp;
  public
    constructor Create(AOwner: TComponent); override; //构造函数，创建实例
    destructor Destroy; override;       //析构函数，销毁实例
    //注册控件信息
    procedure RegisterControl(WinControl: TWinControl; aCaption, aMsg: string;
      TipICON: TTipTool_ICON = ttInformationIcon; TipAlignment: TTipAlignment = taLeft);
    //开始帮助=设置光标状态
    procedure BeginHelp;
    procedure Popup(Handle: HWND); overload; //在指定的句柄中弹出气泡（重载）
    procedure Popup(Handle: HWND; IconType: TTipTool_ICON; Title,
      Text: string); overload;          //在指定的句柄中弹出气泡（重载）
    procedure EndPopup;
  published
    //气泡窗体的窗体句柄
    property Handle: HWND read fTipHandle;
    //气泡窗体的提示信息
    property Text: string read fText write SetText;
    //气泡窗体的标题信息
    property Title: string read fTitle write SetTitle;
    //气泡窗体的信息图标
    property ICON: TTipTool_ICON read fTipIcon write fTipIcon;
    //气泡窗体弹出时对齐位置
    property Alignment: TTipAlignment read fAlignment write fAlignment default taLeft;
    //气泡窗体的显示时间
    property Interval: Cardinal read fInterval write fInterval default 1000;
  end;

procedure Register;

implementation

const
  TTS_BALLOON       = $0040;            //ToolTip提示窗口的外形，指定为气球型
  TTS_CLOSE         = $0080;            //关闭按钮
  TTF_PARSELINKS    = $1000;            //可使用超链接
  TTM_SETTITLE      = WM_USER + 32;     //社这提示标题信息的消息

var
  DefTipProc        : Pointer;

function TipWndProc(WinHanlde, MessageID, WParam, LParam: LongWord): Longint; stdcall;
begin
  result := CallWindowProc(DefTipProc, WinHanlde, messageid, wparam, lparam);
  case messageid of
    WM_LBUTTONDOWN, WM_RBUTTONDOWN,
      WM_NCLBUTTONDOWN, WM_NCRBUTTONDOWN:
      SendMessage(WinHanlde, TTM_TRACKACTIVATE, Integer(false), 0);
  end;
end;

constructor TToolTip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if not (AOwner is TWinControl) then
  begin
    raise exception.Create('TToolTip''s owner must be a ''TWinControl'' type.');
    Destroy;
  end;

  fWindowHandle := Classes.AllocateHWnd(WndProc);

  fEnabled := False;
  fInterval := 1000;

  //创建气泡提示窗口
  fTipHandle := CreateWindow(TOOLTIPS_CLASS, nil,
    WS_POPUP or TTS_NOPREFIX or
    TTS_BALLOON or TTS_ALWAYSTIP,       // or TTS_CLOSE,
    0, 0, 0, 0, fWindowHandle,
    0, HInstance, nil);

  defTipProc := pointer(GetWindowLong(FTipHandle, GWL_WndProc));
  SetWindowLong(FTipHandle, GWL_WNDPROC, longint(@TipwndProc));
  if fTipHandle <> 0 then
  begin
    //设置ToolInfo的大小
    fToolInfo.cbSize := SizeOf(fToolInfo);
    //设置基本风格
    fToolInfo.uFlags := TTF_PARSELINKS or TTF_IDISHWND or TTF_TRACK;
    //设置所有者的句柄
    fToolInfo.uId := fWindowHandle;
  end;
  Flist := TList.Create;
  ApplicationEvents := TApplicationEvents.Create(nil);
  ApplicationEvents.OnMessage := ApplicationEvents1Message;
end;

destructor TToolTip.Destroy;
var
  I                 : Integer;
  tmpTipInfo        : PTipInfo;
begin
  if fTipHandle <> 0 then
    CloseWindow(fTipHandle);
  for I := Flist.Count - 1 downto 0 do  // Iterate
  begin
    tmpTipInfo := PTipInfo(FList.Items[i]);
    Dispose(tmpTipInfo);
  end;                                  // for
  Flist.Free;
  ApplicationEvents.Free;
  DeallocateHWnd(fWindowHandle);
  inherited Destroy;
end;

procedure TToolTip.SetText(AText: string);
begin
  fText := AText;

  if fTipHandle <> 0 then
  begin
    //设置标题信息
    fToolInfo.lpszText := PAnsiChar(fText);
    //向气泡窗体发送消息，将ToolInfo的信息设置到气泡窗体中
    SendMessage(fTipHandle, TTM_ADDTOOL, 0, Integer(@fToolInfo));
    SendMessage(fTipHandle, TTM_SETTOOLINFO, 0, Integer(@fToolInfo));
  end;
end;

procedure TToolTip.SetTitle(ATitle: string);
begin
  fTitle := ATitle;

  if fTipHandle <> 0 then
    //设置气泡窗体的提示图标和标题信息
    SendMessage(fTipHandle, TTM_SETTITLE, Integer(fTipIcon), Integer(fTitle));
end;

procedure TToolTip.Popup(Handle: HWND);
var
  tmpRect           : TRect;
  x, y              : word;
begin
  x := 0;

  fControl := FindControl(Handle);
  if fControl.Hint <> '' then
    fControl.ShowHint := False;

  //得到需要显示窗体所在的屏幕区域
  GetWindowRect(Handle, tmpRect);

  //计算显示区域位置的坐标
  with tmpRect do
  begin
    y := (Bottom - Top) div 2 + Top;

    case fAlignment of
      taLeft: x := Left;
      taCenter: x := (Right - Left) div 2 + Left;
      taRight: x := Right;
    end;
  end;

  //设置气泡窗体弹出的坐标
  SendMessage(fTipHandle, TTM_TRACKPOSITION, 0, MAKELONG(x, y));
  //激活气泡窗体，并显示出来
  SendMessage(fTipHandle, TTM_TRACKACTIVATE, Integer(True), Integer(@fToolInfo));

  fEnabled := True;
  //更新计时器状态
  UpdateTime;
end;

procedure TToolTip.WndProc(var Msg: TMessage);
begin
  fEnabled := False;
  with Msg do
  begin
    case Msg of
      WM_TIMER:
        try
          SendMessage(fTipHandle, TTM_TRACKACTIVATE,
            Integer(False), Integer(@fToolInfo));
          if fControl.Hint <> '' then
            fControl.ShowHint := True;
        except
          Application.HandleException(Self);
        end;
    else
      Result := DefWindowProc(fWindowHandle, Msg, wParam, lParam);
    end;
  end;
  //更新计时器状态
  UpdateTime;
end;

procedure TToolTip.Popup(Handle: HWND; IconType: TTipTool_ICON;
  Title: string; Text: string);
begin
  fTipIcon := IconType;

  SetTitle(Title);
  SetText(Text);

  Popup(Handle);
end;

procedure TToolTip.UpdateTime;
begin
  KillTimer(fWindowHandle, 1);
  if (FInterval <> 0) and FEnabled then
    if SetTimer(fWindowHandle, 1, FInterval, nil) = 0 then
      raise EOutOfResources.Create(SNoTimers);
end;

procedure Register;
begin
  RegisterComponents('ToolTip', [TToolTip]);
end;

procedure TToolTip.RegisterControl(WinControl: TWinControl; aCaption, aMsg: string;
  TipICON: TTipTool_ICON = ttInformationIcon; TipAlignment: TTipAlignment = taLeft);
var
  TipInfo           : PTipInfo;
begin
  New(TipInfo);
  TipInfo.WinControl := WinControl;
  TipInfo.Handle := WinControl.Handle;
  TipInfo.Caption := aCaption;
  Tipinfo.Msg := aMsg;
  TipInfo.TipICON := TipICON;
  TIpInfo.TipAlg := TipAlignment;
  TipInfo.Cursor := WinControl.Cursor;

  Flist.Add(TipInfo);
end;

procedure TToolTip.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
var
  I                 : Integer;
  tmpTipInfo        : PTipInfo;
  tmpPoint          : TPoint;
  tmpHandle         : THandle;
begin
  if Msg.message = WM_LBUTTONUP then
  begin
    GetCurSorPos(tmpPoint);
    tmpHandle := WindowFromPoint(tmpPoint);
    if FLastHandle <> tmpHandle then    //防止不停触发
    begin
      FLastHandle := tmpHandle;
      for I := 0 to FList.Count - 1 do  // Iterate
      begin
        tmpTipInfo := PTipInfo(FList.Items[i]);
        //只有调用了BeginHelp，才会弹出提示窗口
        if (tmpTipInfo.Handle = tmpHandle) and (tmpTipInfo.WinControl.Cursor = crHelp) then
        begin
          Popup(tmpHandle, tmpTipInfo.TipICON, tmpTipInfo.Caption, tmpTipInfo.Msg);
          break;
        end;
      end;                              // for
      EndHelp;
      DefWindowProc(Msg.hwnd, Msg.message, Msg.wParam, Msg.lParam);
    end;
  end;

end;

procedure TToolTip.BeginHelp;
var
  i                 : Integer;
  tmpTipInfo        : PTipInfo;
begin
  for I := 0 to FList.Count - 1 do      // Iterate
  begin
    tmpTipInfo := PTipInfo(FList.Items[i]);
    tmpTipInfo.WinControl.Cursor := crHelp;
  end;                                  // for
end;

procedure TToolTip.EndHelp;
var
  i                 : Integer;
  tmpTipInfo        : PTipInfo;
begin
  for I := 0 to FList.Count - 1 do      // Iterate
  begin
    tmpTipInfo := PTipInfo(FList.Items[i]);
    tmpTipInfo.WinControl.Cursor := tmpTipInfo.Cursor;
  end;                                  // for
end;

procedure TToolTip.EndPopup;
begin
  SendMessage(fTipHandle, TTM_TRACKACTIVATE, Integer(false), 0);
end;

end.

