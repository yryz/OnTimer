
{ 消息提示窗口 }

unit PopTooltip_u;

interface
uses
  Windows, Messages, SysUtils, Classes, GDIPAPI, GDIPOBJ, Dialogs;

const
  POP_WIN_WIDTH     = 238;
  POP_WIN_HEIGH     = 139;

  TIMER_CLOSE       = 1;
  TIMER_ANIMATE     = 2;
type
  TPopTooltip = class
  private                               { Wnd }
    FHandle: HWND;
    FTimer: THandle;
    FWndClass: string;
    FLeft, FTop, FWidth, FHeight: Integer;
    FRight, FBottom: Integer;
    FIsQuitAnimate: Boolean;
    procedure WndProc(var Msg: TMessage);
    function RegWindowClass: boolean;
  private                               { GDI }
    FMsg: string;
    FImage: TGPImage;
    FGraph: TGPGraphics;
  protected
    function Show: Boolean;
    property Msg: string read FMsg write FMsg;
  public
    constructor Create(sMsg, ImgPath: string; dWaitTime: DWORD);
    destructor Destroy; override;
    class function ShowMsg(sMsg, bgImgPath: string; dWaitTime: DWORD): Boolean;
  end;

implementation

procedure DwBG(graphics: TGPGraphics; X, Y, W, H: Integer); //背景
var
  pen               : TGPPen;
  brush             : TGPSolidBrush;
  path              : TGPGraphicsPath;
begin
  pen := TGPPen.Create(MakeColor(100, 255, 255, 255)); //边框着色
  brush := TGPSolidBrush.Create(MakeColor(30, 255, 255, 255)); //中心着色
  path := TGPGraphicsPath.Create;

  pen.SetWidth(1);
  path.Reset;
  path.AddRectangle(MakeRect(X, Y, W, H));
  graphics.DrawPath(pen, path);
  graphics.fillPath(brush, path);

  pen.Free;
  path.Free;
  brush.Free;
end;

procedure GDITextOut(graphics: TGPGraphics; X, Y, W, H: Single;
  FontSize: Integer; StringAlign: StringAlignment; str: string); //文本
var
  FontFamily        : TGPFontFamily;
  font              : TGPFont;
  rectF             : TGPRectF;
  stringFormat      : TGPStringFormat;
  solidBrush        : TGPSolidBrush;
begin
  FontFamily := TGPFontFamily.Create('Arial');
  font := TGPFont.Create(FontFamily, FontSize, FontStyleRegular, UnitPoint);

  stringFormat := TGPStringFormat.Create;
  stringFormat.SetAlignment(StringAlign); // Center-justify each line of text.
  stringFormat.SetLineAlignment(StringAlignmentNear); //StringAlignmentCenter  ,Center the block of text (top to bottom) in the rectangle.

  rectF := MakeRect(X, Y, W, H);
  solidBrush := TGPSolidBrush.Create(MakeColor(255, 0, 0, 0));
  graphics.DrawString(str, -1, font, rectF, stringFormat, solidBrush);

  //  solidBrush := TGPSolidBrush.Create(MakeColor(255, 255, 255, 255));
  //  rectF := MakeRect(X - 1, Y - 1, W, 500.0);
  //  graphics.DrawString(str, -1, font, rectF, stringFormat, solidBrush);

  FontFamily.Free;
  font.Free;
  stringFormat.Free;
  solidBrush.Free;
end;

{ TPopTooltip }

function TPopTooltip.RegWindowClass: boolean;
var
  WindowClass       : TWndClass;
begin
  if GetClassInfo(HInstance, PChar(FWndClass),
    WindowClass) then
    Windows.UnRegisterClass(@WindowClass, HInstance);

  WindowClass.Style := CS_HREDRAW or CS_VREDRAW;
  WindowClass.lpfnWndProc := @DefWindowProc;
  WindowClass.cbClsExtra := 0;
  WindowClass.cbWndExtra := 0;
  WindowClass.Hinstance := Hinstance;
  WindowClass.HIcon := 0;
  WindowClass.hCursor := LoadCursor(0, IDC_ARROW);
  WindowClass.hbrBackground := COLOR_BTNFACE;
  WindowClass.lpszMenuName := nil;
  WindowClass.lpszClassName := PChar(FWndClass);
  Result := Windows.RegisterClass(WindowClass) <> 0;
end;

var
  RegWindowClassed  : Boolean;

constructor TPopTooltip.Create;
var
  rect              : TRect;
begin
  FMsg := sMsg;
  FWndClass := Self.ClassName + 'Wnd';
  { 矩形区 }
  FRight := GetSystemMetrics(SM_CXSCREEN);
  SystemParametersInfo(SPI_GETWORKAREA, 0, @rect, 0);
  FBottom := rect.Bottom;

  { 主窗体 }
  if not RegWindowClassed then
    RegWindowClassed := RegWindowClass;
  if RegWindowClassed then begin
    FHandle := CreateWindowEx(WS_EX_TOPMOST or WS_EX_TOOLWINDOW,
      PChar(FWndClass), '任务消息', WS_POPUP,
      -POP_WIN_WIDTH, -POP_WIN_WIDTH, POP_WIN_WIDTH, POP_WIN_HEIGH, { 动态调整 }
      0, 0, Hinstance, nil);

    if FHandle = 0 then raise Exception.Create('Create Windows Error!');

    { 设置样式 }
    SetWindowLong(FHandle, GWL_WNDPROC, Longint(MakeObjectInstance(WndProc)));
    SetWindowPos(FHandle, GetTopWindow(0), 0, 0, 0, 0, SWP_NOMOVE or
      SWP_NOSIZE or SWP_NOACTIVATE);

    { 画布 }
    FImage := TGPImage.Create(ImgPath);
    FGraph := TGPGraphics.Create(FHandle, False);

    { 定时 }
    FTimer := SetTimer(FHandle, TIMER_ANIMATE, 10, nil); //动画
    FTimer := SetTimer(FHandle, TIMER_CLOSE, dWaitTime, nil);
  end;
end;

destructor TPopTooltip.Destroy;
begin
  if Assigned(FImage) then FreeAndNil(FImage);
  if Assigned(FGraph) then FreeAndNil(FGraph);
  Classes.DeallocateHWnd(FHandle);
  inherited Destroy;
end;

procedure TPopTooltip.WndProc(var Msg: TMessage);
begin
  with Msg do begin
    case Msg of
      WM_ERASEBKGND:
        begin
          if Assigned(FImage) then      { 背景 }
            FGraph.DrawImage(FImage, 0, 0, FWidth, FHeight);
          { 消息内容 }
          if (FMsg <> '') and (FWidth >= POP_WIN_WIDTH) {不是动画} then
            GDITextOut(FGraph, 10, 30, POP_WIN_WIDTH - 2 * 10, POP_WIN_HEIGH - 40, 9,
              StringAlignmentNear, FMsg);
        end;
      WM_LBUTTONDOWN: begin
          //OutputDebugString(PChar(IntToStr(LParamLo)+'|'+IntToStr(LParamHi)));
          if (LParamLo >= FWidth - 10)
            and (LParamHi < 10) then begin { 点击关闭图标 }
            FIsQuitAnimate := True;
            SetTimer(FHandle, TIMER_ANIMATE, 10, nil);
          end else begin                { 拖动 }
            ReleaseCapture;
            PostMessage(FHandle, WM_SYSCOMMAND, $F012, 0);
          end;
        end;
      WM_DESTROY: begin
          SetWindowLong(FHandle, GWL_WNDPROC, LongInt(@DefWindowProc)); //一定要
          Self.Free;
        end;
      WM_MOUSEFIRST: begin              { 取消自动关闭 }
          if FTimer > 0 then begin
            KillTimer(FHandle, FTimer);
            FTimer := 0;
          end;
        end;
      WM_TIMER: begin                   { 自动关闭 }
          case WParam of
            TIMER_CLOSE: begin
                KillTimer(FHandle, WParam);
                FIsQuitAnimate := True;
                SetTimer(FHandle, TIMER_ANIMATE, 10, nil);
              end;
            TIMER_ANIMATE: begin
                if not FIsQuitAnimate then begin
                  if FWidth < POP_WIN_WIDTH then begin
                    Inc(FWidth, 15);
                    if FWidth > POP_WIN_WIDTH then FWidth := POP_WIN_WIDTH;
                    FHeight := Round(FWidth * (POP_WIN_HEIGH / POP_WIN_WIDTH));
                    FLeft := FRight - FWidth;
                    FTop := FBottom - FHeight;
                    SetWindowPos(FHandle, 0, FLeft, FTop, FWidth, FHeight, 0);
                  end else
                    KillTimer(FHandle, WParam);
                end else begin
                  if FWidth > 0 then begin
                    Dec(FWidth, 20);
                    if FWidth < 0 then FWidth := 0;
                    FHeight := Round(FWidth * (POP_WIN_HEIGH / POP_WIN_WIDTH));
                    FLeft := FRight - FWidth;
                    FTop := FBottom - FHeight;
                    SetWindowPos(FHandle, 0, FLeft, FTop, FWidth, FHeight, 0);
                  end else begin
                    KillTimer(FHandle, WParam);
                    PostMessage(FHandle, WM_CLOSE, 0, 0);
                  end;
                end;
              end;

          end;
        end;
    else
      Result := DefWindowProc(FHandle, Msg, wParam, lParam);
    end;
  end;
end;

function TPopTooltip.Show: Boolean;
begin
  Result := ShowWindow(FHandle, SW_SHOW);
end;

class function TPopTooltip.ShowMsg;
var
  PopTooltip        : TPopTooltip;
begin
  PopTooltip := TPopTooltip.Create('　　' + sMsg, bgImgPath, dWaitTime);
  Result := PopTooltip.Show;
end;

end.

