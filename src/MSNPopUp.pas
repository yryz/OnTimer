{
调用:

var
  FM_MSNPopUp: TMSNPopUp;
begin
  FM_MSNPopUp := TMSNPopUp.Create(nil);
  try
    with FM_MSNPopUp do begin
      Text := '你好啊，呵呵。拉拉你好啊，' + #13#10'，嘻嘻^_^呵呵';
      Font.Name := '宋体';
      Font.Size := 10;
      Font.Color := $ff;
      HoverFont.Name := '宋体';
      HoverFont.Color := clred;
      //BackgroundImage.LoadFromFile('6.bmp');
      ShowPopUp;
    end;
  finally
    if assigned(FM_MSNPopUp) then begin
      FM_MSNPopUp := nil;
      FM_MSNPopUp.Free;
    end;
  end;
end;
}

unit MSNPopUp;

interface

uses
  Windows, Classes, Graphics, StdCtrls, ExtCtrls, Controls, Forms,
  ShellApi, SysUtils, Messages;

// fix for Delphi-5 users
const
  WS_EX_NOACTIVATE = $8000000;

type
  TOrientation = (mbHorizontal, mbVertical);
  TScrollSpeed = 1..50;

  TMSNPopupOption = (msnSystemFont, msnCascadePopups, msnAllowScroll);

  // Anomy
  TMSNImageDrawMethod = (dmActualSize, dmTile, dmFit);

  TMSNPopupOptions = set of TMSNPopupOption;

  TMSNPopUp = class(TComponent)
  private
    { Private declarations }
    FText: string;
    FTitle: string;
    FWidth: Integer;
    FHeight: Integer;
    FTimeOut: Integer;
    FScrollSpeed: TScrollSpeed;
    FColor1: TColor;
    FColor2: TColor;
    FGradientOrientation: TOrientation;
    FFont: TFont;
    FHoverFont: TFont;
    FTitleFont: TFont;
    FCursor: TCursor;
    FOptions: TMSNPopupOptions;

    // Anomy
    FTextAlignment: TAlignment;
    FBackgroundDrawMethod: TMSNImageDrawMethod;

    //Jelmer
    FPopupMarge, FPopupStartX, FPopupStartY: Integer;
    PopupCount, NextPopupPos: Integer;
    LastBorder: Integer;
    FDefaultMonitor: TDefaultMonitor;
    FBackground: TBitmap;

    FOnClick: TNotifyEvent;

    function GetEdge: Integer;
    function GetCaptionFont: TFont;

    // Jiang Hong
    procedure SetFont(Value: TFont);
    procedure SetHoverFont(Value: TFont);
    procedure SetTitleFont(Value: TFont);

    //Jelmer
    procedure SetBackground(Value: TBitmap);
  public
    { Public declarations }
    function ShowPopUp: Boolean;
    procedure ClosePopUps;
  published
    { Published declarations }
    property Text: string read FText write FText;
    property TimeOut: Integer read FTimeOut write FTimeOut default 10;
    property Width: Integer read FWidth write FWidth default 175;
    property Height: Integer read FHeight write FHeight default 175;
    property GradientColor1: TColor read FColor1 write FColor1;
    property GradientColor2: TColor read FColor2 write FColor2;
    property GradientOrientation: TOrientation read FGradientOrientation write FGradientOrientation default mbVertical;
    property ScrollSpeed: TScrollSpeed read FScrollSpeed write FScrollSpeed default 5;
    property Font: TFont read FFont write SetFont;
    property HoverFont: TFont read FHoverFont write SetHoverFont; //Jiang Hong
    property Title: string read FTitle write FTitle; //Jiang Hong
    property TitleFont: TFont read FTitleFont write SetTitleFont; //Jiang Hong
    property Options: TMSNPopupOptions read FOptions write FOptions;
    // Anomy
    property TextAlignment: TAlignment read FTextAlignment write FTextAlignment default taLeftJustify;
    property BackgroundDrawMethod: TMSNImageDrawMethod read FBackgroundDrawMethod write FBackgroundDrawMethod default dmActualSize;
    //Jelmer
    property TextCursor: TCursor read FCursor write FCursor;
    property PopupMarge: Integer read FPopupMarge write FPopupMarge;
    property PopupStartX: Integer read FPopupStartX write FPopupStartX;
    property PopupStartY: Integer read FPopupStartY write FPopupStartY;
    property DefaultMonitor: TDefaultMonitor read FDefaultMonitor write FDefaultMonitor;
    property BackgroundImage: TBitmap read FBackground write SetBackground;

    property OnClick: TNotifyEvent read FOnClick write FOnClick;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

type
  TfrmMSNPopUp = class(TCustomForm)
    pnlBorder: TPanel;
    lblText: TLabel;
    imgGradient: TImage;

    tmrExit: TTimer;
    tmrScroll: TTimer;
    // add by Ahmed Hamed 20-3-2002
    tmrScrollDown: TTimer;
    //
    lblTitle: TLabel;
    // Added by Anomy

    procedure tmrExitTimer(Sender: TObject);
    // add by Ahmed Hamed 20-3-2002
    procedure tmrscrollDownTimer(Sender: TObject);
    //
    procedure lblTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure lblTitleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure tmrScrollTimer(Sender: TObject);
    procedure imgGradientMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
  private
    //Jelmer
    PopupPos: Integer;
    ParentMSNPopup: TMSNPopUp;
    CanClose: Boolean;

    // Anomy
    BGDrawMethod: TMSNImageDrawMethod;

    function CalcColorIndex(StartColor, EndColor: TColor; Steps, ColorIndex: Integer): TColor;
    procedure PositionText;
    function IsWinXP: Boolean;
  public
    { Public declarations }
    Text, Title: string;
    TimeOut: Integer;
    sWidth: Integer;
    sHeight: Integer;
    bScroll, bHyperlink: Boolean;
    Color1, Color2: TColor;
    Orientation: TOrientation;
    ScrollSpeed: TScrollSpeed;
    Font, HoverFont, TitleFont: TFont;
    StoredBorder: Integer;
    Cursor: TCursor;

    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure PopUp;
  end;

procedure Register;
function IsNT4: Boolean;

implementation

function IsNT4: Boolean;
var
  VersionInfo: _OSVERSIONINFOA;
begin
  Result := False;

  VersionInfo.dwOSVersionInfoSize := sizeof(VersionInfo);
  GetVersionEx(VersionInfo);

  if VersionInfo.dwPlatformId = VER_PLATFORM_WIN32_NT then begin
    if (VersionInfo.dwMajorVersion <= 4) and ((VersionInfo.dwMinorVersion = 0) or
      (VersionInfo.dwMinorVersion = 51)) then
      Result := True;
  end;
end;

procedure Register;
begin
  RegisterComponents('Custom', [TMSNPopUp]);
end;

// component stuff

function TMSNPopUp.ShowPopUp: Boolean;
var
  r: TRect;
  MSNPopUp: TfrmMSNPopUp;
begin
  //Jelmer
  if GetEdge <> LastBorder then begin
    LastBorder := GetEdge;
    PopupCount := 0;
  end;
  Result := True;
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);
  if PopupCount > 0 then begin
    case LastBorder of
      ABE_BOTTOM:
        if (r.Bottom - (NextPopupPos + FHeight + PopupStartY)) < 0 then begin
          Result := False;
          Exit;
        end;
      ABE_LEFT:
        if (NextPopupPos + FWidth + PopupStartX) > r.Right then begin
          Result := False;
          Exit;
        end;
      ABE_RIGHT:
        if (r.Right - (NextPopupPos + FHeight + PopupStartY)) < 0 then begin
          Result := False;
          Exit;
        end;
      ABE_TOP:
        if ((NextPopupPos + FHeight + PopupStartY)) > r.Bottom then begin
          Result := False;
          Exit;
        end;
    end;
  end
  else
    NextPopupPos := 0;
  Inc(PopupCount);

  MSNPopUp := TfrmMSNPopUp.CreateNew(Self.Owner);

  with MSNPopUp do begin
    Hide;
    //Jelmer
    ParentMSNPopup := Self;
    DefaultMonitor := FDefaultMonitor;

    sWidth := FWidth;
    sHeight := FHeight;

    Text := FText;
    Title := FTitle;
    TimeOut := FTimeOut;

    bScroll := msnAllowScroll in FOptions;
    ScrollSpeed := FScrollSpeed;

    Font := FFont;
    HoverFont := FHoverFont;
    TitleFont := FTitleFont;
    Cursor := FCursor;

    Color1 := FColor1;
    Color2 := FColor2;
    Orientation := FGradientOrientation;

    // Anomy
    lblText.Alignment := FTextAlignment;
    pnlBorder.Align := alClient;
    BGDrawMethod := FBackgroundDrawMethod;

    PopUp;
  end;


end;

// JWB
procedure TMSNPopUp.ClosePopUps;
var
  hTfrmMSNPopUp: Cardinal;
  i: Integer;
begin
  for i := 0 to PopupCount do begin
    hTfrmMSNPopUp := FindWindow(PChar('TfrmMSNPopUP'), nil);
    if hTfrmMSNPopUp <> 0 then begin
      SendMessage(hTfrmMSNPopUp, WM_CLOSE, 0, 0);
      Application.ProcessMessages;
    end;
  end;
end;

//Jiang Hong
procedure TMSNPopUp.SetFont(Value: TFont);
begin
  if not (FFont = Value) then
    FFont.Assign(Value);
end;

//Jiang Hong
procedure TMSNPopUp.SetHoverFont(Value: TFont);
begin
  if not (FHoverFont = Value) then
    FHoverFont.Assign(Value);
end;

//Jiang Hong
procedure TMSNPopUp.SetTitleFont(Value: TFont);
begin
  if not (FTitleFont = Value) then
    FTitleFont.Assign(Value);
end;

// function to find out system's default font
function TMSNPopUp.GetCaptionFont: TFont;
var
  ncMetrics: TNonClientMetrics;
begin
  ncMetrics.cbSize := sizeof(TNonClientMetrics);
  SystemParametersInfo(SPI_GETNONCLIENTMETRICS, sizeof(TNonClientMetrics), @ncMetrics, 0);

  Result := TFont.Create;
  Result.Handle := CreateFontIndirect(ncMetrics.lfMenuFont);
end;

constructor TMSNPopUp.Create(AOwner: TComponent);
begin
  inherited;
  //FOptions := [msnAllowScroll, msnAllowHyperlink, msnAutoOpenURL, msnCascadePopups];
  FOptions := [msnAllowScroll, msnCascadePopups];

  FFont := TFont.Create;
  FHoverFont := TFont.Create;
  FTitleFont := TFont.Create;

  //Jelmer
  FBackground := TBitmap.Create();

  if msnSystemFont in FOptions then begin
    FFont.Name := GetCaptionFont.Name;
    FHoverFont.Name := GetCaptionFont.Name;
    FTitleFont.Name := GetCaptionFont.Name;
  end;

  FWidth := 200;
  FHeight := 100;

  FTimeOut := 10;
  FScrollSpeed := 9;

  FText := '';
  FTitle := '';

  FCursor := crDefault;

  FColor1 := clWhite;
  FColor2 := $00FFEED5;
  FGradientOrientation := mbVertical;

  FHoverFont.Style := [fsUnderline];
  FHoverFont.Color := clBlue;

  FTitleFont.Style := [fsBold];

  // Anomy
  FBackgroundDrawMethod := dmActualSize;
  FTextAlignment := taCenter;

  //Jelmer
  PopupCount := 0;
  LastBorder := 0;
  FPopupMarge := 2;
  FPopupStartX := 16;
  FPopupStartY := 2;
  //---
end;

destructor TMSNPopUp.Destroy;
begin
  FFont.Free;
  FHoverFont.Free;
  FTitleFont.Free;

  //Jelmer
  FBackground.Free;

  inherited;
end;

// form's functions

procedure TfrmMSNPopUp.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000; // MS 12/01/2002
begin
  inherited;
  Params.Style := Params.Style and not WS_CAPTION or WS_POPUP;

  if IsNT4 then
    Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW
  else
    Params.ExStyle := Params.ExStyle or WS_EX_TOOLWINDOW or WS_EX_NOACTIVATE;

  if (IsWinXP) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  Params.WndParent := GetDesktopWindow;
end;

constructor TfrmMSNPopUp.CreateNew(AOwner: TComponent; Dummy: Integer = 0);
begin
  inherited;
  BorderStyle := bsNone; //bsDialog;

  pnlBorder := TPanel.Create(Self);
  with pnlBorder do begin
    Parent := Self;
    Align := alClient;
    BevelWidth := 1;
    //BevelInner:=bvLowered;
   // BevelOuter:=bvRaised;
  end;

  imgGradient := TImage.Create(Self);
  with imgGradient do begin
    Parent := pnlBorder;
    Align := alClient;
    Anchors := [akTop, akLeft, akRight, akBottom];
    OnMouseUp := Self.imgGradientMouseUp;
  end;

  lblText := TLabel.Create(Self);
  with lblText do begin
    ShowAccelChar := False;
    Layout := tlCenter;
    AutoSize := True;
    WordWrap := True;
    Parent := pnlBorder;
    Transparent := True;
    OnMouseUp := Self.lblTextMouseUp;
    Left := 9;
    Top := 49;
    Width := 3;
    Height := 13;
  end;

  lblTitle := TLabel.Create(Self);
  with lblTitle do begin
    ShowAccelChar := False;
    Parent := pnlBorder;
    Transparent := True;
    Top := 6;
    Left := 48;
    OnMouseUp := Self.lblTitleMouseUp;

  end;

  tmrExit := TTimer.Create(Self);
  with tmrExit do begin
    Enabled := False;
    OnTimer := tmrExitTimer;
    Interval := 10000;
  end;

  tmrScroll := TTimer.Create(Self);
  with tmrScroll do begin
    Enabled := False;
    OnTimer := tmrScrollTimer;
    Interval := 25;
  end;


  // add by Ahmed Hamed 20-3-2002
  tmrScrollDown := TTimer.Create(Self);
  with tmrScrollDown do begin
    Enabled := False;
    OnTimer := tmrscrollDownTimer;
    Interval := 25;
  end;

end;

function TMSNPopUp.GetEdge: Integer;
var
  AppBar: TAppbarData;
begin
  Result := -1;

  FillChar(AppBar, sizeof(AppBar), 0);
  AppBar.cbSize := sizeof(AppBar);

  if ShAppBarMessage(ABM_GETTASKBARPOS, AppBar) <> 0 then begin
    if ((AppBar.rc.Top = AppBar.rc.Left) and (AppBar.rc.Bottom > AppBar.rc.Right)) then
      Result := ABE_LEFT
    else if ((AppBar.rc.Top = AppBar.rc.Left) and (AppBar.rc.Bottom < AppBar.rc.Right)) then
      Result := ABE_TOP
    else if (AppBar.rc.Top > AppBar.rc.Left) then
      Result := ABE_BOTTOM
    else
      Result := ABE_RIGHT;
  end;
end;

procedure TfrmMSNPopUp.PopUp;
var
  r: TRect;
  gradient: TBitmap;
  i: Integer;
  tileX, tileY: Integer;
  //Jelmer
  OldLeft, OldTop: Integer;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);

  Self.AutoScroll := False;

  Self.Height := sHeight;
  Self.Width := sWidth;

  lblText.Cursor := Cursor;

  //Jelmer
  StoredBorder := ParentMSNPopup.GetEdge;
  CanClose := True;

  case StoredBorder of
    ABE_LEFT: begin
        Self.Left := r.Left + ParentMSNPopup.PopupStartX;

        //Jelmer
        Self.Top := r.Bottom - ParentMSNPopup.PopupStartY - Self.Height - ParentMSNPopup.NextPopupPos;
      end;

    ABE_TOP: begin
        Self.Left := r.Right - Self.Width - ParentMSNPopup.PopupStartX;

        //Jelmer
        Self.Top := r.Top + ParentMSNPopup.PopupStartY + ParentMSNPopup.NextPopupPos;
      end;

    ABE_BOTTOM: begin
        Self.Left := r.Right - Self.Width - ParentMSNPopup.PopupStartX;

        //Jelmer
        Self.Top := r.Bottom - ParentMSNPopup.PopupStartY - Self.Height - ParentMSNPopup.NextPopupPos;
      end;

    ABE_RIGHT: begin
        Self.Left := r.Right - Self.Width - ParentMSNPopup.PopupStartX;

        //Jelmer
        Self.Top := r.Bottom - ParentMSNPopup.PopupStartY - Self.Height - ParentMSNPopup.NextPopupPos;
      end;
  end;

  //Jelmer
  PopupPos := ParentMSNPopup.NextPopupPos;
  if msnCascadePopups in ParentMSNPopup.FOptions = True then begin
    if (StoredBorder = ABE_BOTTOM) or (StoredBorder = ABE_TOP) then begin
      ParentMSNPopup.NextPopupPos := ParentMSNPopup.NextPopupPos + sHeight + ParentMSNPopup.FPopupMarge;
    end
    else if (StoredBorder = ABE_RIGHT) or (StoredBorder = ABE_LEFT) then begin
      ParentMSNPopup.NextPopupPos := ParentMSNPopup.NextPopupPos + sHeight + ParentMSNPopup.FPopupMarge;
    end;
  end
  else
    ParentMSNPopup.NextPopupPos := 0;

  lblTitle.Font := TitleFont;
  lblTitle.Caption := Title;

  //Jelmer
  OldLeft := Left;
  OldTop := Top;
  Left := -Width - 10;
  Top := -Height - 10;
  Visible := True;
  Visible := False;
  Left := OldLeft;
  Top := OldTop;

  imgGradient.Align := alClient;
  imgGradient.Align := alNone;
  pnlBorder.Align := alNone;

  //Jelmer
  if Self.ParentMSNPopup.FBackground.Empty then begin
    gradient := TBitmap.Create;
    gradient.Width := imgGradient.Width;
    gradient.Height := imgGradient.Height;

    if Orientation = mbVertical then begin
      for i := 0 to gradient.Height do begin
        gradient.Canvas.Pen.Color := CalcColorIndex(Color1, Color2, gradient.Height + 1, i + 1);
        gradient.Canvas.MoveTo(0, i);
        gradient.Canvas.LineTo(gradient.Width, i);
      end;
    end;

    if Orientation = mbHorizontal then begin
      for i := 0 to gradient.Width do begin
        gradient.Canvas.Pen.Color := CalcColorIndex(Color1, Color2, gradient.Height + 1, i + 1);
        gradient.Canvas.MoveTo(i, 0);
        gradient.Canvas.LineTo(i, gradient.Height);
      end;
    end;

    imgGradient.Canvas.Draw(0, 0, gradient);
    gradient.Free;
  end
  else begin
    //Jelmer
    //imgGradient.Canvas.
    ParentMSNPopup.FBackground.Transparent := True;

    // Anomy
    case BGDrawMethod of
      dmActualSize:
        imgGradient.Canvas.Draw(0, 0, ParentMSNPopup.BackgroundImage);
      dmTile: begin
          tileX := 0;
          while tileX < imgGradient.Width do begin
            tileY := 0;
            while tileY < imgGradient.Height do begin
              imgGradient.Canvas.Draw(tileX, tileY, ParentMSNPopup.BackgroundImage);
              tileY := tileY + ParentMSNPopup.BackgroundImage.Height;
            end;
            tileX := tileX + ParentMSNPopup.BackgroundImage.Width;
          end;
        end;
      dmFit:
        imgGradient.Canvas.StretchDraw(Bounds(0, 0, imgGradient.Width,
          imgGradient.Height),
          ParentMSNPopup.BackgroundImage
          );
    end;
  end;

  lblTitle.Left := 8;
  tmrExit.Interval := TimeOut * 1000;

  if bScroll then begin
    case ParentMSNPopup.GetEdge of
      ABE_TOP: begin
          Self.Height := 1;
        end;

      ABE_BOTTOM: begin
          Self.Top := Self.Top + Self.Height;
          Self.Height := 1;
        end;

      ABE_LEFT: begin
          Self.Width := 1;
        end;

      ABE_RIGHT: begin
          Self.Left := Self.Left + Self.Width;
          Self.Width := 1;
        end;
    end;
    tmrScroll.Enabled := True;
  end;

  if not bScroll then
    tmrExit.Enabled := True;

  Self.FormStyle := fsStayOnTop;
  ShowWindow(Self.Handle, SW_SHOWNOACTIVATE);
  Self.Visible := True;

  lblText.Font := HoverFont;
  PositionText;

  lblText.Font := Font;
  PositionText;
end;

procedure TfrmMSNPopUp.PositionText;
begin
  lblText.Caption := Text;
  lblText.Width := pnlBorder.Width - 15;

  lblText.Left := Round((pnlBorder.Width - lblText.Width) / 2);
  lblText.Top := Round((pnlBorder.Height - lblText.Height) / 2);
end;

function TfrmMSNPopUp.CalcColorIndex(StartColor, EndColor: TColor; Steps, ColorIndex: Integer): TColor;
var
  BeginRGBValue: array[0..2] of Byte;
  RGBDifference: array[0..2] of Integer;
  Red, Green, Blue: Byte;
  NumColors: Integer;
begin
  // Initialize
  NumColors := Steps;
  Dec(ColorIndex);

  // Values are set
  BeginRGBValue[0] := GetRValue(ColorToRGB(StartColor));
  BeginRGBValue[1] := GetGValue(ColorToRGB(StartColor));
  BeginRGBValue[2] := GetBValue(ColorToRGB(StartColor));
  RGBDifference[0] := GetRValue(ColorToRGB(EndColor)) - BeginRGBValue[0];
  RGBDifference[1] := GetGValue(ColorToRGB(EndColor)) - BeginRGBValue[1];
  RGBDifference[2] := GetBValue(ColorToRGB(EndColor)) - BeginRGBValue[2];

  // Calculate the bands color
  Red := BeginRGBValue[0] + MulDiv(ColorIndex, RGBDifference[0], NumColors - 1);
  Green := BeginRGBValue[1] + MulDiv(ColorIndex, RGBDifference[1], NumColors - 1);
  Blue := BeginRGBValue[2] + MulDiv(ColorIndex, RGBDifference[2], NumColors - 1);

  // The final color is returned
  Result := RGB(Red, Green, Blue);
end;

procedure TfrmMSNPopUp.tmrExitTimer(Sender: TObject);
begin
  // after several seconds, the popup window will disappear
  // add by Ahmed Hamed 20-3-2002
  tmrExit.Enabled := False;
  tmrScrollDown.Enabled := True;
  //
end;

// add by Ahmed Hamed 20-3-2002
procedure TfrmMSNPopUp.tmrscrollDownTimer(Sender: TObject);
var
  r: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);

  case StoredBorder of
    ABE_LEFT: begin
        if (Self.Width - ScrollSpeed) > 0 then begin
          Self.Width := Self.Width - ScrollSpeed;
        end
        else
          Self.Close;
      end;

    ABE_TOP: begin
        if (Self.Height - ScrollSpeed) > 0 then begin
          Self.Height := Self.Height - ScrollSpeed;
        end
        else
          Self.Close;
      end;

    ABE_BOTTOM: begin
        if (Self.Height - ScrollSpeed) > 0 then begin
          Self.Top := Self.Top + ScrollSpeed;
          Self.Height := Self.Height - ScrollSpeed;
        end
        else
          Self.Close;
      end;

    ABE_RIGHT: begin
        if (Self.Width - ScrollSpeed) > 0 then begin
          Self.Left := Self.Left + ScrollSpeed;
          Self.Width := Self.Width - ScrollSpeed;
        end
        else
          Self.Close;
      end;
  end;
end;
//

procedure TfrmMSNPopUp.lblTextMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // close the popup window on right click
  //Jelmer
  if Button = mbRight then begin
    if Assigned(Self.ParentMSNPopup.FOnClick) then begin
      CanClose := False;
      Self.ParentMSNPopup.FOnClick(Self.ParentMSNPopup);
      CanClose := True;
    end;
    Self.Close;
  end;
end;

procedure TfrmMSNPopUp.tmrScrollTimer(Sender: TObject);
var
  r: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @r, 0);

  case StoredBorder of
    ABE_LEFT: begin
        if (Self.Width + ScrollSpeed) < sWidth then begin
          Self.Width := Self.Width + ScrollSpeed;
        end
        else begin
          Self.Width := sWidth;
          tmrScroll.Enabled := False;
          tmrExit.Enabled := True;
        end;
      end;

    ABE_TOP: begin
        if (Self.Height + ScrollSpeed) < sHeight then begin
          Self.Height := Self.Height + ScrollSpeed;
        end
        else begin
          Self.Height := sHeight;
          tmrScroll.Enabled := False;
          tmrExit.Enabled := True;
        end;
      end;

    ABE_BOTTOM: begin
        if (Self.Height + ScrollSpeed) < sHeight then begin
          Self.Top := Self.Top - ScrollSpeed;
          Self.Height := Self.Height + ScrollSpeed;
        end
        else begin
          Self.Height := sHeight;

          //Jelmer
          Self.Top := r.Bottom - ParentMSNPopup.PopupStartY - Self.Height - Self.PopupPos;

          tmrScroll.Enabled := False;
          tmrExit.Enabled := True;
        end;
      end;

    ABE_RIGHT: begin
        if (Self.Width + ScrollSpeed) < sWidth then begin
          Self.Left := Self.Left - ScrollSpeed;
          Self.Width := Self.Width + ScrollSpeed;
        end
        else begin
          Self.Width := sWidth;

          //Jelmer
          Self.Left := r.Right - ParentMSNPopup.PopupStartX - Self.Width;

          tmrScroll.Enabled := False;
          tmrExit.Enabled := True;
        end;
      end;
  end;
end;

procedure TfrmMSNPopUp.imgGradientMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // close the popup window on click
  //Jelmer
  if Assigned(Self.ParentMSNPopup.FOnClick) then begin
    CanClose := False;
    Self.ParentMSNPopup.FOnClick(Self.ParentMSNPopup);
    CanClose := True;
  end;
  Self.Close;
end;

procedure TfrmMSNPopUp.lblTitleMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  // close the popup window on click
  //Jelmer
  if Assigned(Self.ParentMSNPopup.FOnClick) then begin
    CanClose := False;
    Self.ParentMSNPopup.FOnClick(Self.ParentMSNPopup);
    CanClose := True;
  end;
  Self.Close;
end;

procedure TfrmMSNPopUp.DoClose(var Action: TCloseAction);
begin
  //Jelmer
  if CanClose = False then begin
    Action := caHide;
  end
  else begin
    if ParentMSNPopup.PopupCount > 0 then
      Dec(ParentMSNPopup.PopupCount);
    Action := caFree;
  end;
  inherited;
end;

procedure TMSNPopUp.SetBackground(Value: TBitmap);
begin
  if Value <> Self.FBackground then begin
    Self.FBackground.Assign(Value);
  end;
end;

function TfrmMSNPopUp.IsWinXP: Boolean; // MS 12/01/2002
begin
  Result := (Win32Platform = VER_PLATFORM_WIN32_NT) and (Win32MajorVersion >= 5) and (Win32MinorVersion >= 1);
end;

end.

