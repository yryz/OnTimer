unit HouListView;

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, ComCtrls, CommCtrl;

const
  LVIS_CHECKED      = $2000;
  LVIS_UNCHECKED    = $1000;

type
  TOnChecking = procedure(Item: TListItem; Checked: Boolean;
    var Accept: Boolean) of object;

  THouListView = class(TListView)
  private
    FIgnoreCheck: Boolean;
    FOnChecking: TOnChecking;
    procedure CNNotify(var Message: TWMNotify);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy(); override;
  published
    property IgnoreCheck: Boolean read FIgnoreCheck write FIgnoreCheck;
    property OnChecking: TOnChecking read FOnChecking write FOnChecking;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HouGUI', [THouListView]);
end;

{ HouListView }

procedure THouListView.CNNotify(var Message: TWMNotify);
var
  b                 : Boolean;
begin
  with Message do begin
    b := True;
    Result := 0;
    case NMHdr^.Code of
      LVN_ITEMCHANGING:
        with PNMListView(NMHdr)^ do
          if (uOldState <> 0) then      //ÓÐ¸ü¸Ä
          begin
            if uNewState and LVIS_CHECKED <> 0 then begin
              if not FIgnoreCheck and Assigned(FOnChecking) then
                FOnChecking(Items[iItem], True, b);
            end else
              if uNewState and LVIS_UNCHECKED <> 0 then begin
                if not FIgnoreCheck and Assigned(FOnChecking) then
                  FOnChecking(Items[iItem], False, b);
              end;
          end;
    end;
    if not b then Result := 1;
  end;
end;

constructor THouListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //Self.OnColumnClick := ColumnClick;
end;

destructor THouListView.Destroy;
begin
  //Self.OnColumnClick := nil;
  inherited Destroy;
end;

procedure THouListView.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_NOTIFY: CNNotify(TWMNotify(Message));
  end;
  inherited WndProc(Message);
end;

end.

