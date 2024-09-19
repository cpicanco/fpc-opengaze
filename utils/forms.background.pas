{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit forms.background;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls;

type

  { TFormBackground }

  TFormBackground = class(TForm)
  private
    FForm  : TForm;
    FLabel : TLabel;
    FTimer : TTimer;
    procedure TimerOnTimerStart(Sender: TObject);
    procedure TimerOnTimerStop(Sender: TObject);
    procedure TimerOnTimer(Sender: TObject);
  public
    constructor CreateNew(TheOwner: TComponent; Num: integer = 0); override;
    destructor Destroy; override;
    function MoveToNextMonitor : integer;
  end;

var
  FormBackground: TFormBackground;

implementation

{$R *.lfm}

{ TFormBackground }

procedure TFormBackground.TimerOnTimerStart(Sender: TObject);
begin
  FForm.BoundsRect := Self.BoundsRect;
  if not FForm.Visible then begin
    FForm. Show;
  end;

  if not FLabel.Visible then begin
    FLabel.Show;
  end;
end;

procedure TFormBackground.TimerOnTimerStop(Sender: TObject);
begin
  if FLabel.Visible then begin
    FLabel.Hide;
  end;

  if FForm.Visible then begin
    FForm.Hide;
  end;
end;

procedure TFormBackground.TimerOnTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
end;

constructor TFormBackground.CreateNew(TheOwner: TComponent; Num: integer);
begin
  inherited CreateNew(TheOwner, Num);
  BorderStyle := bsNone;
  WindowState := wsFullScreen;
  Color := 0;
  Width := Screen.Width;
  Height := Screen.Height;
  Cursor := -1;

  FForm := TForm.CreateNew(nil);
  FForm.Width := Screen.Width;
  FForm.Height := Screen.Height;
  FForm.BorderStyle := bsNone;
  FForm.WindowState := wsFullScreen;
  FForm.Color:=clBlack;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.OnStartTimer := @TimerOnTimerStart;
  FTimer.OnStopTimer := @TimerOnTimerStop;
  FTimer.OnTimer := @TimerOnTimer;

  FLabel := TLabel.Create(Self);
  FLabel.Align := alClient;
  FLabel.Alignment := taCenter;
  FLabel.Layout := tlCenter;
  FLabel.Font.Size := Round(100 * (Width/Screen.PrimaryMonitor.Width));
  FLabel.Font.Bold := True;
  FLabel.Font.Color := clWhite;
  FLabel.Caption := '';
  FLabel.Visible := False;
  FLabel.Parent := FForm;
end;

destructor TFormBackground.Destroy;
begin
  FForm.Free;
  FLabel.Free;
  inherited Destroy;
end;

function TFormBackground.MoveToNextMonitor: integer;
var
  IsInsideMonitor: Boolean = False;
  i: Integer;
begin
  Result := 0;

  if FTimer.Enabled then Exit;

  if Screen.MonitorCount < 2 then begin
    Exit;
  end;

  for i := 0 to Screen.MonitorCount -1 do begin
    if Monitor = Screen.Monitors[i] then begin
      IsInsideMonitor := True;
      Break;
    end;
  end;

  if IsInsideMonitor then begin
    if i = Forms.Screen.MonitorCount -1 then begin
      i := 0;
    end else begin
      Inc(i);
    end;
  end else begin
    i := 0;
  end;

  BoundsRect := Forms.Screen.Monitors[i].BoundsRect;
  FLabel.Caption := Format('%d:%d', [BoundsRect.Width, BoundsRect.Height]);
  FTimer.Enabled := True;
  Result := i;
end;



end.

