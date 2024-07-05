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
  Classes, SysUtils, Forms, Controls, Graphics;

type

  { TFormBackground }

  TFormBackground = class(TForm)
  public
    function MoveToNextMonitor : integer;
  end;

var
  FormBackground: TFormBackground;

implementation

{$R *.lfm}

{ TFormBackground }

function TFormBackground.MoveToNextMonitor: integer;
var
  IsInsideMonitor: Boolean = False;
  i: Integer;
begin
  Result := 0;

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
  Result := i;
end;



end.

