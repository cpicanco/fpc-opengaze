{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit screen.lines;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TBresenhamLine = array of TPoint;

function BresenhamLine(x0, y0, x1, y1: Integer) : TBresenhamLine;

implementation

function BresenhamLine(x0, y0, x1, y1: Integer) : TBresenhamLine;
var
  dx, dy, sx, sy, err, e2: Integer;
begin
  dx := abs(x1 - x0);
  dy := abs(y1 - y0);
  if x0 < x1 then sx := 1 else sx := -1;
  if y0 < y1 then sy := 1 else sy := -1;
  err := dx - dy;

  SetLength(Result, 0);

  while True do begin
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].X := x0;
    Result[High(Result)].Y := y0;
    if (x0 = x1) and (y0 = y1) then Break;
    e2 := 2 * err;
    if e2 > -dy then begin
      err := err - dy;
      x0 := x0 + sx;
    end;
    if e2 < dx then begin
      err := err + dx;
      y0 := y0 + sy;
    end;
  end;
end;

end.

