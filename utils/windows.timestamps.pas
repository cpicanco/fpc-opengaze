{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit windows.timestamps;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Windows;

function ClockMonotonic : TLargeInteger;

var
  PerSecond : TLargeInteger;

implementation

function ClockMonotonic: TLargeInteger;
begin
  QueryPerformanceCounter(Result);
end;

initialization
   QueryPerformanceFrequency(PerSecond);

end.

