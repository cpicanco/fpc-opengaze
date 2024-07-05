{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit windows.methods;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Windows;

function GetRefreshRate: Integer;

implementation

function GetRefreshRate: Integer;
var
  LDevMode: DEVMODE;
begin
  ZeroMemory(@LDevMode, SizeOf(LDevMode));
  LDevMode.dmSize := SizeOf(LDevMode);

  if EnumDisplaySettings(nil, ENUM_CURRENT_SETTINGS, LDevMode) then
    Result := LDevMode.dmDisplayFrequency
  else
    Result := -1;
end;

end.

