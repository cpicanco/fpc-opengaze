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

