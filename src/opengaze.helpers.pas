{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit opengaze.helpers;

{$mode ObjFPC}{$H+}

{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, math, opengaze.constants;

type

  { TFloaTOpenGazeHelper }

  TFloaTOpenGazeHelper = type helper (TDoubleHelper) for Float
    function ToValueArray : TStringArray;
    function ToXArray : TStringArray;
    function ToYArray : TStringArray;
  end;

  { TBooleanOpenGazeHelper }

  TBooleanOpenGazeHelper = type helper (TBooleanHelper) for Boolean
    function ToStateArray : TStringArray;
    function ToValueString : string;
  end;

  { TIntegerOpenGazeHelper }

  TIntegerOpenGazeHelper = type helper (TIntegerHelper) for Integer
    function ToPointsArray : TStringArray;
    function ToValueArray  : TStringArray;
  end;


implementation

var
  FormatSettings : TFormatSettings;

{ TFloaTOpenGazeHelper }

function TFloaTOpenGazeHelper.ToValueArray: TStringArray;
begin
  Result := [VALUE, Self.ToString(FormatSettings)];
end;

function TFloaTOpenGazeHelper.ToXArray: TStringArray;
begin
  Result := [XCOORDENATE, Self.ToString(FormatSettings)];
end;

function TFloaTOpenGazeHelper.ToYArray: TStringArray;
begin
  Result := [YCOORDENATE, Self.ToString(FormatSettings)];
end;

{ TBooleanOpenGazeHelper }

function TBooleanOpenGazeHelper.ToStateArray: TStringArray;
begin
  Result := [STATE, BoolToStr(Self, _TRUE_, _FALSE_)]
end;

function TBooleanOpenGazeHelper.ToValueString: string;
begin
  Result := BoolToStr(Self, _TRUE_, _FALSE_);
end;

{ TIntegerOpenGazeHelper }

function TIntegerOpenGazeHelper.ToPointsArray: TStringArray;
begin
  Result := [PTS, Self.ToString]
end;

function TIntegerOpenGazeHelper.ToValueArray: TStringArray;
begin
  Result := [VALUE, Self.ToString]
end;

initialization
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DecimalSeparator := '.';

end.

