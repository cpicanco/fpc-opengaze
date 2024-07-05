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
  Classes, SysUtils, Math, opengaze.constants;

type

  { TFloatOpenGazeHelper }

  TFloatOpenGazeHelper = type helper (TDoubleHelper) for Float
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

  { TStringOpenGazeHelper }

  TStringOpenGazeHelper = type helper (TStringHelper) for string
    function ToFloat : Float;
    function ToTimeInteger : Int64;
  end;


implementation

var
  FormatSettings : TFormatSettings;

{ TFloatOpenGazeHelper }

function TFloatOpenGazeHelper.ToValueArray: TStringArray;
begin
  Result := [VALUE, Self.ToString(FormatSettings)];
end;

function TFloatOpenGazeHelper.ToXArray: TStringArray;
begin
  Result := [XCOORDENATE, Self.ToString(FormatSettings)];
end;

function TFloatOpenGazeHelper.ToYArray: TStringArray;
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

{ TStringOpenGazeHelper }

function TStringOpenGazeHelper.ToFloat: Float;
begin
  Result := StrToFloat(Self, FormatSettings);
end;

function TStringOpenGazeHelper.ToTimeInteger: Int64;
begin
  Result := Round(Self.ToFloat*1e+7);
end;

initialization
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DecimalSeparator := '.';

end.

