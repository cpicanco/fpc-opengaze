{
  fpc-opengaze
  Copyright (C) 2024 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit screen.helpers;

{$mode ObjFPC}{$H+}

{$modeswitch typehelpers}

interface

uses
  Classes, SysUtils, Math;

type
  { TFloatScreenHelper }

  TFloatScreenHelper = type helper (TDoubleHelper) for Float
    function DenormalizeI(AMax: Integer; AMin: Integer = 0) : integer; overload;
    function Denormalize(AMax: Integer; AMin: Integer = 0) : integer; overload;
  end;

  { TIntegerScreenHelper }

  TIntegerScreenHelper = type helper (TIntegerHelper) for Integer
    function NormalizeI(AMax: Integer; AMin: Integer = 0) : Float; overload;
    function Normalize(AMax: Integer; AMin: Integer = 0) : Float; overload;
  end;

implementation

{ TFloatScreenHelper }

function TFloatScreenHelper.DenormalizeI(AMax: Integer; AMin: Integer): integer;
begin
  Result := Round((AMax - AMin) * (1.0 - Self)) + AMin;
end;

function TFloatScreenHelper.Denormalize(AMax: Integer; AMin: Integer): integer;
begin
  Result := Round((AMax - AMin) * Self) + AMin;
end;

{ TIntegerScreenHelper }

function TIntegerScreenHelper.NormalizeI(AMax: Integer; AMin: Integer): Float;
begin
  Result := (AMax - Self) / (AMax - AMin);
end;

function TIntegerScreenHelper.Normalize(AMax: Integer; AMin: Integer): Float;
begin
  Result := (Self - AMin) / (AMax - AMin);
end;

end.

