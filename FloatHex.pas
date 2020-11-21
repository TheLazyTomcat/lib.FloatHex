{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  Floating point numbers <-> HexString conversion routines

  ©František Milt 2018-10-21

  Version 1.5.5

  Dependencies:
    AuxTypes - github.com/ncs-sniper/Lib.AuxTypes

===============================================================================}
unit FloatHex;

{$IFDEF ENDIAN_BIG}
  {$MESSAGE FATAL 'Big-endian system not supported'}
{$ENDIF}

{$IFDEF FPC}
  {$MODE ObjFPC}
  {$INLINE ON}
  {$DEFINE CanInline}
{$ELSE}
  {$IF CompilerVersion >= 17 then}  // Delphi 2005+
    {$DEFINE CanInline}
  {$ELSE}
    {$UNDEF CanInline}
  {$IFEND}
{$ENDIF}
{$H+}

interface

uses
  SysUtils,
  AuxTypes, Float80Utils;

// cannot be placed any higher because of FPC
{$IF SizeOf(Extended) = 8}
  {$DEFINE Extended64}
{$ELSEIF SizeOf(Extended) = 10}
  {$UNDEF Extended64}
{$ELSE}
  {$MESSAGE FATAL 'Unsupported platform, type extended must be 8 or 10 bytes.'}
{$IFEND}

type
  // library-specific exceptions, unused atm.
  EFHException = class(Exception);

{===============================================================================
--------------------------------------------------------------------------------
                        Float <-> HexString conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float <-> HexString conversions - declaration
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
Function HexToFloat16(const HexString: String): Float16;
Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToHalf(const HexString: String): Half;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
Function HexToFloat32(const HexString: String): Float32;
Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToSingle(const HexString: String): Single;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
Function HexToFloat64(const HexString: String): Float64;
Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToDouble(const HexString: String): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float80
-------------------------------------------------------------------------------}

Function Float80ToHex(Value: Float80): String;
Function HexToFloat80(const HexString: String): Float80;
Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToExtended(const HexString: String): Extended;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;{$IFDEF CanInline} inline;{$ENDIF}

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToFloat(const HexString: String): Double;{$IFDEF CanInline} inline;{$ENDIF}
Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;{$IFDEF CanInline} inline;{$ENDIF}
Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;{$IFDEF CanInline} inline;{$ENDIF}

implementation

{===============================================================================
--------------------------------------------------------------------------------
                        Float <-> HexString conversions
--------------------------------------------------------------------------------
===============================================================================}
{===============================================================================
    Float <-> HexString conversions- auxiliary routines
===============================================================================}

Function RectifyHexString(const Str: String; RequiredLength: Integer): String;

  Function StartsWithHexMark: Boolean;
  begin
    If Length(Str) > 0 then
      Result := Str[1] = '$'
    else
      Result := False;
  end;

begin
If not StartsWithHexMark then
  Result := '$' + Str
else
  Result := Str;
Inc(RequiredLength);
If Length(Result) <> RequiredLength then
  begin
    If Length(Result) < RequiredLength then
      Result := Result + StringOfChar('0',RequiredLength - Length(Result))
    else
      Result := Copy(Result,1,RequiredLength);
  end;
end;

{===============================================================================
    Float <-> HexString conversions - implementation
===============================================================================}
{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float16
-------------------------------------------------------------------------------}

Function Float16ToHex(Value: Float16): String;
var
  Overlay:  UInt16 absolute Value;
begin
Result := IntToHex(Overlay,4);
end;

//------------------------------------------------------------------------------

Function HexToFloat16(const HexString: String): Float16;
var
  Overlay:  UInt16 absolute Result;
begin
Overlay := UInt16(StrToInt(RectifyHexString(HexString,4)));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat16(const HexString: String; out Value: Float16): Boolean;
begin
try
  Value := HexToFloat16(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat16Def(const HexString: String; const DefaultValue: Float16): Float16;
begin
If not TryHexToFloat16(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Half
-------------------------------------------------------------------------------}

Function HalfToHex(Value: Half): String;
begin
Result := Float16ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToHalf(const HexString: String): Half;
begin
Result := HexToFloat16(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToHalf(const HexString: String; out Value: Half): Boolean;
begin
Result := TryHexToFloat16(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToHalfDef(const HexString: String; const DefaultValue: Half): Half;
begin
Result := HexToFloat16Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float32
-------------------------------------------------------------------------------}

Function Float32ToHex(Value: Float32): String;
var
  Overlay:  UInt32 absolute Value;
begin
Result := IntToHex(Overlay,8);
end;
 
//------------------------------------------------------------------------------

Function HexToFloat32(const HexString: String): Float32;
var
  Overlay:  UInt32 absolute Result;
begin
Overlay := UInt32(StrToInt(RectifyHexString(HexString,8)));
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat32(const HexString: String; out Value: Float32): Boolean;
begin
try
  Value := HexToFloat32(HexString);
  Result := True;
except
  Result := False;
end;
end;

//------------------------------------------------------------------------------

Function HexToFloat32Def(const HexString: String; const DefaultValue: Float32): Float32;
begin
If not TryHexToFloat32(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Single
-------------------------------------------------------------------------------}

Function SingleToHex(Value: Single): String;
begin
Result := Float32ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToSingle(const HexString: String): Single;
begin
Result := HexToFloat32(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToSingle(const HexString: String; out Value: Single): Boolean;
begin
Result := TryHexToFloat32(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToSingleDef(const HexString: String; const DefaultValue: Single): Single;
begin
Result := HexToFloat32Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float64
-------------------------------------------------------------------------------}

Function Float64ToHex(Value: Float64): String;
var
  Overlay:  UInt64 absolute Value;
begin
Result := IntToHex(Overlay,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat64(const HexString: String): Float64;
var
  Overlay:  Int64 absolute Result;
begin
Overlay := StrToInt64(RectifyHexString(HexString,16));
end;

//------------------------------------------------------------------------------

Function TryHexToFloat64(const HexString: String; out Value: Float64): Boolean;
begin
try
  Value := HexToFloat64(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat64Def(const HexString: String; const DefaultValue: Float64): Float64;
begin
If not TryHexToFloat64(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Double
-------------------------------------------------------------------------------}

Function DoubleToHex(Value: Double): String;
begin
Result := Float64ToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToDouble(const HexString: String): Double;
begin
Result := HexToFloat64(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToDouble(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToFloat64(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToDoubleDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToFloat64Def(HexString,DefaultValue);
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Float80
-------------------------------------------------------------------------------}

Function Float80ToHex(Value: Float80): String;
var
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Value{$ENDIF};
begin
{$IFDEF Extended64}
ConvertFloat64ToFloat80(@Value,@Overlay);
{$ENDIF}
Result := IntToHex(Overlay.Part_16,4) + IntToHex(Overlay.Part_64,16);
end;

//------------------------------------------------------------------------------

Function HexToFloat80(const HexString: String): Float80;
var
  Temp:     String;
  Overlay:  TFloat80Overlay {$IFNDEF Extended64}absolute Result{$ENDIF};
begin
Temp := RectifyHexString(HexString,20);
Overlay.Part_16 := UInt16(StrToInt(Copy(Temp,1,5)));
Overlay.Part_64 := UInt64(StrToInt64('$' + Copy(Temp,6,16)));
{$IFDEF Extended64}
ConvertFloat80ToFloat64(@Overlay,@Result);
{$ENDIF}
end;
 
//------------------------------------------------------------------------------

Function TryHexToFloat80(const HexString: String; out Value: Float80): Boolean;
begin
try
  Value := HexToFloat80(HexString);
  Result := True;
except
  Result := False;
end;
end;
 
//------------------------------------------------------------------------------

Function HexToFloat80Def(const HexString: String; const DefaultValue: Float80): Float80;
begin
If not TryHexToFloat80(HexString,Result) then
  Result := DefaultValue;
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - type Extended
-------------------------------------------------------------------------------}

Function ExtendedToHex(Value: Extended): String;
begin
{$IFDEF Extended64}
Result := Float64ToHex(Value);
{$ELSE}
Result := Float80ToHex(Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function HexToExtended(const HexString: String): Extended;
begin
{$IFDEF Extended64}
Result := HexToFloat64(HexString);
{$ELSE}
Result := HexToFloat80(HexString);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function TryHexToExtended(const HexString: String; out Value: Extended): Boolean;
begin
{$IFDEF Extended64}
Result := TryHexToFloat64(HexString,Value);
{$ELSE}
Result := TryHexToFloat80(HexString,Value);
{$ENDIF}
end;

//------------------------------------------------------------------------------

Function HexToExtendedDef(const HexString: String; const DefaultValue: Extended): Extended;
begin
{$IFDEF Extended64}
Result := HexToFloat64Def(HexString,DefaultValue);
{$ELSE}
Result := HexToFloat80Def(HexString,DefaultValue);
{$ENDIF}
end;

{-------------------------------------------------------------------------------
    Float <-> HexString conversions - default float type
-------------------------------------------------------------------------------}

Function FloatToHex(Value: Double): String;
begin
Result := DoubleToHex(Value);
end;

//------------------------------------------------------------------------------

Function HexToFloat(const HexString: String): Double;
begin
Result := HexToDouble(HexString);
end;

//------------------------------------------------------------------------------

Function TryHexToFloat(const HexString: String; out Value: Double): Boolean;
begin
Result := TryHexToDouble(HexString,Value);
end;

//------------------------------------------------------------------------------

Function HexToFloatDef(const HexString: String; const DefaultValue: Double): Double;
begin
Result := HexToDoubleDef(HexString,DefaultValue);
end;

end.
