Unit TERRA_ShaderNode;

{$I terra.inc}
Interface
Uses TERRA_String, TERRA_Utils;

Type
  ShaderNode = Class(TERRAObject)
    Protected
      _Inputs:Array Of _ShaderNode;
      _InputCount:Integer;

      //Function RequireSamplerInput(Const Name:TERRAString)

    Public
      Procedure AddSingleInput(Input:ShaderNode);
      Procedure AddMultipleInput(Input:ShaderNode);

      Function GenerateCode():TERRAString; Virtual; Abstract;
  End;

  TextureSamplerShaderNode = Class(ShaderNode)
  End;

  NoiseShaderNode = Class(ShaderNode)
  End;

  NoiseShaderNode = Class(ShaderNode)
  End;

Implementation

End.
