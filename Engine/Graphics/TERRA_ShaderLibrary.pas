Unit TERRA_ShaderLibrary;

{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_ShaderNode;

Type
	//Remaps A from to range [B..C]
	ShaderRangeNode = Class(ShaderTernaryNode)
		Public
	End;

	// blend modes (binary nodes)
	// https://mouaif.wordpress.com/2009/01/05/photoshop-math-with-glsl-shaders/
	ShaderBlendNode = Class(ShaderBinaryNode)

	End;

	ShaderBlendLighten = Class(ShaderBlendNode)
	End;

	ShaderBlendDarken = Class(ShaderBlendNode)
	End;

	ShaderBlendAverage = Class(ShaderBlendNode)
	End;

	ShaderBlendDifference = Class(ShaderBlendNode)
	End;

	ShaderBlendNegation = Class(ShaderBlendNode)
	End;

	ShaderBlendExclusion = Class(ShaderBlendNode)
	End;

	ShaderBlendScreen = Class(ShaderBlendNode)
	End;

	ShaderBlendOverlay = Class(ShaderBlendNode)
	End;

	ShaderBlendSoftLight = Class(ShaderBlendNode)
	End;

	ShaderBlendHardLight = Class(ShaderBlendNode)
	End;

	ShaderBlendColorDodge = Class(ShaderBlendNode)
	End;

	ShaderBlendColorBurn = Class(ShaderBlendNode)
	End;

	ShaderBlendLinearDodge = Class(ShaderBlendNode)
	End;

	ShaderBlendLinearBurn = Class(ShaderBlendNode)
	End;

	ShaderBlendLinearLight = Class(ShaderBlendNode)
	End;

	ShaderBlendVividLight = Class(ShaderBlendNode)
	End;

	ShaderBlendPinLight = Class(ShaderBlendNode)
	End;

	ShaderBlendHardMix = Class(ShaderBlendNode)
	End;

	ShaderBlendReflect = Class(ShaderBlendNode)
	End;

	ShaderBlendGlow = Class(ShaderBlendNode)
	End;

	ShaderBlendPhoenix = Class(ShaderBlendNode)
	End;

	ShaderBlendHue = Class(ShaderBlendNode)
	End;

	ShaderBlendSaturation = Class(ShaderBlendNode)
	End;

	ShaderBlendColor = Class(ShaderBlendNode)
	End;
	

	ShaderBlendLuminosity = Class(ShaderBlendNode)
	End;

	// Outputs 1 minus its input. When used with color inputs, it will invert the color
	ShaderInvertNode = Class(ShaderUnaryNode)
		Public
	End;

	// Outputs the main input multiplied by -1.
	ShaderNegateNode = Class(ShaderUnaryNode)
		Public
	End;


	ShaderPosterizeNode = Class(ShaderBinaryNode)
		Public
	End;

	ShaderClampNode = Class(ShaderTernaryNode)
		Public
	End;
  
	ShaderInputScreenPosition = Class(ShaderNode)
		Public
			Function GetType():ShaderNodeType; Override;
	End;

	ShaderInputScreenSize = Class(ShaderNode)
		Public
			Function GetType():ShaderNodeType; Override;
	End;


	ShaderNoise2D = Class(ShaderUnaryNode)
		Public
	End;

	ShaderNoise3D = Class(ShaderUnaryNode)
		Public
	End;

	// applies a skinning transformation to a position
	ShaderPositionBoneSkinning = Class(ShaderComplexUnaryNode)
		Public
			Function GetType():ShaderNodeType; Override;
	End;

	ShaderNormalBoneSkinning = Class(ShaderComplexUnaryNode)
		Public
			Function GetType():ShaderNodeType; Override;
  End;

	// vertex offset

	// complex nodes
  ShaderComplexUnaryNode = Class(ShaderUnaryNode)
    Protected
      _Graph:ShaderGroup;

    Public
      Function GetSubGraph():ShaderGroup; Override;
  End;



Implementation

{ ShaderComplexUnaryNode }
Function ShaderComplexUnaryNode.GetSubGraph: ShaderGroup;
Begin
  Result := _Graph;
End;

End;
