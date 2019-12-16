///// vertex shader
float4x4   wld_view_proj_mtx;      // Variable Semantics | WorldViewProjection
float4x4   wld_IT_mtx;             // Variable Semantics | WorldInverseTranspose
float4x4   wld_mtx;                // Variable Semantics | World
float4     eye_position;           // Variable Semantics | ViewPosition
float4     light_position;

void vs_main(  float4 position : POSITION,
               float3 normal   : NORMAL,
               float2 uv       : TEXCOORD0,
               float3 tangent  : TEXCOORD1,
               
               out float4 oPosition : POSITION,   // position in projection(screen) space, used by hardware to locate corresponding pixel position
               out float2 oUv  : TEXCOORD0,       // pass uv to pixel shader
               out float3 oL    : TEXCOORD1,
               out float3 oV    : TEXCOORD2,
               out float3 oNormal : TEXCOORD3,
               out float  oD : TEXCOORD4,
               out float3 oTangent : TEXCOORD5
             )
{
   oPosition = mul( wld_view_proj_mtx, position);

   oUv = uv;
   
   oNormal = normalize( mul( (float3x3)wld_IT_mtx , normal) );
   
   float4   wld_pos = mul( wld_mtx, position );
   oL = light_position.xyz - wld_pos.xyz;
   oD = length(oL);
   oL = normalize(oL);
   
   oV = normalize( eye_position.xyz - wld_pos.xyz );
   
   oTangent = normalize( mul((float3x3)wld_IT_mtx, tangent)  );
}