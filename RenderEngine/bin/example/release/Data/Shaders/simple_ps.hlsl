///// pixel shader
float3     attenuation;
sampler2D   normal_map;
sampler2D   color_map;

float4x4   wld_IT_mtx;             // Variable Semantics | WorldInverseTranspose

float   pointAtten(float d, float3 a)
{
   return   1/(a.x + a.y*d + a.z*d*d);
}

void ps_main(   float2 uv : TEXCOORD0,
                float3 L  : TEXCOORD1,
                float3 V  : TEXCOORD2,
                float3 N_g  : TEXCOORD3,
                float  d  : TEXCOORD4,
                float3 T  : TEXCOORD5,

                
                out float4 oColor : COLOR)
{   
   L = normalize(L);
   V = normalize(V);
   N_g = normalize(N_g);
   T = normalize(T);
   
   float3   B = cross(N_g, T);
   float3x3   M = { T, B, N_g};
  
   
   float3   N_t = tex2D(normal_map, uv)*2 - 1;
 
   float3   N_f = mul( N_t, M );
   N_f = normalize( mul( wld_IT_mtx, N_f ) );
   
   float3    H = normalize( (L + V)*0.5 );

   float    atten = pointAtten(d, attenuation);

   float4   base_color = tex2D(color_map, uv);

   oColor = ( (0.3 + max(dot(N_f,L), 0)) * base_color
             + pow(max(dot(N_f,H), 0), 32)  )*atten;
}
