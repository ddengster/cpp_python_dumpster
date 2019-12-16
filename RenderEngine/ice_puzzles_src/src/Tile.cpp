
#include "Tile.h"

#include "GameWorld.h"

#include "Headers.h"

#define SPACING 6.0

using namespace ddengine_RenderEngine;

VertexBuffer* tile_v_buf;
IndexBuffer* tile_i_buf;

Texture* rock_texture;
Texture* ice_texture;
Texture* path_texture;
Texture* cracked_texture;
Texture* pit_texture;
Texture* exit_texture;

Tile::Tile(TILETYPE tile_ID, CellLoc location)
: m_TileType_ID(tile_ID), m_location(location)
{
}

Tile::Tile()
: m_TileType_ID(TILETYPE_NULL), m_location(CellLoc(0,0))
{
}

Tile::~Tile()
{
}

void Tile::Render()
{
	//todo
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	
	tile_v_buf->SetBuffer();
	tile_i_buf->SetBuffer();

	Matrix4 mat = Matrix4::IDENTITY;
	mat.MakeTransform(	Vector3((m_location.x-1)*SPACING,0,(m_location.y-1)*SPACING),
						Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
						Vector3(0,1,0)));
	renderer->SetMatrix(WORLD_MATRIX, mat);

	Texture *touse;
	switch (m_TileType_ID)
	{
		case TILETYPE_ROCK:
			touse = rock_texture;
		break;
		case TILETYPE_ICE:
			touse = ice_texture;
		break;
		case TILETYPE_ROCKYPATH:
			touse = path_texture;
		break;
		case TILETYPE_CRACKEDICE:
			touse = cracked_texture;
		break;
		case TILETYPE_PIT:
			touse = pit_texture;
		break;
		case TILETYPE_EXIT:
			touse = exit_texture;
		break;
		default:
			touse = rock_texture;
		break;
	}
	touse->SetTexture(0);
	renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (tile_i_buf->GetNumOfIndices()/3));
	touse->UnsetTexture();

	tile_i_buf->UnsetBuffer();
	tile_v_buf->UnsetBuffer();

	mat = Matrix4::IDENTITY;
	renderer->SetMatrix(WORLD_MATRIX, mat);

}

void Tile::SetTileLocation(CellLoc loc)
{
	m_location = loc;
}

CellLoc	Tile::GetTileLocation()
{
	return m_location;
}

void Tile::SetUpRenderingObjects()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* tile_vert_dec = hwbmgr->GenerateVertexDeclaration();
	tile_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	tile_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	tile_v_buf = hwbmgr->GenerateVertexBuffer(tile_vert_dec, 24, HDW_BUF_USG_STATIC);
	Real tile_data[][5] = {
		// side 1
			{ -3.0f, 3.0f, -3.0f, 0, 0 },
			{ 3.0f, 3.0f, -3.0f, 1, 0 },
			{ 3.0f, -3.0f, -3.0f, 1, 1 },
			{ -3.0f, -3.0f, -3.0f, 0, 1 },

			// side 2
			{ -3.0f, 3.0f, 3.0f, 0, 0 },
			{ -3.0f, -3.0f, 3.0f, 1, 0 },
			{ 3.0f, -3.0f, 3.0f, 1, 1 },
			{ 3.0f, 3.0f, 3.0f, 0, 1  },

			// side 3
			{ -3.0f, 3.0f, 3.0f, 0, 0 },
			{ 3.0f, 3.0f, 3.0f, 1, 0 },
			{ 3.0f, 3.0f, -3.0f, 1, 1 },
			{ -3.0f, 3.0f, -3.0f, 0, 1 },

			// side 4
			{ -3.0f, -3.0f, 3.0f, 0, 0 },
			{ -3.0f, -3.0f, -3.0f, 1, 0 },
			{ 3.0f, -3.0f, -3.0f, 1, 1 },
			{ 3.0f, -3.0f, 3.0f, 0, 1 },

			// side 5
			{ 3.0f, 3.0f, -3.0f, 0, 0 },
			{ 3.0f, 3.0f, 3.0f, 1, 0 },
			{ 3.0f, -3.0f, 3.0f, 1, 1 },
			{ 3.0f, -3.0f, -3.0f, 0, 1 },

			// side 6
			{ -3.0f, 3.0f, -3.0f, 0, 0 },
			{ -3.0f, -3.0f, -3.0f, 1, 0 },
			{ -3.0f, -3.0f, 3.0f, 1, 1 },
			{ -3.0f, 3.0f, 3.0f, 0, 1 },
	};
	tile_v_buf->UpdateData(tile_data, 0, 24);

	tile_i_buf = hwbmgr->GenerateIndexBuffer(36, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t tile_indices[] =  { 0, 1, 2,
								 0, 2, 3,

								 4, 5, 6,
								 4, 6, 7,

								 8, 9, 10,
								 8, 10, 11,

								 12, 13, 14,
								 12, 14, 15,

								 16, 17, 18,
								 16, 18, 19,

								 20, 21, 22,
								 20, 22, 23
	};
	tile_i_buf->UpdateData(tile_indices, 0, 36);

	TextureManager* texturemgr = TextureManager::GetInstance();

	rock_texture = texturemgr->GetTexture2D("Rock.jpg","./data/textures/");
	ice_texture = texturemgr->GetTexture2D("ice.jpg","./data/textures/");
	path_texture = texturemgr->GetTexture2D("RockyPath.jpg","./data/textures/");
	cracked_texture = texturemgr->GetTexture2D("crackedice.jpg","./data/textures/");
	pit_texture = texturemgr->GetTexture2D("pit.jpg","./data/textures/");
	exit_texture = texturemgr->GetTexture2D("exit.jpg","./data/textures/");
	
}

