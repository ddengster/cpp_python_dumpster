
#include "Player.h"

#include "Headers.h"

#define SPACING 6
#define SECONDLEVELHEIGHT 4
using namespace ddengine_RenderEngine;

VertexBuffer* player_v_buf;
IndexBuffer* player_i_buf;

Player::Player(CellLoc loc)
:m_moveto(CellLoc(0,0)), m_worldposition(Vector3(0,0,0)), moveto_worldposition(Vector3(0,0,0)), m_state(PLAYER_STATE_IDLE)
{
	SetLocation(loc);
}

Player::~Player()
{
}

void Player::SetLocation(CellLoc loc)
{
	m_location = loc;
	m_worldposition = Vector3((m_location.x-1) * SPACING, SECONDLEVELHEIGHT, (m_location.y-1) * SPACING);
}

CellLoc& Player::GetLocation()
{
	return m_location;
}

void Player::SetMoveToLocation(CellLoc loc)
{
	if (loc.x <= 0 || loc.y <= 0 || m_state == PLAYER_STATE_MOVING)
		return;
	m_moveto = loc;
	moveto_worldposition = Vector3((m_moveto.x-1) * SPACING, SECONDLEVELHEIGHT, (m_moveto.y-1) * SPACING);
	SetPlayerState(PLAYER_STATE_MOVING);
}

void Player::Update(float timeElapsed)
{
	if (m_state == PLAYER_STATE_IDLE)
		return;

	static float counter = 0;
	static float movex = 0;
	static float movez = 0;

	static const float moveperloop = 0.01;
	if (counter == 0)
	{
		movex = moveto_worldposition.x - m_worldposition.x;
		movez = moveto_worldposition.z - m_worldposition.z;
	}
	m_worldposition = m_worldposition + Vector3(	(moveperloop) * (movex),
													0,
													(moveperloop) * (movez));
	counter += moveperloop;

	if (counter > 1)
	{
		counter = 0;
		SetLocation(m_moveto);
		SetPlayerState(PLAYER_STATE_IDLE);
		m_moveto.x = 0;
		m_moveto.y = 0;
	}
}

void Player::Render()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();
	
	Matrix4 mat = Matrix4::IDENTITY;
	mat.MakeTransform(	m_worldposition,
						Vector3(1,1,1), Quaternion(Degree(0).ConvertToRadian(),
						Vector3(0,1,0)));

	renderer->SetMatrix(WORLD_MATRIX, mat);
	
	player_v_buf->SetBuffer();
	player_i_buf->SetBuffer();

	renderer->DrawIndexedPrimitive(PRIMITIVE_TRI_LIST, 0, (player_i_buf->GetNumOfIndices()/3));

	player_i_buf->UnsetBuffer();
	player_v_buf->UnsetBuffer();

	mat = Matrix4::IDENTITY;
	renderer->SetMatrix(WORLD_MATRIX, mat);

}

void Player::SetUpRenderingObjects()
{
	IRenderer* renderer = RenderEngine::GetInstance()->GetRenderer();

	HardwareBufferManager* hwbmgr = HardwareBufferManager::GetInstance();

	VertexDeclaration* player_vert_dec = hwbmgr->GenerateVertexDeclaration();
	player_vert_dec->AddVertexElement(ELE_USG_POSITION, ELE_TYP_FLOAT3, 0);
	player_vert_dec->AddVertexElement(ELE_USG_TEXCOORD, ELE_TYP_FLOAT2, 0);

	player_v_buf = hwbmgr->GenerateVertexBuffer(player_vert_dec, 24, HDW_BUF_USG_STATIC);
	Real player_data[][5] = {
		// side 1
			{ -1.0f, 1.0f, -1.0f, 0, 0 },
			{ 1.0f, 1.0f, -1.0f, 1, 0 },
			{ 1.0f, -1.0f, -1.0f, 1, 1 },
			{ -1.0f, -1.0f, -1.0f, 0, 1 },

			// side 2
			{ -1.0f, 1.0f, 1.0f, 0, 0 },
			{ -1.0f, -1.0f, 1.0f, 1, 0 },
			{ 1.0f, -1.0f, 1.0f, 1, 1 },
			{ 1.0f, 1.0f, 1.0f, 0, 1  },

			// side 1
			{ -1.0f, 1.0f, 1.0f, 0, 0 },
			{ 1.0f, 1.0f, 1.0f, 1, 0 },
			{ 1.0f, 1.0f, -1.0f, 1, 1 },
			{ -1.0f, 1.0f, -1.0f, 0, 1 },

			// side 4
			{ -1.0f, -1.0f, 1.0f, 0, 0 },
			{ -1.0f, -1.0f, -1.0f, 1, 0 },
			{ 1.0f, -1.0f, -1.0f, 1, 1 },
			{ 1.0f, -1.0f, 1.0f, 0, 1 },

			// side 5
			{ 1.0f, 1.0f, -1.0f, 0, 0 },
			{ 1.0f, 1.0f, 1.0f, 1, 0 },
			{ 1.0f, -1.0f, 1.0f, 1, 1 },
			{ 1.0f, -1.0f, -1.0f, 0, 1 },

			// side 6
			{ -1.0f, 1.0f, -1.0f, 0, 0 },
			{ -1.0f, -1.0f, -1.0f, 1, 0 },
			{ -1.0f, -1.0f, 1.0f, 1, 1 },
			{ -1.0f, 1.0f, 1.0f, 0, 1 },
	};
	player_v_buf->UpdateData(player_data, 0, 24);

	player_i_buf = hwbmgr->GenerateIndexBuffer(36, IDX_BUF_32, HDW_BUF_USG_STATIC);
	uint32_t player_indices[] =  { 0, 1, 2,
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
	player_i_buf->UpdateData(player_indices, 0, 36);
}

