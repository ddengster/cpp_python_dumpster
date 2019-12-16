
#include "GameWorld.h"
#include "Tile.h"
#include "Player.h"
#include "GameCamera.h"
#include "GameStateManager.h"
#include "GameState_GamePlay.h"

#include <vector>
#include <fstream>
//#include <cctype>

using namespace std;

//file parsing tokens
const string comment_Token("//");
const string sizeX_Token("X - ");
const string sizeY_Token("Y - ");
const string ROCK_Token("ROCK");
const string ICE_Token("ICET");
const string ROCKYPATH_Token("PATH");
const string CRACKEDICE_Token("CICE");
const string PIT_Token("PITT");
const string EXIT_Token("EXIT");
const string NEXTDATA_Token("/");
const string PLAYERLOCATION_Token("PLAYERLOC - ");

#define DELIMITER_SIZE 1
#define TILETYPE_TOKEN_SIZE 4

struct LevelData
{
	TILETYPE m_type;
	unsigned int m_numberof;
	LevelData(TILETYPE type, unsigned int numberof)
		:m_type(type), m_numberof(numberof)
	{
	}
	LevelData()
		:m_type(TILETYPE_NULL), m_numberof(0)
	{
	}
};

GameWorld::GameWorld()
{
	//InitializeTileTextures
	Tile::SetUpRenderingObjects();
	Player::SetUpRenderingObjects();
	m_player = NULL;
	m_tiles = NULL;
	m_camera = NULL;
	m_camera = new GameCamera(Vector3(10, 20, 45), Vector3(0, -3, -6), Vector3(0, 1, 0));
	m_camera->FinalizeCamera();
}

GameWorld::~GameWorld()
{
	for (int i=0; i<m_breadth; ++i)
	{
		for (int j=0; j<m_length; ++j)
		{
			SAFE_DELETE(m_tiles[i][j]);
		}
		SAFE_DELETE(m_tiles[i]);
	}
	if (m_tiles)
		delete m_tiles;
	m_tiles = NULL;

	if (m_player)
		delete m_player;
	m_player = NULL;

	SAFE_DELETE(m_camera);
}

bool GameWorld::InitializeFromFile(string filename, string *errormsg)
{
	//todo
	ifstream file;

	file.open(filename.c_str());
	if (!file)
	{
		*errormsg = "Cannot open file: " + filename;
		return false;
	}
	
	string line;

	//game info
	unsigned int length = 0, breadth = 0;

	vector<LevelData> mapdata;

	LevelData parseddata;
	string remainder;
	string datablock;
	size_t pos;
	string tiletype;
	string num;

	while (!file.eof())
	{
		getline(file, line);
		if (line.find(comment_Token) != string::npos || line == "")
			continue;
		else if (line.find(sizeX_Token) != string::npos)
		{
			//xsize
			string val = line.substr(sizeX_Token.length());
			if (!val.length())
			{
				*errormsg = "No X size specified.";
				return false;
			}
			//check
			for (int i=0; i<val.length(); ++i)
			{
				if (!isdigit(val.c_str()[i]))
				{
					*errormsg = "X size is not a number!";
					return false;
				}
			}
			length = atoi(val.c_str());
		}
		else if (line.find(sizeY_Token) != string::npos)
		{
			//ysize
			string val = line.substr(sizeY_Token.length());

			if (!val.length())
			{
				*errormsg = "No Y size specified.";
				return false;
			}
			//check
			for (int i=0; i<val.length(); ++i)
			{
				if (!isdigit(val.c_str()[i]))
				{
					*errormsg = "Y size is not a number!";
					return false;
				}
			}
			breadth = atoi(val.c_str());
		}
		else if (line.find(PLAYERLOCATION_Token) != string::npos)
		{
			int xpos = 0;
			int ypos = 0;

			string val = line.substr(PLAYERLOCATION_Token.length());
			string xposition = val.substr(0, val.find(NEXTDATA_Token));
			//check
			if (!xposition.length())
			{
				*errormsg = "Player X location not specified!";
				return false;
			}
			for (int i=0; i<xposition.length(); ++i)
			{
				if (!isdigit(xposition.c_str()[i]))
				{
					*errormsg = "Player X location is not a number!";
					return false;
				}
			}
			xpos = atoi(xposition.c_str());

			string yposition = val.substr(val.find(NEXTDATA_Token)+DELIMITER_SIZE);
			if (!yposition.length())
			{
				*errormsg = "Player Y location not specified!";
				return false;
			}
			//check
			for (int i=0; i<yposition.length(); ++i)
			{
				if (!isdigit(yposition.c_str()[i]))
				{
					*errormsg = "Player Y location is not a number!";
					return false;
				}
			}
			ypos = atoi(yposition.c_str());

			m_player = new Player(CellLoc(xpos, ypos));
		}
		else if (length && breadth) // once length and breath are gotten
		{
			remainder = line;
			//enter map processing block
			while(remainder.length())
			{
				pos = remainder.find(NEXTDATA_Token);

				//seperate the data block
				datablock = remainder.substr(0, pos);

				tiletype = datablock.substr(0, TILETYPE_TOKEN_SIZE);
				if (!tiletype.compare(ROCK_Token))
					parseddata.m_type = TILETYPE_ROCK;
				else if (!tiletype.compare(ICE_Token)) 
					parseddata.m_type = TILETYPE_ICE;
				else if (!tiletype.compare(ROCKYPATH_Token)) 
					parseddata.m_type = TILETYPE_ROCKYPATH;
				else if (!tiletype.compare(CRACKEDICE_Token)) 
					parseddata.m_type = TILETYPE_CRACKEDICE;
				else if (!tiletype.compare(PIT_Token))
					parseddata.m_type = TILETYPE_PIT;
				else if (!tiletype.compare(EXIT_Token))
					parseddata.m_type = TILETYPE_EXIT;

				num = datablock.substr(TILETYPE_TOKEN_SIZE);
				for (int i=0; i<num.length(); ++i)
				{
					if (!isdigit(num.c_str()[i]))
					{
						*errormsg = "Number of tiles not specified.";
						return false;
					}
				}
				parseddata.m_numberof = atoi(num.c_str());

				if (pos != string::npos)
					remainder = remainder.substr(pos+DELIMITER_SIZE);
				else
					remainder = "";

				mapdata.push_back(parseddata);
			}
		}
	}

	//initialize tiles
	SetSize(length, breadth);
	DoCameraAdjustments();
	m_tiles = new Tile**[breadth];
	for (int j=0; j<breadth; ++j)
	{
		m_tiles[j] = new Tile*[length];
		for(int i=0; i<length; ++i)
		{
			m_tiles[j][i] = new Tile();
		}
	}

	int tilecount = 0;
	for (int i=0; i<mapdata.size(); ++i)
	{
		for (int p=0; p<mapdata[i].m_numberof; ++p)
		{
			int xcoord = (tilecount+p)%length;
			int ycoord = (tilecount+p)/length;
			m_tiles[ycoord][xcoord]->SetTileType(mapdata[i].m_type);
			m_tiles[ycoord][xcoord]->SetTileLocation(CellLoc(xcoord+1, ycoord+1));
		}
		tilecount += mapdata[i].m_numberof;
		if (tilecount > (length*breadth)) // safety check
		{
			*errormsg = "More tiles written than the specified length times breadth!";
			return false;
		}
	}
	return true;
}

bool GameWorld::SetSize(unsigned int length, unsigned int breadth)
{
	if (length != 0 && breadth != 0)
	{
		m_length = length;
		m_breadth = breadth;

		//m_tiles = new Tile[breadth][length];
		return true;
	}
	else
		return false;
}

bool GameWorld::SetPlayerStartLocation(CellLoc location)
{
	return true;
}

bool GameWorld::AddTreasureChest(CellLoc location)
{
	return true;
}

bool GameWorld::SetWinCondition(unsigned int treasureChestCount)
{
	return true;
}

void GameWorld::Update(float timeElapsed)
{
	m_player->Update(timeElapsed);
	Render();
}

void GameWorld::Render()
{
	for (int i=0; i<m_breadth; ++i)
	{
		for (int j=0; j<m_length; ++j)
		{
			m_tiles[i][j]->Render();
		}
	}

	m_player->Render();
}

bool GameWorld::Clear()
{
	return true;
}

void GameWorld::SetPlayerMoveDirection(PLAYER_DIR dir)
{
	CellLoc player_location = m_player->GetLocation();

	//remember arrays start from 0
	if (dir == PLAYER_UP && player_location.y > 1)
	{
		// player steps onto an ice tile
		if (m_tiles[player_location.y-2][player_location.x-1]->GetTileType() == TILETYPE_ICE) 
		{
			int j = 0;
			//check each tile in that direction until it reaches a non-ice tile
			for (int i=player_location.y-2; i>=0; --i,++j) 
			{
				if (m_tiles[i][player_location.x-1]->GetTileType() != TILETYPE_ICE)
				{
					if(m_tiles[i][player_location.x-1]->GetTileType() == TILETYPE_ROCK)
						m_player->SetMoveToLocation(player_location - CellLoc(0, j));
					else
						m_player->SetMoveToLocation(player_location - CellLoc(0, ++j));
					break;
				}
			}
		}
		// do nothing if rock is in the way
		else if (m_tiles[player_location.y-2][player_location.x-1]->GetTileType() != TILETYPE_ROCK) 
			m_player->SetMoveToLocation(player_location - CellLoc(0, 1));
	}
	else if (dir == PLAYER_DOWN && player_location.y < m_breadth)
	{
		if (m_tiles[player_location.y][player_location.x-1]->GetTileType() == TILETYPE_ICE)
		{
			int j = 0;
			for (int i=player_location.y; i<m_breadth; ++i,++j)
			{
				if (m_tiles[i][player_location.x-1]->GetTileType() != TILETYPE_ICE)
				{
					if (m_tiles[i][player_location.x-1]->GetTileType() == TILETYPE_ROCK)
						m_player->SetMoveToLocation(player_location + CellLoc(0, j));
					else
						m_player->SetMoveToLocation(player_location + CellLoc(0, ++j));
					break;
				}
			}
		}
		else if (m_tiles[player_location.y][player_location.x-1]->GetTileType() != TILETYPE_ROCK)
			m_player->SetMoveToLocation(player_location + CellLoc(0, 1));
	}
	else if (dir == PLAYER_LEFT && player_location.x > 1)
	{
		if (m_tiles[player_location.y-1][player_location.x-2]->GetTileType() == TILETYPE_ICE)
		{
			int j = 0;
			for (int i=player_location.x-2; i>=0; --i,++j)
			{
				if (m_tiles[player_location.y-1][i]->GetTileType() != TILETYPE_ICE)
				{
					if (m_tiles[player_location.y-1][i]->GetTileType() == TILETYPE_ROCK)
						m_player->SetMoveToLocation(player_location - CellLoc(j, 0));
					else
						m_player->SetMoveToLocation(player_location - CellLoc(++j, 0));
					break;
				}
			}
		}
		else if (m_tiles[player_location.y-1][player_location.x-2]->GetTileType() != TILETYPE_ROCK)
			m_player->SetMoveToLocation(player_location - CellLoc(1, 0));
	}
	else if (dir == PLAYER_RIGHT && player_location.x < m_length)
	{
		if (m_tiles[player_location.y-1][player_location.x]->GetTileType() == TILETYPE_ICE)
		{
			int j = 0;
			for (int i=player_location.x; i<m_length; ++i,++j)
			{
				if (m_tiles[player_location.y-1][i]->GetTileType() != TILETYPE_ICE)
				{
					if (m_tiles[player_location.y-1][i]->GetTileType() == TILETYPE_ROCK)
						m_player->SetMoveToLocation(player_location + CellLoc(j, 0));
					else
						m_player->SetMoveToLocation(player_location + CellLoc(++j, 0));
					break;
				}
			}
		}
		else if (m_tiles[player_location.y-1][player_location.x]->GetTileType() != TILETYPE_ROCK)
			m_player->SetMoveToLocation(player_location + CellLoc(1, 0));
	}
}

void GameWorld::CheckWin()
{
	CellLoc player_location = m_player->GetLocation();

	//victory or defeat conditions
	if (m_tiles[player_location.y-1][player_location.x-1]->GetTileType() == TILETYPE_PIT && 
		m_player->GetPlayerState() == PLAYER_STATE_IDLE) // we want to make sure he isnt moving first
	{
		GameState_GamePlay* gstate = (GameState_GamePlay*) GameStateManager::GetInstance()->GetCurrentGameState();
		gstate->GameOver(false);
	}
	else if (m_tiles[player_location.y-1][player_location.x-1]->GetTileType() == TILETYPE_EXIT &&
			 m_player->GetPlayerState() == PLAYER_STATE_IDLE)
	{
		GameState_GamePlay* gstate = (GameState_GamePlay*) GameStateManager::GetInstance()->GetCurrentGameState();
		gstate->GameOver(true);
	}
}

GameCamera* GameWorld::GetCamera()
{
	return m_camera;
}

void GameWorld::DoCameraAdjustments()
{
	using namespace ddengine_RenderEngine;

	float centerx = 0, centerz = 0;
	float ypos = 0;
	Vector3 campos;
	Vector3 camdir(0, -1, 0);
	Vector3 camup(0,0,-1);

	if (m_length <= 5 && m_breadth <= 5)
	{
		ypos = 22.5;
		centerx = (float)m_length * 0.5 * 6;
		centerz = (float)m_breadth * 0.5 * 6 - 4.5;

		campos = Vector3(centerx, ypos, centerz);
	}
	else
	{
		//determine center
		centerx = (float)m_length * 0.5 * 6 + 4.5;
		centerz = (float)m_breadth * 0.5 * 6 - 4.5;

		float zoomfactor = m_length > m_breadth ? m_length : m_breadth;
		ypos = 6 * zoomfactor * 0.75;
		campos = Vector3(centerx, ypos, centerz);
	}

	m_camera->SetPosition(campos);
	m_camera->SetDirection(camdir);
	m_camera->SetUp(camup);
	m_camera->FinalizeCamera();
}