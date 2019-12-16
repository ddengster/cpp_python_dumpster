
#ifndef _GAMEWORLD_H
#define _GAMEWORLD_H

#include <string>

class Tile;
class Player;
class GameCamera;

struct CellLoc
{
	int x, y;

	CellLoc()
		:x(0), y(0)
	{
	}

	CellLoc(int _x, int _y)
		:x(_x), y(_y)
	{
	}

	CellLoc& operator+(CellLoc loc)
	{
		x += loc.x;
		y += loc.y;
		return *this;
	}
	CellLoc& operator-(CellLoc loc)
	{
		x -= loc.x;
		y -= loc.y;
		return *this;
	}
};

enum PLAYER_DIR
{
	PLAYER_UP = 0,
	PLAYER_DOWN,
	PLAYER_LEFT,
	PLAYER_RIGHT,
	PLAYER_MAX
};

class GameWorld
{
public:
	GameWorld();
	virtual ~GameWorld();

	bool	InitializeFromFile(std::string filename, std::string *errormsg);

	bool	SetPlayerStartLocation(CellLoc location);
	bool	AddTreasureChest(CellLoc location);
	bool	SetWinCondition(unsigned int treasureChestCount);
	
	void	Update(float timeElapsed);
	bool	Clear();	//reset tiles
	void	CheckWin();

	Player*	GetPlayer()		{	return m_player;	}
	int		GetBreadth()	{	return m_breadth;	}
	int		GetLength()		{	return m_length;	}

	void	SetPlayerMoveDirection(PLAYER_DIR dir);

	GameCamera*		GetCamera();
	void			DoCameraAdjustments();

private:
	bool	SetSize(unsigned int length, unsigned int breadth);
	void	Render();

private:
	int	m_length, m_breadth;
	Tile***			m_tiles;

	Player*			m_player;
	
	GameCamera *m_camera;
};

#endif
