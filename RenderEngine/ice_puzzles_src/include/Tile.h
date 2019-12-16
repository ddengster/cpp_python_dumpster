
#ifndef _TILE_H
#define _TILE_H

#include "GameWorld.h"

enum TILETYPE
{
	TILETYPE_NULL = 0,
	TILETYPE_ROCK,
	TILETYPE_ICE,
	TILETYPE_ROCKYPATH,
	TILETYPE_CRACKEDICE,
	TILETYPE_PIT,
	TILETYPE_EXIT,
	TILETYPE_MAX
};

class Tile
{
public:
	Tile(TILETYPE tile_ID, CellLoc location);
	Tile();
	virtual ~Tile();

	void		Render();

	void		SetTileType(TILETYPE tile_ID)	{	m_TileType_ID = tile_ID;	}
	TILETYPE	GetTileType()					{	return m_TileType_ID;		}

	void		SetTileLocation(CellLoc loc);
	CellLoc		GetTileLocation();

	static void		SetUpRenderingObjects();

private:
	TILETYPE	m_TileType_ID;
	CellLoc		m_location;
};

#endif
