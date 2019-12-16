
#ifndef _GAMESTATEMANAGER_H
#define _GAMESTATEMANAGER_H

#include <string>

enum GAMESTATE
{
	GAMESTATE_NULL = 0,
	GAMESTATE_MAINMENU,
	GAMESTATE_SINGLEPLAYERMENU,
	GAMESTATE_GAMEPLAY,
	//GAMESTATE_LEVELEDITOR,
	//GAMESTATE_DLREPOSITORY,
	GAMESTATE_MAX
};

class GameState;
class GameState_GamePlay;
class GameWorld;

class GameStateManager
{
public:
	virtual	~GameStateManager();
	static	GameStateManager* GetInstance();

	void	Init();
	void	ChangeGameState(GAMESTATE new_GameState_ID);
	void	Shutdown();

	void	Loop(float timeElapsed);

	void			SetCurrentMap(std::string mapname)	{ m_mapname = mapname; }
	std::string&	GetCurrentMap()						{ return m_mapname; }

protected:
	friend class GameState_GamePlay;
	friend class GameWorld;
	GameState*		GetCurrentGameState()		{	return m_current_GameState; }
private:
	GAMESTATE	m_current_GameState_ID;
	GAMESTATE	m_new_GameState_ID;
	GameState*	m_current_GameState;

	std::string m_mapname;

	GameStateManager();
};



#endif