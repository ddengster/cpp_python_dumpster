
#ifndef TRICK9_H
#define TRICK9_H

/***********
Interface macro
***********/

#define INTERFACE_CREATURE(terminal)                \
	public:                                         \
		virtual void Attack() const ## terminal     \
		virtual int GetHP() const ## terminal       
#define BASE_CREATURE INTERFACE_CREATURE(=0;)
#define DERIVED_CREATURE INTERFACE_CREATURE(;)

class ICreature
{
	BASE_CREATURE;
};

class Creature_2
{
	DERIVED_CREATURE;
};

class Create_3
{
	DERIVED_CREATURE;
};

#endif
